{-# LANGUAGE FlexibleContexts, ViewPatterns, BangPatterns, GeneralizedNewtypeDeriving #-}

import BlobStore
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.ByteString.Lazy.Builder.ASCII as Builder
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Char8 as A8
-- import qualified Data.Attoparsec.ByteString.Lazy as AL
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Attoparsec as PA
import Control.Applicative
import Data.Monoid
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Free
import Data.Int
import Control.Exception

newtype ObjectDepth = ObjectDepth { depth :: Int32 }
  deriving (Eq, Ord, Num, Show)

depthBuilder :: ObjectDepth -> Builder.Builder
depthBuilder (ObjectDepth i) = Builder.string7 "depth=" <> Builder.int32Dec i <> Builder.string7 ":"

depthParser :: A.Parser ObjectDepth
depthParser = ObjectDepth <$> (A.string (B8.pack "depth=") *> A8.decimal <* A.string (B8.pack ":"))

{-
data ObjectDepth = PlainData | Chain ObjectDepth
    deriving (Eq, Show)

data TagMatch = DirectMatch | IndirectMatch | ArtificialMatch
    deriving (Show)

matchTag :: ObjectDepth -> ObjectDepth -> TagMatch
matchTag PlainData PlainData = DirectMatch
matchTag PlainData (Chain _) = ArtificialMatch
matchTag (Chain x) (Chain y) = matchTag x y
matchTag (Chain _) PlainData = IndirectMatch

depthBuilder :: ObjectDepth -> Builder.Builder
depthBuilder PlainData = Builder.string7 "plain:"
depthBuilder (Chain t) = Builder.string7 "chain:" <> depthBuilder t

depthParser :: A.Parser ObjectDepth
depthParser = (PlainData <$ A.string (B8.pack "plain:"))
            <|> (Chain <$> (A.string (B8.pack "chain:") *> depthParser))
-}

data DepthObject = DepthObject ObjectDepth B.ByteString
    deriving (Show)

depthObjectBuilder :: DepthObject -> Builder.Builder
depthObjectBuilder (DepthObject tag dat) = depthBuilder tag <> Builder.byteString dat

depthObjectParser :: A.Parser DepthObject
depthObjectParser = DepthObject <$> depthParser <*> A.takeByteString

depthObjectStore :: (Functor f, MonadCatch f) => Store' f Address B.ByteString -> Store' f Address DepthObject
depthObjectStore = parserStore depthObjectBuilder depthObjectParser

readAddresses :: (MonadCatch f) => Producer B.ByteString f () -> Producer Address f ()
readAddresses p = do r <- PA.parseMany addressParse p >-> P.map snd
                     case r of
                       Left (pe, _) -> lift $ throwM (ParseError $ PA.peMessage pe)
                       Right () -> return ()


data SplitStore f = SplitStore {
      addressStore :: ObjectDepth -> Store' f Address (Producer Address f ())
    , producerStore :: ObjectDepth -> Store' f Address (Producer B.ByteString f ())
    }

splitStore :: (Functor f, MonadCatch f) => (Producer B.ByteString f () -> Producer B.ByteString f ()) -> Store' f Address DepthObject -> SplitStore f
splitStore splitter tst = SplitStore { addressStore = ast, producerStore = pst }
    where ast t = Store { store = doStore, load = doLoad }
            where
              doStore p = store (pst $ t + 1) (for p writeAddress)
              doLoad a = readAddresses <$> load (pst $ t + 1) a

          unwrap n n' p | n >= n' = p
                        | otherwise = unwrap n (n' - 1) $ for (readAddresses p) $ join . lift . load (pst $ n' - 1)

          pst t = assert (t < 5) $ Store { store = doStore, load = doLoad }
            where
              doStore (splitter -> p) =
                do r <- next p
                   let (v, p') = case r of
                        Left () -> (B.empty, return ())
                        Right (vx, px) -> (vx, px)
                   s <- next p'
                   case s of
                        Left () -> store tst (DepthObject t v)
                        Right (vx, px) -> let as = (yield v >> yield vx >> px) >-> P.mapM (store tst . DepthObject t)
                                          in store (ast t) as

              doLoad a = do DepthObject t' o <- load tst a
                            let p = case () of
                                       _ | t > t' -> writeAddress a
                                         | otherwise -> yield o
                            return $ unwrap t t' p

mkSplitter :: Monad m => (Producer B.ByteString m () -> PB.FreeT (Producer B.ByteString m) m ()) -> (Producer B.ByteString m () -> Producer B.ByteString m ())
mkSplitter f p = go (f p)
    where go x = do r <- lift $ runFreeT x
                    case r of
                      Pure () -> return ()
                      Free p' -> do (v, x') <- lift $ consume p' L.empty
                                    yield v
                                    go x'
          consume px !acc = do r <- next px
                               case r of
                                 Left s -> return (L.toStrict acc, s)
                                 Right (v, p') -> consume p' (acc <> L.fromStrict v)
