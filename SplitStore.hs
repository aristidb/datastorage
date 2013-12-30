{-# LANGUAGE FlexibleContexts, ViewPatterns, BangPatterns #-}

import BlobStore
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec.ByteString as A
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

data ObjectTag = PlainData | Chain ObjectTag
    deriving (Eq, Show)

data TagMatch = DirectMatch | IndirectMatch | ArtificialMatch
    deriving (Show)

matchTag :: ObjectTag -> ObjectTag -> TagMatch
matchTag PlainData PlainData = DirectMatch
matchTag PlainData (Chain _) = ArtificialMatch
matchTag (Chain x) (Chain y) = matchTag x y
matchTag (Chain _) PlainData = IndirectMatch

tagBuilder :: ObjectTag -> Builder.Builder
tagBuilder PlainData = Builder.string7 "plain:"
tagBuilder (Chain t) = Builder.string7 "chain:" <> tagBuilder t

tagParser :: A.Parser ObjectTag
tagParser = (PlainData <$ A.string (B8.pack "plain:"))
            <|> (Chain <$> (A.string (B8.pack "chain:") *> tagParser))

data TaggedObject = TaggedObject ObjectTag B.ByteString
    deriving (Show)

taggedBuilder :: TaggedObject -> Builder.Builder
taggedBuilder (TaggedObject tag dat) = tagBuilder tag <> Builder.byteString dat

taggedParser :: A.Parser TaggedObject
taggedParser = TaggedObject <$> tagParser <*> A.takeByteString

taggedStore :: (Functor f, MonadCatch f) => Store' f Address B.ByteString -> Store' f Address TaggedObject
taggedStore = parserStore taggedBuilder taggedParser

readAddresses :: (MonadCatch f) => Producer B.ByteString f () -> Producer Address f ()
readAddresses p = do r <- PA.parseMany addressParse p >-> P.map snd
                     case r of
                       Left (pe, _) -> lift $ throwM (ParseError $ PA.peMessage pe)
                       Right () -> return ()


data SplitStore f = SplitStore {
      addressStore :: ObjectTag -> Store' f Address (Producer Address f ())
    , producerStore :: ObjectTag -> Store' f Address (Producer B.ByteString f ())
    }

splitStore :: (Functor f, MonadCatch f) => (Producer B.ByteString f () -> Producer B.ByteString f ()) -> Store' f Address TaggedObject -> SplitStore f
splitStore splitter tst = SplitStore { addressStore = ast, producerStore = pst }
    where ast t = Store { store = doStore, load = doLoad }
            where
              doStore p = store (pst $ Chain t) (for p writeAddress)
              doLoad a = readAddresses <$> load (pst $ Chain t) a

          pst t = Store { store = doStore, load = doLoad }
            where
              doStore (splitter -> p) =
                do r <- next p
                   let (v, p') = case r of
                        Left () -> (B.empty, return ())
                        Right (vx, px) -> (vx, px)
                   s <- next p'
                   case s of
                        Left () -> store tst (TaggedObject t v)
                        Right (vx, px) -> let as = (yield v >> yield vx >> px) >-> P.mapM (store tst . TaggedObject t)
                                          in store (ast t) as

              doLoad a = do TaggedObject t' o <- load tst a
                            return $ case matchTag t' t of
                                       ArtificialMatch -> writeAddress a
                                       DirectMatch -> yield o
                                       IndirectMatch -> for (readAddresses (yield o)) $ join . lift . doLoad

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
