{-# LANGUAGE FlexibleContexts, ViewPatterns #-}

import BlobStore
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec.ByteString as A
-- import qualified Data.Attoparsec.ByteString.Lazy as AL
import Pipes
import qualified Pipes.Prelude as P
-- import qualified Pipes.ByteString as PB
import Control.Applicative
import Data.Monoid
import Data.Foldable (foldMap)
import Control.Monad.Catch
import Control.Monad

data Representation =
    Embedded B.ByteString |
    ObjectList [Address]
  deriving (Show)

representationBuilder :: Representation -> Builder.Builder
representationBuilder (Embedded x) = Builder.word8 0 <> Builder.byteString x
representationBuilder (ObjectList as) = Builder.word8 1 <> foldMap addressBuilder as

representationParser :: A.Parser Representation
representationParser = Embedded <$> (A.word8 0 *> A.takeByteString) <|>
                       ObjectList <$> (A.word8 1 *> many addressParse)

streamStore :: (Functor f, MonadCatch f) => (Producer B.ByteString f () -> Producer B.ByteString f ()) -> Store f Address Representation Representation -> Store f Address (Producer B.ByteString f ()) (Producer B.ByteString f ())
streamStore splitter st = Store { store = doStore, load = \a -> makeProducer <$> load st a }
    where doStore (splitter -> p) = do xs <- P.toListM $ for p $ \o -> yield =<< lift (store st (Embedded o))
                                       case xs of
                                         [x] -> return x
                                         _ -> store st (ObjectList xs)

          makeProducer (Embedded o) = yield o
          makeProducer (ObjectList xs) = mapM_ (makeProducer <=< lift . load st) xs
