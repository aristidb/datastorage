{-# LANGUAGE DataKinds, KindSignatures, GADTs, RankNTypes #-}
module HighLevel where

import qualified BlobStore
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Pipes
import Prelude hiding (read, splitAt)
import Data.Monoid
import Data.Foldable (foldMap)
import Control.Applicative

data Representation =
    Embedded L.ByteString |
    ObjectList [BlobStore.Address]
  deriving (Show)

representationBuilder :: Representation -> Builder.Builder
representationBuilder (Embedded x) = Builder.word8 0 <> Builder.lazyByteString x
representationBuilder (ObjectList as) = Builder.word8 1 <> foldMap BlobStore.addressBuilder as

representationParser :: A.Parser Representation
representationParser = (Embedded <$> (A.word8 0 *> A.takeLazyByteString) <|>
                        ObjectList <$> (A.word8 1 *> many BlobStore.addressParse))
                       <* A.endOfInput

instance BlobStore.Object Representation where
    serialize = BlobStore.decorate . Builder.toLazyByteString . representationBuilder
    deserialize (BlobStore.Decorated _ x) = AL.eitherResult $ AL.parse representationParser x

data Object (m :: * -> *) a where
    FromAddress :: BlobStore.Address -> Object m a
    FromByteString :: L.ByteString -> Object m a

read :: Object m a -> Producer a m ()
read = undefined

write :: Producer a m () -> Object m a
write = undefined

slice :: Int -> Int -> Object m a
slice = undefined

splitAt :: Int -> Object m a -> (Object m a, Object m a)
splitAt = undefined

append :: Object m a -> Object m a -> Object m a
append = undefined
