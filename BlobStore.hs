{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor, RankNTypes, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveDataTypeable #-}

module BlobStore where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Base64.URL as Base64U
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable
import qualified Data.Cache.LRU as LRU
import Control.Applicative ((<$>), (<$))
import Data.Monoid
import Control.Lens
import Control.Monad.Catch
import Control.Exception hiding (try)
import Data.Typeable
import Control.Monad

data Address = SHA512Key B.ByteString
    deriving (Eq, Ord, Show)

instance Byteable Address where
    toBytes (SHA512Key x) = x

addressBuilder :: Address -> Builder.Builder
addressBuilder (SHA512Key key) = Builder.word8 1 <> Builder.byteString (toBytes key)

addressParse :: A.Parser Address
addressParse = SHA512Key <$> (A.word8 1 >> A.take 64)

instance Hashable Address where
    hashWithSalt salt (SHA512Key k) = hashWithSalt salt (toBytes k)

class Addressable a o where
    address :: o -> a
    verifyAddress :: o -> a -> Bool

instance Addressable Address B.ByteString where
    address o = SHA512Key $ SHA512.hash o
    verifyAddress o (SHA512Key k) = SHA512.hash o == k

data Decorated a = Decorated a B.ByteString
    deriving (Eq, Show)

instance Eq a => Addressable a (Decorated a) where
    address (Decorated a _) = a
    verifyAddress (Decorated a1 _) a2 = a1 == a2

instance Byteable (Decorated a) where
    toBytes (Decorated _ o) = o

type Put a x = (Addressable a x, Byteable x)

decorate :: (Addressable a o, Byteable o) => o -> Decorated a
decorate o = Decorated (address o) (toBytes o)

class Get f a x where
    undecorate :: Decorated a -> f x
    undecorate (Decorated a x) = unroll a x

    unroll :: a -> B.ByteString -> f x
    unroll a x = undecorate (Decorated a x)

instance Monad m => Get m a (Decorated a) where
    undecorate = return

instance Monad m => Get m a B.ByteString where
    unroll _ = return

data Store f a i o = Store
    { store :: i -> f a
    , load :: a -> f o
    }

objectStore :: (Functor f, Monad f, Put a i, Get f a o) => Store f a (Decorated a) (Decorated a) -> Store f a i o
objectStore raw = Store { store = doStore, load = doLoad }
    where doStore x = store raw (decorate x)
          doLoad a = undecorate =<< load raw a

-- TODO: use non-simple Prism?
prismStore :: (Functor f, Monad f) => (forall i. f i) -> Prism' s x -> Store f a s s -> Store f a x x
prismStore err p st = Store { store = doStore, load = doLoad }
    where doStore o = store st (review p o)
          doLoad a = do m <- preview p <$> load st a
                        case m of
                          Nothing -> err
                          Just x -> return x

data UnknownAddress = UnknownAddress
  deriving (Show, Typeable)

instance Exception UnknownAddress

memoryStore :: (Eq a, Hashable a, Put a i, Get IO a o) => IORef (HM.HashMap a B.ByteString) -> Store IO a i o
memoryStore mapRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, a))
          doLoad a = maybe (throwM UnknownAddress) (unroll a) . HM.lookup a =<< readIORef mapRef

newMemoryStore :: (Eq a, Hashable a, Put a i, Get IO a o) => IO (Store IO a i o)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: (Ord a, Put a i, Get IO a o) => IORef (LRU.LRU a B.ByteString) -> Store IO a i o
lruCache cacheRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, a))
          doLoad a = maybe (throwM UnknownAddress) (unroll a) =<< atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: (Ord a, Put a i, Get IO a o) => Maybe Integer -> IO (Store IO a i o)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: (Byteable a, Put a i, Get IO a o) => FilePath -> Store IO a i o
fsStore dir = Store { store = doStore, load = doLoad }
    where
        addrPath k = dir ++ "/O_" ++ B8.unpack (Base64U.encode $ toBytes k)
        doStore (decorate -> Decorated a o) = a <$ B.writeFile (addrPath a) o
        doLoad a = unroll a =<< B.readFile (addrPath a)

data InvalidObject = InvalidObject
  deriving (Show, Typeable)

instance Exception InvalidObject

verify :: (Functor f, MonadCatch f, Put a i, Get f a o, Eq a, Addressable a o) => Store f a i o -> Store f a i o
verify st = Store { store = doStore, load = doLoad }
    where doStore x = do a <- store st x
                         when (a /= address x) $ throwM InvalidObject
                         return a
          doLoad a = do o <- load st a
                        when (a /= address o) $ throwM InvalidObject
                        return o

newtype ParseError = ParseError String
  deriving (Show, Typeable)

instance Exception ParseError

parserStore :: (Functor f, MonadCatch f, Put a B.ByteString, Get f a B.ByteString) => (i -> Builder.Builder) -> A.Parser o -> Store f a B.ByteString B.ByteString -> Store f a i o
parserStore render parser st = Store { store = doStore, load = doLoad }
    where doStore x = store st (L.toStrict . Builder.toLazyByteString . render $ x)
          doLoad a = do e <- A.parseOnly parser <$> load st a
                        case e of
                          Left s -> throwM (ParseError s)
                          Right x -> return x

{-
-- possible implementation of <|>
orM :: Monad f => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
orM m n = do x <- m
             case x of
               Just _ -> return x
               Nothing -> n

type Rel a = a -> a -> a

duplicated :: Monad f => Rel (f ()) -> Rel (f (Maybe Decorated)) ->
                         RawStore p1 f -> RawStore p2 f -> RawStore p3 f
duplicated (<&>) (<|>) a b = RawStore { store = \o -> store a o <&> store b o
                                   , load = \i -> load a i <|> load b i }

duplicatedSerial :: Monad f => RawStore p1 f -> RawStore p2 f -> RawStore p2 f
duplicatedSerial = duplicated (>>) orM

cache :: Monad f => RawStore Cached f -> RawStore p f -> RawStore p f
cache = duplicatedSerial

multi :: Monad f => (Address -> f (RawStore p f)) -> RawStore p f
multi locate = RawStore { store = doStore, load = doLoad }
    where doStore o@(Decorated a _) = locate a >>= (`store` o)
          doLoad a = locate a >>= (`load` a)
-}
