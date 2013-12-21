{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor, RankNTypes, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module BlobStore where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Base64.URL as Base64U
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Hashable
import Data.Byteable
import qualified Data.Cache.LRU as LRU
import Control.Exception
import Control.Applicative ((<$>), (<$))
import Data.Monoid
import Control.Lens

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

instance Addressable Address B.ByteString where
    address o = SHA512Key $ SHA512.hash o

data Decorated a = Decorated a B.ByteString
    deriving (Eq, Show)

instance Addressable a (Decorated a) where
    address (Decorated a _) = a

instance Byteable (Decorated a) where
    toBytes (Decorated _ o) = o

type Put a x = (Addressable a x, Byteable x)

decorate :: (Addressable a o, Byteable o) => o -> Decorated a
decorate o = Decorated (address o) (toBytes o)

class Get a x where
    undecorate :: Decorated a -> Either String x
    undecorate (Decorated a x) = unroll a x

    unroll :: a -> B.ByteString -> Either String x
    unroll a x = undecorate (Decorated a x)

instance Get a (Decorated a) where
    undecorate = Right

instance Get a B.ByteString where
    unroll _ x = Right x

data StorageQuality = Cached | Permanent
    deriving (Show)

data StorageLevel e a = NoValidObject e | Stored StorageQuality a
    deriving (Functor, Show)

joinStorageLevel :: StorageLevel e (Either e a) -> StorageLevel e a
joinStorageLevel (NoValidObject e) = NoValidObject e
joinStorageLevel (Stored _ (Left e)) = NoValidObject e
joinStorageLevel (Stored q (Right x)) = Stored q x

checkStorageLevel :: (a -> Maybe e) -> StorageLevel e a -> StorageLevel e a
checkStorageLevel _ s@(NoValidObject _) = s
checkStorageLevel f s@(Stored _ x) = maybe s NoValidObject (f x)

stored :: Get a x => StorageQuality -> a -> B.ByteString -> StorageLevel String x
stored q a = joinStorageLevel . Stored q . unroll a

undecorateStored :: Get a x => StorageLevel String (Decorated a) -> StorageLevel String x
undecorateStored = joinStorageLevel . fmap undecorate

data Store f a i o = Store
    { store :: i -> f (StorageLevel String a)
    , load :: a -> f (StorageLevel String o)
    }

objectStore :: (Functor f, Monad f, Put a i, Get a o) => Store f a (Decorated a) (Decorated a) -> Store f a i o
objectStore raw = Store { store = doStore, load = doLoad }
    where doStore x = store raw (decorate x)
          doLoad a = joinStorageLevel . fmap undecorate <$> load raw a

-- TODO: use non-simple Prism?
prismStore :: (Functor f, Monad f) => Prism' s x -> Store f a s s -> Store f a x x
prismStore p st = Store { store = doStore, load = doLoad }
    where doStore o = store st (review p o)
          doLoad a = joinStorageLevel . fmap (maybe (Left "Excluded type") Right . preview p) <$> load st a

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

memoryStore :: (Eq a, Hashable a, Put a i, Get a o) => IORef (HM.HashMap a B.ByteString) -> Store IO a i o
memoryStore mapRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, Stored Permanent a))
          doLoad a = maybe (NoValidObject "Unknown address") (stored Permanent a) . HM.lookup a <$> readIORef mapRef

newMemoryStore :: (Eq a, Hashable a, Put a i, Get a o) => IO (Store IO a i o)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: (Ord a, Put a i, Get a o) => IORef (LRU.LRU a B.ByteString) -> Store IO a i o
lruCache cacheRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, Stored Cached a))
          doLoad a = maybe (NoValidObject "Not in cache") (stored Cached a) <$> atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: (Ord a, Put a i, Get a o) => Maybe Integer -> IO (Store IO a i o)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: (Byteable a, Put a i, Get a o) => FilePath -> Store IO a i o
fsStore dir = Store { store = doStore, load = doLoad }
    where
        addrPath k = dir ++ "/O_" ++ B8.unpack (Base64U.encode $ toBytes k)
        doStore (decorate -> Decorated a o) = Stored Permanent a <$ B.writeFile (addrPath a) o
        doLoad a = do m <- try $ B.readFile (addrPath a)
                      return $ case m of
                        Left (e :: IOException) -> (NoValidObject $ show e)
                        Right x -> stored Permanent a x

verify :: (Functor f, Monad f, Put Address i, Get Address o) => Store f Address (Decorated Address) (Decorated Address) -> Store f Address i o
verify st = Store { store = doStore, load = doLoad }
    where doStore (decorate -> x@(Decorated a _)) = check (== a) "Non-matching return address" <$> store st x
          doLoad a = undecorateStored . check (\(Decorated a' o) -> a == a' && checkAddress a o) "Non-matching SHA-512" <$> load st a
          check f e = checkStorageLevel (\x -> if f x then Nothing else Just e)
          checkAddress (SHA512Key k) o = k == SHA512.hash o
