{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor, RankNTypes, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, DeriveGeneric #-}

module BlobStore where

import TypedBinary
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
import Control.Applicative
import Data.Monoid
import Control.Lens
import Control.Monad.Catch
import Data.Typeable
import Control.Monad
import Pipes
import qualified Pipes.ByteString as PB
import GHC.Generics hiding (from, to)
import qualified Data.Binary.Get as Bin

data Address = SHA512Key B.ByteString
    deriving (Eq, Ord, Show, Generic)

instance Byteable Address where
    toBytes (SHA512Key x) = x

instance Grammatical Address where
    grammar = gGrammar { defaultType = TVariant [(L "SHA512Key", TVector (Just 64) (TUInt (Just 1)))] }

addressBuilder :: Address -> Builder.Builder
addressBuilder (SHA512Key key) = Builder.string7 "sha512:" <> Builder.byteString (toBytes key)

writeAddress :: Monad f => Address -> Producer' B.ByteString f ()
writeAddress = PB.fromLazy . Builder.toLazyByteString . addressBuilder

addressParse :: A.Parser Address
addressParse = SHA512Key <$> (A.string (B8.pack "sha512:") *> A.take 64)

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

data Store f a o = Store
    { store :: o -> f a
    , load :: a -> f o
    }

objectStore :: (Functor f, Monad f, Put a o, Get f a o) => Store f a (Decorated a) -> Store f a o
objectStore raw = Store { store = doStore, load = doLoad }
    where doStore x = store raw (decorate x)
          doLoad a = undecorate =<< load raw a

-- TODO: do not use "fail"
grammarStore :: (Functor f, Monad f) => Grammar o -> Store f a B.ByteString -> Store f a o
grammarStore g st = Store { store = doStore, load = doLoad }
    where doStore x = case writeDefault g x of
                        Left s -> fail s
                        Right o -> store st $ L.toStrict $ Builder.toLazyByteString o
          doLoad a = do o <- load st a
                        case Bin.runGetOrFail (parseFull g) (L.fromStrict o) of
                          Left (_, _, s) -> fail s
                          Right (_, _, x) -> return x

-- TODO: use non-simple Prism?
prismStore :: (Functor f, Monad f) => (forall i. f i) -> Prism' s x -> Store f a s -> Store f a x
prismStore err p st = Store { store = doStore, load = doLoad }
    where doStore o = store st (review p o)
          doLoad a = do m <- preview p <$> load st a
                        case m of
                          Nothing -> err
                          Just x -> return x

data UnknownAddress = UnknownAddress
  deriving (Show, Typeable)

instance Exception UnknownAddress

memoryStore :: (Eq a, Hashable a, Put a o, Get IO a o) => IORef (HM.HashMap a B.ByteString) -> Store IO a o
memoryStore mapRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, a))
          doLoad a = maybe (throwM UnknownAddress) (unroll a) . HM.lookup a =<< readIORef mapRef

newMemoryStore :: (Eq a, Hashable a, Put a o, Get IO a o) => IO (Store IO a o)
newMemoryStore = memoryStore <$> newIORef HM.empty

lruCache :: (Ord a, Put a o, Get IO a o) => IORef (LRU.LRU a B.ByteString) -> Store IO a o
lruCache cacheRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, a))
          doLoad a = maybe (throwM UnknownAddress) (unroll a) =<< atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: (Ord a, Put a o, Get IO a o) => Maybe Integer -> IO (Store IO a o)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)

fsStore :: (Byteable a, Put a o, Get IO a o) => FilePath -> Store IO a o
fsStore dir = Store { store = doStore, load = doLoad }
    where
        addrPath k = dir ++ "/O_" ++ B8.unpack (Base64U.encode $ toBytes k)
        doStore (decorate -> Decorated a o) = putStrLn ("Write " ++ show (toBytes a) ++ " : " ++ show o) >> (a <$ B.writeFile (addrPath a) o)
        doLoad a = unroll a =<< do o <- B.readFile (addrPath a); putStrLn $ "Read " ++ show (toBytes a) ++ " : " ++ show o; return o

data InvalidObject = InvalidObject
  deriving (Show, Typeable)

instance Exception InvalidObject

verify :: (Functor f, MonadCatch f, Put a o, Get f a o, Eq a, Addressable a o) => Store f a o -> Store f a o
verify st = Store { store = doStore, load = doLoad }
    where doStore x = do a <- store st x
                         when (a /= address x) $ throwM InvalidObject
                         return a
          doLoad a = do o <- load st a
                        when (a /= address o) $ throwM InvalidObject
                        return o

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
