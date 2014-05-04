{-# LANGUAGE ConstraintKinds, KindSignatures, DataKinds, ScopedTypeVariables, DeriveFunctor, RankNTypes, ViewPatterns, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, DeriveDataTypeable, DeriveGeneric, TupleSections #-}

module BlobStore where

import TypedBinary
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.HashMap.Strict as HM
import Crypto.Hash
import Data.IORef
import qualified Data.Hashable as H
-- import qualified Data.Cache.LRU as LRU
import Control.Applicative
import Control.Monad.Catch
import Data.Typeable
import GHC.Generics hiding (from, to)
import qualified Data.Binary.Get as Bin
import Control.DeepSeq

data Address = SHA512Key (Digest SHA512)
    deriving (Eq, Ord, Show, Generic)

instance Grammatical Address

class ToFileName a where
    toFileName :: a -> FilePath

instance ToFileName Address where
    toFileName (SHA512Key k) = show k

{-
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
-}

data Store f a o = Store
    { store :: o -> f a
    , load :: a -> f o
    }

data GrammarException = GrammarException String
    deriving (Show, Typeable)

instance Exception GrammarException

grammarStore :: (Functor f, Monad f, MonadCatch f) => Grammar o -> Store f a L.ByteString -> Store f a o
grammarStore g st = Store { store = doStore, load = doLoad }
    where doStore x = case writeDefault g x of
                        Left s -> throwM (GrammarException s)
                        Right o -> store st $ Builder.toLazyByteString o
          doLoad a = do o <- load st a
                        case Bin.runGetOrFail (parseFull g) o of
                          Left (_, _, s) -> throwM (GrammarException s)
                          Right (_, _, x) -> return x

data UnknownAddress = UnknownAddress
  deriving (Show, Typeable)

instance Exception UnknownAddress

memoryStore :: (Eq a, H.Hashable a) => IORef (HM.HashMap a o) -> Store IO a (a, o)
memoryStore mapRef = Store { store = doStore, load = doLoad }
    where doStore (a, o) = atomicModifyIORef' mapRef (\m -> (HM.insert a o m, a))
          doLoad a = maybe (throwM UnknownAddress) (return . (a, )) . HM.lookup a =<< readIORef mapRef

newMemoryStore :: (Eq a, H.Hashable a) => IO (Store IO a (a, o))
newMemoryStore = memoryStore <$> newIORef HM.empty

{-
lruCache :: (Ord a, Put a o, Get IO a o) => IORef (LRU.LRU a B.ByteString) -> Store IO a o
lruCache cacheRef = Store { store = doStore, load = doLoad }
    where doStore (decorate -> Decorated a o) = atomicModifyIORef' cacheRef (\m -> (LRU.insert a o m, a))
          doLoad a = maybe (throwM UnknownAddress) (unroll a) =<< atomicModifyIORef' cacheRef (LRU.lookup a)

newLRUCache :: (Ord a, Put a o, Get IO a o) => Maybe Integer -> IO (Store IO a o)
newLRUCache len = lruCache <$> newIORef (LRU.newLRU len)
-}

fsStore :: (ToFileName a) => FilePath -> Store IO a (a, L.ByteString)
fsStore dir = Store { store = doStore, load = doLoad }
    where
        addrPath k = dir ++ "/" ++ toFileName k
        doStore (a, o) = putStrLn ("Write " ++ addrPath a ++ " : " ++ show o) >> (a <$ L.writeFile (addrPath a) o)
        doLoad a = do o <- L.readFile (addrPath a); putStrLn $ "Read " ++ addrPath a ++ " : " ++ show o; o `deepseq` return (a, o)

hashStore :: (Functor f) => Store f Address (Address, L.ByteString) -> Store f Address L.ByteString
hashStore st = Store { store = doStore, load = doLoad }
    where
      doStore o = store st (SHA512Key (hashlazy o), o)
      doLoad a = snd <$> load st a

{-
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
-}

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
