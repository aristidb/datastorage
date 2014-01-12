{-# LANGUAGE RankNTypes, KindSignatures, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
module List where

import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import qualified Pipes

data List f m r = Cons (f m (List f m r)) | Impure (m (List f m r)) | Nil r

instance (Functor (f m), Monad m) => Monad (List f m) where
    return = Nil
    Nil r >>= k = k r
    Cons f >>= k = Cons (fmap (>>= k) f)
    Impure m >>= k = Impure (liftM (>>= k) m)

liftL :: (Monad m, Functor (f m)) => m a -> List f m a
liftL m = Impure $ liftM return m

yieldW :: Monad m => a -> List (WriterT a) m ()
yieldW x = Cons (WriterT (return (Nil (), x)))

pureListW :: List (WriterT a) Identity () -> [a]
pureListW (Nil ()) = []
pureListW (Impure m) = pureListW (runIdentity m)
pureListW (Cons (WriterT (Identity (l', a)))) = a : pureListW l'

toListM :: Monad m => List (WriterT a) m () -> m [a]
toListM (Nil ()) = return []
toListM (Impure m) = m >>= toListM
toListM (Cons w) = do (l, x) <- runWriterT w
                      liftM (x:) (toListM l)

fromProducerW :: (Functor m, Monad m) => Pipes.Producer a m r -> List (WriterT a) m r
fromProducerW p =
    do x <- liftL $ Pipes.next p
       case x of
         Left r -> return r
         Right (a, p') -> yieldW a >> fromProducerW p'

toProducerW :: (Functor m, Monad m) => List (WriterT a) m r -> Pipes.Producer' a m r
toProducerW (Nil r) = return r
toProducerW (Impure m) = Pipes.lift m >>= toProducerW
toProducerW (Cons w) = do (l, x) <- Pipes.lift (runWriterT w)
                          Pipes.yield x
                          toProducerW l

listIt :: (Functor m, Monad m) => List (WriterT (Bool, a)) m r -> List (List (WriterT a)) m r
listIt (Nil r) = Nil r
listIt (Impure m) = Impure (liftM listIt m)
listIt l@(Cons _) = Cons (go l)
    where go (Nil r) = Nil (Nil r)
          go (Impure m) = Impure (liftM go m)
          go (Cons w) =
            do (l', (p, a)) <- liftL $ runWriterT w
               yieldW a
               if p
                 then Nil (listIt l')
                 else go l'

awaitR :: (Monad m, Functor m) => List (ReaderT r) m r
awaitR = Cons (fmap return ask)

feedR :: Monad m => [a] -> List (ReaderT a) m r -> m ([a], Maybe r)
feedR xs (Nil r) = return (xs, Just r)
feedR xs (Impure m) = m >>= feedR xs
feedR [] (Cons _) = return ([], Nothing)
feedR (x:xs) (Cons r) = runReaderT r x >>= feedR xs

toConsumer :: (Functor m, Monad m) => List (ReaderT a) m r -> Pipes.Consumer' a m r
toConsumer (Nil r) = return r
toConsumer (Impure m) = Pipes.lift m >>= toConsumer
toConsumer (Cons r) = do x <- Pipes.await
                         l <- Pipes.lift (runReaderT r x)
                         toConsumer l

fromConsumer :: (Functor m, Monad m) => Pipes.Consumer a m r -> List (ReaderT a) m r
fromConsumer = undefined -- TODO: figure this out

data PipeF a b (m :: * -> *) r = Request (a -> m r) | Response (m (b, r))

instance (Functor m) => Functor (PipeF a b m) where
    fmap f (Request r) = Request (fmap f . r)
    fmap f (Response w) = Response (fmap (fmap f) w)

yieldP :: (Functor m, Monad m) => b -> List (PipeF a b) m ()
yieldP a = Cons (Response (return (a, return ())))

awaitP :: (Functor m, Monad m) => List (PipeF a b) m a
awaitP = Cons (Request (return.return))

mapInner :: (Functor (f m), Monad m) => (forall a. f m a -> g m a) -> List f m r -> List g m r
mapInner _ (Nil r) = Nil r
mapInner f (Impure m) = Impure (liftM (mapInner f) m)
mapInner f (Cons x) = Cons (f (fmap (mapInner f) x))

singleton :: (Functor (f m)) => f m r -> List f m r
singleton = Cons . fmap Nil

newtype Compose f g (m :: * -> *) a = Compose { getCompose :: f m (g m a) }

instance (Functor (f m), Functor (g m)) => Functor (Compose f g m) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

class Yield f a | f -> a where
    yield :: (Functor m, Monad m) => a -> List f m ()

instance Yield (WriterT a) a where
    yield = yieldW

instance Yield (PipeF a b) b where
    yield = yieldP

instance (Yield f a) => Yield (Compose f g) a where
    -- yield = Compose . yield

class Await f a | f -> a where
    await :: (Functor m, Monad m) => List f m a

instance Await (ReaderT a) a where
    await = awaitR

instance Await (PipeF a b) a where
    await = awaitP
