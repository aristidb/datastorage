{-# LANGUAGE BangPatterns, RecordWildCards, FlexibleContexts, TemplateHaskell, RankNTypes, KindSignatures #-}
module RollingVector where

import qualified Data.Vector.Generic as G
import Data.Word
import Data.Bits
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans.Free
import Pipes
import Control.Lens

-- import Control.Exception (assert)

hashCombine :: Word64 -> Word64 -> Word64
hashCombine !x !y = x `rotateL` 1 `xor` y
{-# INLINE hashCombine #-}

data HashType a = HashType {
      hash :: a -> Word64
    , window :: Int
    , mask :: Word64
    }

addRemove :: Int -> Word64 -> Word64 -> Word64
addRemove !w !o !n = o `rotateL` w `xor` n
{-# INLINE addRemove #-}

prehash :: (G.Vector v a, G.Vector v Word64) => HashType a -> v a -> Word64 -> Word64
prehash HashType{..} v h = G.foldl' hashCombine h x
    where x = G.map hash v

nextBoundary :: (G.Vector v a, MonadState Word64 m) => HashType a -> v a -> v a -> m (Maybe Int)
nextBoundary HashType{..} old new =
    state $ \h0 -> runST (go h0 0)
  where
    inputSize = G.length new
    go !h !iIn | iIn < inputSize =
                        do case (h' .&. mask) == mask of
                             True -> return (Just $ iIn + 1, h)
                             False -> go h' (iIn + 1)
               | otherwise = return (Nothing, h)
      where hi = addRemove window (hash (old `G.unsafeIndex` iIn)) (hash (new `G.unsafeIndex` iIn))
            h' = hashCombine h hi
{-# INLINE nextBoundary #-}

-- drawBlock :: (G.Vector v a, Monad m) => HashType a -> v a -> v a -> 

contiguous :: (Monad m, G.Vector v a) => HashType a -> v a -> v a -> Producer' (Bool, v a) (StateT Word64 m) ()
contiguous ht = go
    where
      go old new =
        do x <- nextBoundary ht old new
           case x of
             Nothing -> yield (False, new)
             Just n -> do yield (True, a)
                          go old' new'
               where (a, new') = G.splitAt n new
                     old' = G.drop n new

data HashState a = HashState { _lastHash :: !Word64, _lastWindow :: !a }

makeLenses ''HashState

initial :: (Monad m, G.Vector v a, G.Vector v Word64) => HashType a -> v a -> Pipe (v a) (Bool, v a) (StateT (HashState (v a)) m) (v a)
initial ht@HashType{..} x =
    do w <- use lastWindow
       if G.length w < window
         then do let n = window - G.length w
                     (xi, xn) = G.splitAt n x
                     w' = w G.++ xi
                 lastWindow .= w'
                 lastHash %= prehash ht xi
                 yield (False, xi)
                 if G.length w' < window
                   then await >>= initial ht
                   else return xn
         else return x

warm :: (Monad m, G.Vector v a) => HashType a -> v a -> Pipe (v a) (Bool, v a) (StateT (HashState (v a)) m) ()
warm ht@HashType{..} x =
    do w <- use lastWindow
       w' <- hoist (zoom lastHash) $
         do contiguous ht w (G.take window x)
            let len = G.length x
            when (len > window) $ contiguous ht x (G.drop window x)
            return $ G.drop len w G.++ G.drop (len - window) x
       lastWindow .= w'
       await >>= warm ht

rollsplit :: (Monad m, G.Vector v a, G.Vector v Word64) => HashType a -> Pipe (v a) (Bool, v a) (StateT (HashState (v a)) m) ()
rollsplit ht = await >>= initial ht >>= warm ht

rollsplit' :: (Monad m, G.Vector v a, G.Vector v Word64) => HashType a -> Producer (v a) m () -> Producer (Bool, v a) (StateT (HashState (v a)) m) ()
rollsplit' ht p = hoist lift p >-> rollsplit ht

clamp :: (Monad m, G.Vector v a) => Int -> Int -> Producer (Bool, v a) m r -> Producer' (Bool, v a) m r
clamp nmin nmax = loop 0
    where yieldSplitted t n d | n + G.length d < nmax = when (not $ G.null d) $ yield (t, d)
                              | otherwise =
            do let (d1, d2) = G.splitAt (nmax - n) d
               yield (True, d1)
               yieldSplitted t 0 d2
          loop n p =
            do x <- lift (next p)
               case x of
                 Left r -> return r
                 Right ((t, d), p') ->
                   do let n' = n + G.length d
                      t' <- if n' < nmin
                        then yield (False, d) >> return False
                        else yieldSplitted t n d >> return t
                      loop (if t' then 0 else n') p'

freeIt :: Monad m => Producer (Bool, a) m r -> FreeT (Producer a m) m r
freeIt p0 =
    do x <- lift (next p0)
       case x of
         Left r -> return r
         Right (v, p') -> do n <- liftF (go v p')
                             case n of
                               Left r -> return r
                               Right p'' -> freeIt p''
  where go (eof, a) p =
          do yield a
             if eof
               then return (Right p)
               else do x <- lift (next p)
                       case x of
                         Left r -> return (Left r) -- premature end
                         Right (v, p') -> go v p'

byteHashShort :: HashType Word8
byteHashShort = HashType { hash = \x -> 31 * fromIntegral x, window = 16, mask = 0xf }

