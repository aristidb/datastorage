{-# LANGUAGE BangPatterns, RecordWildCards, FlexibleContexts, TemplateHaskell, RankNTypes, KindSignatures #-}
module RollingVector where

import qualified Data.Vector.Generic as G
import Data.Word
import Data.Bits
import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Lens
import Control.Monad.Morph (hoist)
import Conduit
import qualified Test.QuickCheck as QC
import qualified Data.Vector as V
-- import Debug.Trace
import Control.Exception (assert)
import Data.List

hashCombine :: Word64 -> Word64 -> Word64
hashCombine !x !y = (x `rotateL` 1) `xor` y
{-# INLINE hashCombine #-}

data HashType a = HashType {
      hash :: a -> Word64
    , window :: Int
    , mask :: Word64
    }

addRemove :: Int -> Word64 -> Word64 -> Word64
addRemove !w !o !n = (o `rotateL` w) `xor` n
{-# INLINE addRemove #-}

prehash :: (G.Vector v a, G.Vector v Word64) => HashType a -> v a -> Word64 -> Word64
prehash HashType{..} v h = G.foldl' hashCombine h x
    where x = G.map hash v

nextBoundary :: (G.Vector v a, MonadState Word64 m) => HashType a -> v a -> v a -> m (Maybe Int)
nextBoundary HashType{..} !old !new =
    state $ \h0 -> runST (go h0 0)
  where
    inputSize = G.length new
    go !h !iIn | iIn < inputSize =
                        do case (h' .&. mask) == mask of
                             True -> return (Just $ iIn + 1, h')
                             False -> go h' (iIn + 1)
               | otherwise = return (Nothing, h)
      where hi = addRemove window (hash (old G.! iIn)) (hash (new G.! iIn)) -- TODO: G.unsafeIndex
            h' = hashCombine h hi
{-# INLINE nextBoundary #-}

contiguous :: (Monad m, G.Vector v a) => HashType a -> v a -> v a -> Producer (StateT Word64 m) (Bool, v a)
contiguous ht = go
    where
      go !old !new =
        do x <- nextBoundary ht old new
           case x of
             Nothing -> yield (False, new)
             Just n -> do yield (True, a)
                          go old' new'
               where (a, new') = G.splitAt n new
                     old' = G.drop n old

data HashState a = HashState { _lastHash :: !Word64, _lastWindow :: !a }

makeLenses ''HashState

initialHashState :: G.Vector v a => HashState (v a)
initialHashState = HashState 0 G.empty

initial :: (Monad m, G.Vector v a, G.Vector v Word64) => HashType a -> Maybe (v a) -> Conduit (v a) (StateT (HashState (v a)) m) (Bool, v a)
initial _ Nothing = return ()
initial ht@HashType{..} (Just x) =
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
                   else leftover xn
         else leftover x

warm :: (Monad m, G.Vector v a) => HashType a -> Maybe (v a) -> Conduit (v a) (StateT (HashState (v a)) m) (Bool, v a)
warm _ Nothing = return ()
warm ht@HashType{..} (Just x) =
    do w <- use lastWindow
       w' <- hoist (zoom lastHash) $
         assert (G.length w == window) $
         do contiguous ht w (G.take window x)
            let len = G.length x
            when (len > window) $ contiguous ht x (G.drop window x)
            return $ G.drop len w G.++ G.drop (len - window) x
       lastWindow .= w'
       await >>= warm ht

rollsplit :: (Monad m, G.Vector v a, G.Vector v Word64) => HashType a -> Conduit (v a) (StateT (HashState (v a)) m) (Bool, v a)
rollsplit ht =
    do await >>= initial ht
       await >>= warm ht

clamp :: (Monad m, G.Vector v a) => Int -> Int -> Conduit (Bool, v a) m (Bool, v a)
clamp nmin nmax = loop 0
  where
    loop n = await >>= maybe (return ()) (go n)

    go n (t, d) =
        do let t' = not (G.null d2) || (t && n' >= nmin)
           yield (t', d1)
           unless (G.null d2) $ leftover (t, d2)
           loop (if t' then 0 else n')
      where (d1, d2) = G.splitAt (nmax - n) d
            n' = n + G.length d1

recombine :: (Monad m, G.Vector v a) => Conduit (Bool, v a) m (v a)
recombine = go G.empty
  where
    go x =
      do n <- await
         case n of
           Nothing -> yield x
           Just (False, y) -> go (x G.++ y)
           Just (True, y) -> yield (x G.++ y) >> go G.empty

simpleRollsplit :: (G.Vector v a, G.Vector v Word64) => HashType a -> [v a] -> [v a]
simpleRollsplit ht xs = runIdentity (yieldMany xs $$ evalStateC initialHashState (rollsplit ht =$= recombine) =$= sinkList)

simpleRollsplit' :: (G.Vector v a, G.Vector v Word64) => HashType a -> [v a] -> [(Bool, v a)]
simpleRollsplit' ht xs = runIdentity (yieldMany xs $$ evalStateC initialHashState (rollsplit ht) =$= sinkList)

byteHashShort :: HashType Word8
byteHashShort = HashType { hash = \x -> 31 * fromIntegral x, window = 16, mask = 0xf }

prop_allInputIsOutput :: (QC.Arbitrary a, Show a, Eq a) => HashType a -> QC.Property
prop_allInputIsOutput ht = QC.forAll (QC.listOf (fmap V.fromList QC.arbitrary)) $ \xs -> V.concat (simpleRollsplit ht xs) == V.concat xs

prop_inputSplit :: (QC.Arbitrary a, Show a, Eq a) => HashType a -> QC.Property
prop_inputSplit ht = QC.forAll (QC.listOf (fmap V.fromList QC.arbitrary)) $ \xs -> simpleRollsplit ht xs == simpleRollsplit ht [V.concat xs]

prop_prefix :: (QC.Arbitrary a, Show a, Eq a) => HashType a -> QC.Property
prop_prefix ht = QC.forAll (fmap V.fromList QC.arbitrary) $ \xs ->
                 QC.forAll (fmap V.fromList QC.arbitrary) $ \ys ->
                 let a = simpleRollsplit ht [xs, ys]; b = simpleRollsplit ht [xs] in init' b `isPrefixOf` a

prop_suffix :: (QC.Arbitrary a, Show a, Eq a) => HashType a -> QC.Property
prop_suffix ht = QC.forAll (fmap V.fromList QC.arbitrary) $ \xs ->
                 QC.forAll (fmap V.fromList QC.arbitrary) $ \ys ->
                 let a = simpleRollsplit ht [xs, ys]; c = simpleRollsplit ht [ys] in tail' c `isSuffixOf` a

prop_valid :: (QC.Arbitrary a, Show a, Eq a) => HashType a -> QC.Property
prop_valid ht = prop_allInputIsOutput ht QC..&&. prop_inputSplit ht QC..&&. prop_prefix ht QC..&&. prop_suffix ht

init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

tail' :: [a] -> [a]
tail' xs = drop 1 xs
