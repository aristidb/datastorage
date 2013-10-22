{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, NoMonomorphismRestriction #-}
-- vim: sts=2:sw=2:ai:et
--module Rolling where

import Data.Word
import Data.Bits
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Storable.ByteString as S
import Control.Monad.ST
import qualified Data.ByteString as B
import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
import Control.Monad.State.Strict
import Control.Exception (assert)
import qualified Test.QuickCheck as QC
import Data.List
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.DeepSeq
import Criterion.Main
import Debug.Trace

window :: Int
window = 16 --256

mask :: Word64
mask = 0xf --0x1fff

testMask :: Word64 -> Bool
testMask x = x .&. mask == mask
{-# INLINE testMask #-}

hash :: Word8 -> Word64
hash x = lut S.! fromIntegral x
{-# INLINE hash #-}

rhash :: Word8 -> Word8 -> Word64 -> Word64
rhash old new h = h `rotateL` 1 `xor` (hash old `rotateL` window) `xor` hash new

(+>) :: Word64 -> Word64 -> Word64
(+>) !o !n = (o `rotateL` window) `xor` n
{-# INLINE (+>) #-}

hashCombine :: Word64 -> Word64 -> Word64
hashCombine !x !y = x `rotateL` 1 `xor` y
{-# INLINE hashCombine #-}

type Data = S.Vector Word8

roll :: S.Vector Word64 -> S.Vector Word64
roll hashed = S.postscanl hashCombine h (S.zipWith (+>) hashed (S.drop window hashed))
  where h = S.foldl' hashCombine 0 (S.take window hashed)

prop_rolls :: QC.Property
prop_rolls = QC.forAll inputVector $ \a ->
             QC.forAll (QC.suchThat inputVector (\v -> S.length v >= window)) $ \b ->
               S.last (roll b) == S.last (roll (a S.++ b))

contiguous :: Data -> [Data]
contiguous xs = zipWith (\a b -> S.slice a (b - a) xs) (0:boundaries) boundaries
  where hashed = S.map hash xs
        rolled = roll hashed
        markers = S.findIndices testMask rolled
        boundaries = (++[S.length xs]) $ map (+(window+1)) $ S.toList markers

cprop_allInputIsOutput :: QC.Property
cprop_allInputIsOutput = QC.forAll inputVector $ \xs -> S.concat (contiguous xs) == xs

cprop_prefix :: QC.Property
cprop_prefix = QC.forAll inputVector $ \xs -> QC.forAll inputVector $ \ys ->
  let a = contiguous (xs S.++ ys); b = contiguous xs in init' b `isPrefixOf` a

cprop_suffix :: QC.Property
cprop_suffix = QC.forAll inputVector $ \xs -> QC.forAll inputVector $ \ys ->
  let a = contiguous (xs S.++ ys); c = contiguous ys in tail' c `isSuffixOf` a

cprop_valid :: QC.Property
cprop_valid = cprop_allInputIsOutput QC..&. cprop_prefix QC..&. cprop_suffix

data HashState
  = HashState {
      _lastHash :: {-# UNPACK #-} !Word64
    , _lastWindow :: !Data
    }

initialState :: HashState
initialState = HashState 0 S.empty

lastHash :: L.Lens' HashState Word64
lastHash f (HashState h w) = fmap (\h' -> HashState h' w) (f h)

lastWindow :: L.Lens' HashState Data
lastWindow f (HashState h w) = fmap (HashState h) (f w)

data Output = Partial { getOutput :: Data } | Complete { getOutput :: Data }
  deriving (Show)

isComplete :: Output -> Bool
isComplete (Partial _) = False
isComplete (Complete _) = True

rollingBoundaries :: Data -> Data -> Word64 -> (Word64, S.Vector Int)
rollingBoundaries old new h0 = runST $ do mv <- SM.new (S.length new)
                                          (h', len) <- runner mv
                                          v <- S.unsafeFreeze (SM.take len mv)
                                          return (h', v)
  where
    runner mv = go h0 0 0
      where
        go !h !iOut !iIn | iIn < S.length new =
                           do
                              let hi = hash (old `S.unsafeIndex` iIn) +> hash (new `S.unsafeIndex` iIn)
                                  h' = hashCombine h hi
                              iOut' <- case testMask h' of
                                         True -> do SM.unsafeWrite mv iOut (iIn + 1)
                                                    return (iOut + 1)
                                         False -> return iOut
                              go h' iOut' (iIn + 1)
                         | otherwise = return (h, iOut)

rollsplitP :: Monad m => Pipe Data Output (StateT HashState m) ()
rollsplitP =
  forever $
  do
    x <- await
    w <- L.use lastWindow
    w' <- hoist (L.zoom lastHash) $ do
      if S.length w < window
      then do
              let n = window - S.length w
              let w' = w S.++ S.take n x
              let x' = S.drop n x
              modify (\h -> S.foldl' hashCombine h (S.map hash (S.take n x)))
              yield (Partial (S.take n x))
              step w' x'
      else step w x
    lastWindow .= w'
  where
    step w x | S.null x  = return w
    step w x | otherwise =
      do
        let len = S.length x
        chow w (S.take window x)
        when (len > window) $ chow x (S.drop window x)
        return (S.drop len w S.++ S.drop (len - window) x)

    chow old new = assert (S.length old >= S.length new) $
      do
        h <- get
        let (newH, boundaries) = rollingBoundaries old new h
        -- traceShow (boundaries, old, new) $ return ()
        let sliceAction a b = do
              yield (Complete (S.unsafeSlice a (b - a) new))
              return b
        n <- S.foldM sliceAction 0 boundaries
        when (n < S.length new) $ yield (Partial (S.drop n new))
        put newH

recombine :: Monad m => Int -> Int -> Producer Output m a -> Producer Data m a
recombine nmin nmax = loop S.empty
  where
    loop d p =
      case S.length d of
        n | n < nmax ->
            do e <- lift (next p)
               case e of
                 Left v -> yield d >> return v
                 Right (output, p') -> if isComplete output && S.length d' >= nmin
                                       then yield d' >> loop S.empty p'
                                       else loop d' p'
                   where d' = d S.++ getOutput output
          | otherwise ->
            do yield (S.take nmax d)
               loop (S.drop nmax d) p

rollsplit :: Monad m => Int -> Int -> Producer Data m () -> Producer Data m ()
rollsplit nmin nmax p = recombine nmin nmax $ evalStateP initialState $ hoist lift p >-> rollsplitP

rollsplitL :: [Data] -> [Data]
rollsplitL = filter (not.S.null) . rollsplitL'

rollsplitL' :: [Data] -> [Data]
rollsplitL' xs = P.toList $ rollsplit 0 (maxBound :: Int) (each xs)

inputVector :: (QC.Arbitrary a, S.Storable a) => QC.Gen (S.Vector a)
inputVector = QC.sized $ \n -> do
  k <- QC.choose (0,n)
  a <- QC.choose (0,window)
  let len = max 0 $ (k-1)*window+a
  fmap S.fromList $ QC.vector len


init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

tail' :: [a] -> [a]
tail' xs = drop 1 xs

prop_allInputIsOutput :: QC.Property
prop_allInputIsOutput = QC.forAll (QC.listOf inputVector) $ \xs -> S.concat (rollsplitL xs) == S.concat xs


prop_inputSplit :: QC.Property
prop_inputSplit = QC.forAll (QC.listOf inputVector) $ \xs -> rollsplitL xs == rollsplitL [S.concat xs]

prop_prefix :: QC.Property
prop_prefix = QC.forAll inputVector $ \xs -> QC.forAll inputVector $ \ys ->
  let a = rollsplitL [xs, ys]; b = rollsplitL [xs] in init' b `isPrefixOf` a

prop_suffix :: QC.Property
prop_suffix = QC.forAll inputVector $ \xs -> QC.forAll inputVector $ \ys ->
  let a = rollsplitL [xs, ys]; c = rollsplitL [ys] in tail' c `isSuffixOf` a

prop_concat :: QC.Property
prop_concat = prop_prefix QC..&. prop_suffix

prop_eq :: QC.Property
prop_eq = QC.forAll inputVector $ \xs -> rollsplitL' [xs] == contiguous xs

prop_valid :: QC.Property
prop_valid = prop_allInputIsOutput QC..&. prop_inputSplit QC..&. prop_concat QC..&. prop_eq

qc :: IO ()
qc = QC.quickCheckWith (QC.stdArgs { QC.maxSuccess = 700 }) (cprop_valid QC..&. prop_valid)

test :: Int -> Int -> [Data] -> IO ()
test nmin nmax xs = runEffect $ for (rollsplit nmin nmax (each xs)) (lift . print)

test2 :: [Data] -> IO ()
test2 xs = runEffect $ for (evalStateP initialState $ each xs >-> rollsplitP) (lift . print)

lut :: S.Vector Word64
lut = S.fromList [
    0xfead5b707dc7705c, 0x377c1e06dc1e45cf, 0x0184179586d5ae76, 0xd23aa044f8193aa6,
    0xbd8ef5fcde7bd95e, 0x29a822b00a75ea90, 0x5ba03c1b2fdc2f86, 0x4f67d80bad410270,
    0xfcc4b6b0cb67bb75, 0x8f4359ea8777f5d0, 0x5a110ec6371430f5, 0xe15dae4e9709aa66,
    0xf8008efefbc29115, 0xd667f1ec0a5bcccf, 0xf56a2815cb29dbf8, 0xfc684381b1846cad,
    0x15be2b7ed5231683, 0x0c226b1f911732c5, 0xe449b5885fd1d930, 0xf6d86c27a242cf1d,
    0xc45cd857fdd32ca4, 0x36de9a99be8c3419, 0x8c34f2ee7050433f, 0x9fca9c3c61c05ce8,
    0xf3727fe070e026f1, 0x019172d2948847be, 0x547bcbfaaef62543, 0x97df2b9f6c70001b,
    0x2833bcc8513a3ae8, 0x63fb313206823b31, 0xe97ba036be16b286, 0xdde1143e9e8f7684,
    0xc4e0d8e6e491054c, 0x7c488b71d4cab5fd, 0x2e0986d1f8e2d362, 0x97978ef2435b5cb1,
    0x5506c42930dfc2a4, 0x3205d4c5886673cf, 0x22d5bfe8ba771cc5, 0x39503d4cb89afd6b,
    0x9fc1cad11e5b5755, 0xce967aae1fccb3c3, 0x0f164d18916d4043, 0x113893188dd7e602,
    0xedb9343bcf5f16ef, 0x0ce3b155cfc00cbf, 0x144c6c104492dff7, 0xe1ab67476c3c787f,
    0x06357095472efed7, 0x29573d1071bb1237, 0xe27e7ce26dcd54c6, 0x03a8623e84a0e89c,
    0x381194ea8cb4f8fe, 0x53cbe859f67ae71c, 0x85e08cd4432b31f5, 0x21f9758ed837239d,
    0x835aad64a22264b4, 0xf83ec94f1a0159f3, 0x016a63af376515a4, 0x8af0e82577e30b6e,
    0x4e3717bd197f8c8d, 0x20193e9699604cea, 0xfa8eb53310f65183, 0xb4980361f9a7e3b3,
    0xccb9771f04663244, 0xe975bed27cfd7a05, 0x46e9eb7dd3ffd802, 0xbe4b5af269bf4b64,
    0xf3ff155176f34e7e, 0xfc8e0e5c20814c3f, 0x3910b75cbe6677f5, 0x86fb5f8ef54ad45a,
    0xad430e08aa6aea72, 0x6a82d96b64a7f48c, 0x8aa8af38d787327e, 0x3a10990b93334a3d,
    0x010c38275c1fc3ac, 0x03eac81f78d29425, 0x356d5c2052ac066f, 0x9a14a98e51d4eafc,
    0x9c07059dac0831b5, 0x51c3aa247abb4a6c, 0x7f9d96a58c371a56, 0xe098fe430cedd615,
    0x960e3a1dbe524565, 0xf0401c427323f9e8, 0x2c22d55a6135331c, 0x8684f0837cb96bc9,
    0xb7ac495220cc7e9b, 0x4083bd84b86ad15e, 0xd7434c89c5464bfd, 0x6af7d49f8b658a9d,
    0x1b9878f861dd7d00, 0x6f5b7c3c3baaca36, 0xab6e6179f8b69d37, 0xda4c68dc98bcecd5,
    0x42cb0a636ba3fa38, 0x04f2814021d2f99a, 0x53a069c086a24b1d, 0xce39ccfebf0fd3cf,
    0x6da4576c5b938392, 0x73961a564d3684af, 0xe62079e996d934ab, 0x0fe43a5aded65708,
    0x8ea00b7c1ba4a6b4, 0xb3f325b0d1acc61a, 0xb7d68c724ee32277, 0x8838a5a56df55c2e,
    0x639e1a16eebc31dc, 0xc824476623ee8713, 0x6cfd35dd41d4a35f, 0xc0cf8dfb2eb67ca4,
    0x9b2db80c20bf8281, 0x8f43090206cbc3a5, 0xf018850ed3cce401, 0xdd9c873dbc24dacf,
    0x8c310f7792646b4a, 0xd64514da16af625c, 0x7de1ce3ec600b27d, 0x97742bfa39500785,
    0x37e57a0934a9e321, 0x0872c00e96058f5f, 0x9a9291f9452ff389, 0x1240421b117dc351,
    0x179b86fb16a829ed, 0xf773ae083f3cd908, 0xc32304664f8ce10f, 0x3bf48227de1ad114,
    0x1e051eb805a697ad, 0x70ef7848e5ff8346, 0xe7dc2e245cb0d7f3, 0xf9ccdaee9f48ca62,
    0x2ef082a2b4419ad3, 0x8a9287ed4ecd5750, 0x9f7b959d7c2ff1ba, 0x1b86fb1047eee0a9,
    0x342ac90ae166bfb4, 0x9c28c8b1a92ab370, 0x7248bbdc9faaf6c1, 0x1a3b7cdd85269cfa,
    0xcf3af94626a8c04f, 0x7a4ba5bef4f2856c, 0xa80dd551adc99bb8, 0xd7cef340d2e894ba,
    0x134c05ffd4928eba, 0x78b5ec19a589ff89, 0xd8d23f88cbd19231, 0xa95a65c91e98558c,
    0x0a996675f053aa80, 0xf35d31427e2dce98, 0x9985000bf5aefe7b, 0x73ac3f7cc6a7d6e0,
    0xc48e7406cc446de3, 0xf919aa74e167dbbe, 0x8b09e12fc48dbbcd, 0x33b5f1fbafc21ead,
    0xfbbf051f129c275d, 0x3239b2a8ee124a62, 0x8984d8d6b4a2ad6a, 0xe2293308df3acd2b,
    0x2529297459fa7fac, 0x36c72d90491e670f, 0xe7ca9d442ddae3b3, 0x8fa38fa2a012ce0d,
    0x5caac60bfad2c355, 0xa92158609a2e1285, 0x8a097df8fdb4c6cf, 0x1f1b6f8230e90ff0,
    0x8369b558bec7e569, 0x2e9d4f7aabcb210d, 0xafb7d767ac296c18, 0x2f5cd5c2515d81c1,
    0x45934465add6c676, 0xa5a45e774fd55fc3, 0xb4cb7e6e96d2a59d, 0xfdc6635b7ec562b0,
    0x1bcedbd2ea8b4bf9, 0x0ab5b24bb7ccf99d, 0x31aa59853645b444, 0x9149c77d16594ff5,
    0x77d9fee30ed21a67, 0x12982d192e88015b, 0xd45d1f788a6a1ef3, 0x332d24e0edf8ca0a,
    0x88c7695975b9d19b, 0x065f29d44b168060, 0xd2a2a94e28c99de3, 0x8235595da56d3cd7,
    0x4d58d451daa20b65, 0x36a1edff55e32372, 0xdf5c5c1e6b75c40b, 0x85c8a6bf77ecf2d3,
    0x7d72ca70ec2882f2, 0xfe286fdc7db952ca, 0x13e308d730603af4, 0x7803625ac832d270,
    0x920af03f31044bc6, 0x1fe4f2d207fecc03, 0x38537ef4067d59e9, 0x0eca7ef2100d20fc,
    0x0a6fe0e637366afc, 0x9d52574f1a85c9e5, 0xd0a0fdbd96ed75d2, 0xb6785ac2254c0d92,
    0x708122c30c97ff1f, 0x3e265597b73bc11e, 0xb3a870c781058225, 0xd93d905b304c4ec6,
    0x52ff676e213f6ccb, 0xde7503df2ee1aed6, 0xcc95a88b5c5fc05a, 0x351d157d93a36b21,
    0xf33702abc575fb08, 0xedcec8d4e602f76d, 0x210d869a4002a35a, 0xed6debb5a777ff11,
    0x4aa4ef159b20a5da, 0x1248e5993f479e53, 0x6afdd7979ed93e76, 0x0048f115bbf8fcc9,
    0x3c43dcc810f39b4c, 0xd596e217217ba38d, 0xf026411acc2ffc3f, 0xa9bfe1a48496f61c,
    0x3c5d0feeb403ffe6, 0xf3cc4252ff0a0e4b, 0x9287b23486699a23, 0xfcb9175b7c6ad7f1,
    0xd28a3ca6572757d0, 0x9679f32c0d2f01f8, 0x9aec761f1611e561, 0x00d429298b4544fc,
    0x08aa8292dcaff98f, 0x36c88c6cf0188da2, 0x674a34c8653e66b8, 0x0e6316f1731e02b8,
    0xd049743dfb30601e, 0xe99b74d4efa92d88, 0xa41f9c33729e01ad, 0xe0a2c2031c01b13d,
    0x04a396e2dfc4f61f, 0xdb2556b846e68964, 0xa5de683723f23704, 0x456f750ec820ea4f,
    0x47b81bb8dda336aa, 0x7c0b4cab5dffb32e, 0x020a4567a37ada75, 0x66325b8ce0bc3890,
    0x566e9b6d6a194d5e, 0x53efbb71787c4049, 0xee775a2e794219de, 0xa65c279ca51a6a46,
    0xaac56091fcec9c38, 0x0b34953b386ed0c4, 0xde46839785d1b945, 0x623a65d725974df2
  ]

{-
stats :: [Int] -> IO ()
stats = gg
-}

main :: IO ()
main = do
  benchRawData <- fmap S.byteStringToVector (B.readFile "bench.dat")
  let benchData bs = force $ map (\p -> S.slice p bs benchRawData) [0,bs..S.length benchRawData-1]
  print $ map S.length (rollsplitL' $ benchData 4096)
  print $ map S.length (rollsplitL' $ benchData 65536)
  defaultMain [
    let dat = benchData 4096 in dat `seq` bench "simple 4096" $ nf rollsplitL' dat,
    let dat = benchData 65536 in dat `seq` bench "simple 65536" $ nf rollsplitL' dat,
    let dat = benchData 1048576 in dat `seq` bench "simple 1048576" $ nf rollsplitL' dat,
    let dat = benchData 4096 in dat `seq` bench "clamped 4096" $ nf (P.toList . rollsplit 1024 16386 . each)  dat,
    let dat = benchData 65536 in dat `seq` bench "clamped 65536" $ nf (P.toList . rollsplit 1024 16386 . each) dat
    ]
