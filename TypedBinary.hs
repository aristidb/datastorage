{-# LANGUAGE OverloadedStrings, BangPatterns, ViewPatterns #-}
module TypedBinary where

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.ByteString.Lazy.Builder as B
import Data.Text (Text)
import Data.Monoid
import Data.List
import Data.Binary.Get
import Data.Unique
import Data.Bits
import Control.Applicative
import Data.Binary.IEEE754
import GHC.Float (float2Double, double2Float)

data Void

-- do not leak (a hash of the) the transient ID to disk or rely on the particular ordering between runs of the program!
data Label = Label { humanReadable :: Text, transientId :: Unique }

instance Show Label where
    show = T.unpack . humanReadable

makeLabel :: Text -> IO Label
makeLabel name = Label name <$> newUnique

instance Eq Label where
    Label _ a == Label _ b = a == b

instance Ord Label where
    compare (Label _ a) (Label _ b) = compare a b

data Type =
    TVoid |
    TUnit |
    TBool |
    TInt { nbytes :: Maybe Int } |
    TUInt { nbytes :: Maybe Int } |
    TFloat32 |
    TFloat64 |
    TChar |
    TTuple { fields :: [(Label, Type)] } |
    TVariant { choices :: [(Label, Type)] } |
    TVector { elementType :: Type, fixedSize :: Maybe Int }
    -- TMap { source :: Type, destination :: Type }
  deriving (Show)

fieldTypes :: [(Label, Type)] -> [Type]
fieldTypes = map snd

typeBuilder :: Type -> TB.Builder
typeBuilder TVoid = TB.fromText "void"
typeBuilder TUnit = TB.fromText "unit"
typeBuilder TBool = TB.fromText "boolean"
typeBuilder (TInt l) = TB.fromText "integer" <> lengthBuilder l (TB.fromText "bytes")
typeBuilder (TUInt l) = TB.fromText "unsigned integer" <> lengthBuilder l (TB.fromText "bytes")
typeBuilder TFloat32 = TB.fromText "float32"
typeBuilder TFloat64 = TB.fromText "float64"
typeBuilder TChar = TB.fromText "character"
typeBuilder (TTuple fs) = TB.fromText "tuple of " <> fieldsBuilder fs
typeBuilder (TVariant cs) = TB.fromText "variant of " <> fieldsBuilder cs
typeBuilder (TVector t n) = TB.fromText "vector of " <> maybe mempty ((<> TB.singleton ' ') . TB.decimal) n <> typeBuilder t
-- typeBuilder (TMap s t) = TB.fromText "map from " <> typeBuilder s <> TB.fromText " to " <> typeBuilder t

lengthBuilder :: Maybe Int -> TB.Builder -> TB.Builder
lengthBuilder Nothing _unit = mempty
lengthBuilder (Just n) unit = TB.fromText " of " <> TB.decimal n <> TB.singleton ' ' <> unit

fieldsBuilder :: [(Label, Type)] -> TB.Builder
fieldsBuilder xs = TB.fromText "{ " <> innerBuilder <> TB.fromText " }"
    where innerBuilder = mconcat (intersperse (TB.fromText "; ") (map fieldBuilder xs))
          fieldBuilder (l, t) = labelBuilder l <> TB.fromText " as " <> typeBuilder t

labelBuilder :: Label -> TB.Builder
labelBuilder (Label name _) = TB.fromText name

data TypeSize =
    Constant {-# UNPACK #-} !Int |
    Range {-# UNPACK #-} !Int !(Maybe Int)
  deriving (Show)

addSize :: TypeSize -> TypeSize -> TypeSize
addSize (Constant a) (Constant b) = Constant (a + b)
addSize (Constant a) (Range x y) = Range (a + x) (fmap (+a) y)
addSize r@(Range _ _) c@(Constant _) = addSize c r
addSize (Range x1 y1) (Range x2 y2) = Range (x1 + x2) (liftA2 (+) y1 y2)

maxSize :: TypeSize -> TypeSize -> TypeSize
maxSize (Constant a) (Constant b) = Range (min a b) (Just $ max a b)
maxSize (Constant a) (Range x y) = Range (max a x) (max a <$> y)
maxSize r@(Range _ _) c@(Constant _) = maxSize c r
maxSize (Range x1 y1) (Range x2 y2) = Range (max x1 x2) (liftA2 max y1 y2)

multSize :: Int -> TypeSize -> TypeSize
multSize n (Constant a) = Constant (n * a)
multSize n (Range x y) = Range (n * x) ((* n) <$> y)

sizeOf :: Type -> TypeSize
sizeOf TVoid = Constant 0
sizeOf TUnit = Constant 0
sizeOf TBool = Constant 1
sizeOf (TInt Nothing) = Range 1 Nothing
sizeOf (TInt (Just n)) = Constant n
sizeOf (TUInt Nothing) = Range 1 Nothing
sizeOf (TUInt (Just n)) = Constant n
sizeOf TFloat32 = Constant 4
sizeOf TFloat64 = Constant 8
sizeOf TChar = Range 1 (Just 4) -- UTF8 uses 1-4 bytes i believe
sizeOf (TTuple (fieldTypes -> ts)) = foldl' addSize (Constant 0) (map sizeOf ts)
sizeOf (TVariant (fieldTypes -> ts)) = addSize (Constant 1) $ foldl' maxSize (Constant 0) (map sizeOf ts)
sizeOf (TVector _ Nothing) = Range 0 Nothing
sizeOf (TVector et (Just n)) = multSize n (sizeOf et)
-- sizeOf (TMap _ _) = Range 0 Nothing

parseVoid :: Type -> Get Void
parseVoid TVoid = fail "Void is uninhabited"
parseVoid _ = fail "Non-matching type"

parseUnit :: Type -> Get ()
parseUnit TUnit = return ()
parseUnit _ = fail "Non-matching type"

parseBool :: Type -> Get Bool
parseBool TBool = fmap (> 0) getWord8
parseBool _ = fail "Non-matching type"

parseInt :: (Num a, Ord a, Bits a) => Type -> Get a
parseInt (TInt Nothing) = parseVarInt
parseInt (TInt (Just n)) = parseFixedInt n
parseInt (TUInt Nothing) = parseVarUInt
parseInt (TUInt (Just n)) = parseFixedUInt n
parseInt _ = fail "Non-matching type"

parseVarUInt :: (Num a, Bits a) => Get a
parseVarUInt = go 0 0
  where
    go !acc !sh =
      do w <- getWord8
         let n = fromIntegral (w .&. 0x7F) `shiftL` sh .|. acc
         if w .&. 0x80 /= 0
           then go n (sh + 7)
           else return n

parseVarInt :: (Eq a, Num a, Bits a) => Get a
parseVarInt = do x <- parseVarUInt
                 let n = x `shiftR` 1
                 return $ if x .&. 1 /= 0 then complement n else n

encodeVarUInt :: (Integral a, Bits a) => a -> B.Builder
encodeVarUInt n | n < 0 = error "Negative numbers cannot be encoded by encodeVarUInt"
                | n < 0x80 = B.word8 (fromIntegral n)
                | otherwise = B.word8 (0x80 .|. fromIntegral (n .&. 0x7F)) <> encodeVarUInt (n `shiftR` 7)

encodeVarInt :: (Integral a, Bits a) => a -> B.Builder
encodeVarInt n | n < 0 = encodeVarUInt (complement n `shiftL` 1 .|. 1)
               | otherwise = encodeVarUInt (n `shiftL` 1)

parseFixedUInt :: (Num a, Bits a) => Int -> Get a
parseFixedUInt 0 = return 0
parseFixedUInt 1 = fromIntegral <$> getWord8
parseFixedUInt 2 = fromIntegral <$> getWord16le
parseFixedUInt 4 = fromIntegral <$> getWord32le
parseFixedUInt 8 = fromIntegral <$> getWord64le
parseFixedUInt n = do b <- getWord8
                      v <- parseFixedUInt (n-1)
                      return $ fromIntegral b .|. (v `shiftL` 8)

parseFixedInt :: (Num a, Ord a, Bits a) => Int -> Get a
parseFixedInt n = do v <- parseFixedUInt n
                     let nbits = n * 8
                     case () of
                       _ | v < 0 -> return v
                         | testBit v (nbits - 1) -> return (- (complement v) .&. (1 `shiftL` nbits - 1) - 1)
                         | otherwise -> return v

parseDouble :: Type -> Get Double
parseDouble TFloat32 = float2Double <$> getFloat32le
parseDouble TFloat64 = getFloat64le
parseDouble t = fromInteger <$> parseInt t

parseFloat :: Type -> Get Float
parseFloat TFloat32 = getFloat32le
parseFloat TFloat64 = double2Float <$> getFloat64le
parseFloat t = fromInteger <$> parseInt t
