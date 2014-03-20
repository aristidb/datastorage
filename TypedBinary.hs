{-# LANGUAGE OverloadedStrings, BangPatterns, ViewPatterns, RecordWildCards, GADTs, RankNTypes, TupleSections, KindSignatures, TypeFamilies, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module TypedBinary where

-- import IndexTree
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Monoid
import Data.List
import Data.Binary.Get
-- import Data.Unique
import Data.Bits
import Control.Applicative
import Data.Binary.IEEE754
import GHC.Float (float2Double, double2Float)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Generic as G
-- import Succinct.Dictionary
import Data.Functor.Invariant
import Control.Monad.Trans.Class
import Control.Monad.Writer hiding (void)
import Control.Lens
import qualified Data.Text.Lens as T
import qualified Data.ByteString.Lens as B
import qualified Data.Vector.Lens as V
import GHC.Generics hiding (from, to)
import GHC.Generics.Lens
import Data.Word
import Data.Int

data Void

instance Show Void where
    show _ = error "Void"

type Label = Text

{-
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
-}

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
  deriving (Eq, Show)

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
lengthBuilder Nothing _measure = mempty
lengthBuilder (Just n) measure = TB.fromText " of " <> TB.decimal n <> TB.singleton ' ' <> measure

fieldsBuilder :: [(Label, Type)] -> TB.Builder
fieldsBuilder xs = TB.fromText "{ " <> innerBuilder <> TB.fromText " }"
    where innerBuilder = mconcat (intersperse (TB.fromText "; ") (map fieldBuilder xs))
          fieldBuilder (l, t) = labelBuilder l <> TB.fromText " as " <> typeBuilder t

labelBuilder :: Label -> TB.Builder
labelBuilder label = TB.fromText label

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
sizeOf (TVector _ Nothing) = addSize (sizeOf (TUInt Nothing)) (Range 0 Nothing)
sizeOf (TVector et (Just n)) = multSize n (sizeOf et)
-- sizeOf (TMap _ _) = Range 0 Nothing

isScalar :: Type -> Bool
isScalar (TTuple _) = False
isScalar (TVariant _) = False
isScalar (TVector _ _) = False
isScalar _ = True

type Parser a = Type -> Get a

type Generator a = Type -> a -> Either String B.Builder

data Grammar a = Grammar { parse :: Parser a, write :: Generator a, defaultType :: Type }

parse' :: Grammar a -> Get a
parse' g = parse g (defaultType g)

simpleParse :: Grammar a -> L.ByteString -> a
simpleParse g = runGet (parse' g)

write' :: Grammar a -> a -> Either String B.Builder
write' g = write g (defaultType g)

simpleWrite :: Grammar a -> a -> Either String L.ByteString
simpleWrite g x = B.toLazyByteString <$> write' g x

writeW :: Grammar a -> Type -> a -> WriterT B.Builder (Either String) ()
writeW g t x = do a <- lift (write g t x)
                  tell a

instance Invariant Grammar where
    invmap f g (Grammar p w dt) = Grammar (fmap f . p) ((. g) . w) dt

isomap :: Iso' a b -> Grammar a -> Grammar b
isomap m = invmap (view m) (review m)

class Grammatical a where
    grammar :: Grammar a

simpleTyped :: Type -> Get a -> (a -> B.Builder) -> Grammar a
simpleTyped t p w = Grammar { parse = \t' -> if t == t' then p else fail ("Non-matching type " ++ show t' ++ ", expected " ++ show t),
                              write = \t' -> if t == t' then Right . w else const (Left "Non-matching type"),
                              defaultType = t }

void :: Grammar Void
void = simpleTyped TVoid (fail "Void is uninhabited") (\_ -> error "Void is uninhabited")

instance Grammatical Void where grammar = void

unit :: Grammar ()
unit = simpleTyped TUnit (return ()) (\() -> mempty)

instance Grammatical () where grammar = unit

bool :: Grammar Bool
bool = simpleTyped TBool (fmap (> 0) getWord8) (\v -> B.word8 (if v then 1 else 0))

instance Grammatical Bool where grammar = bool

intWith :: (Integral a, Bits a) => Type -> Grammar a
intWith def = Grammar { .. }
    where
      parse (TInt Nothing) = parseVarInt
      parse (TInt (Just n)) = parseFixedInt n
      parse (TUInt Nothing) = parseVarUInt
      parse (TUInt (Just n)) = parseFixedUInt n
      parse _ = fail "Non-matching type"
      write (TInt Nothing) = Right . encodeVarInt
      write (TUInt Nothing) = Right . encodeVarUInt
      write (TInt (Just n)) = Right . encodeFixedInt n
      write (TUInt (Just n)) = Right . encodeFixedInt n
      write _ = const (Left "Non-matching type")
      defaultType = def

instance Grammatical Int where grammar = intWith (TInt Nothing)
instance Grammatical Int8 where grammar = intWith (TInt (Just 1))
instance Grammatical Int16 where grammar = intWith (TInt (Just 2))
instance Grammatical Int32 where grammar = intWith (TInt (Just 4))
instance Grammatical Int64 where grammar = intWith (TInt (Just 8))
instance Grammatical Integer where grammar = intWith (TInt Nothing)

instance Grammatical Word where grammar = intWith (TUInt Nothing)
instance Grammatical Word8 where grammar = intWith (TUInt (Just 1))
instance Grammatical Word16 where grammar = intWith (TUInt (Just 2))
instance Grammatical Word32 where grammar = intWith (TUInt (Just 4))
instance Grammatical Word64 where grammar = intWith (TUInt (Just 8))

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
parseFixedUInt _ = error "Non-standard fixed UInt size not supported yet"

encodeFixedInt :: Integral a => Int -> a -> B.Builder
encodeFixedInt 0 _ = mempty
encodeFixedInt 1 v = B.word8 (fromIntegral v)
encodeFixedInt 2 v = B.word16LE (fromIntegral v)
encodeFixedInt 4 v = B.word32LE (fromIntegral v)
encodeFixedInt 8 v = B.word64LE (fromIntegral v)
encodeFixedInt _ _ = error "Non-standard fixed UInt size not supported yet"

parseFixedInt :: (Num a, Ord a, Bits a) => Int -> Get a
parseFixedInt n = do v <- parseFixedUInt n
                     let nbits = n * 8
                     case () of
                       _ | v < 0 -> return v
                         | testBit v (nbits - 1) -> return (- (complement v) .&. (1 `shiftL` nbits - 1) - 1)
                         | otherwise -> return v

double :: Grammar Double
double = Grammar { parse = parseF, write = writeF, defaultType = def }
    where
      parseF TFloat32 = float2Double <$> getFloat32le
      parseF TFloat64 = getFloat64le
      parseF t = fromInteger <$> parse grammar t
      writeF TFloat32 = Right . B.floatLE . double2Float
      writeF TFloat64 = Right . B.doubleLE
      writeF _ = const (Left "Non-matching type")
      def = TFloat64

instance Grammatical Double where grammar = double

float :: Grammar Float
float = Grammar { parse = parseF, write = writeF, defaultType = def }
    where
      parseF TFloat32 = getFloat32le
      parseF TFloat64 = double2Float <$> getFloat64le
      parseF t = fromInteger <$> parse grammar t
      writeF TFloat32 = Right . B.floatLE
      writeF TFloat64 = Right . B.doubleLE . float2Double
      writeF _ = const (Left "Non-matching type")
      def = TFloat32

instance Grammatical Float where grammar = float

-- TODO
char :: Grammar Char
char = simpleTyped TChar undefined undefined

instance Grammatical Char where grammar = char

vector :: G.Vector v a => Grammar a -> Grammar (v a)
vector inner = Grammar { parse = parseF, write = writeF, defaultType = def }
    where
      parseF (TVector t qn) =
        do n <- case qn of
                  Just n0 -> return n0
                  Nothing -> parseVarUInt
           G.replicateM n (parse inner t)
      parseF _ = fail "Non-matching type"
      writeF (TVector t Nothing) v = execWriterT $
        do tell (encodeVarUInt (G.length v))
           G.forM_ v (writeW inner t)
      writeF (TVector t (Just n)) v | n == G.length v = execWriterT $ G.forM_ v (writeW inner t)
                                    | otherwise = Left "Non-matching length"
      writeF _ _ = Left "Non-matching type"
      def = TVector (defaultType inner) Nothing

instance Grammatical a => Grammatical (V.Vector a) where
    grammar = vector grammar

instance (Grammatical a, UV.Unbox a) => Grammatical (UV.Vector a) where
    grammar = vector grammar

instance (Grammatical a, SV.Storable a) => Grammatical (SV.Vector a) where
    grammar = vector grammar

instance Grammatical a => Grammatical [a] where
    grammar = isomap (from V.vector) grammar

instance Grammatical T.Text where
    grammar = isomap T.packed grammar

instance Grammatical LT.Text where
    grammar = isomap T.packed grammar

instance Grammatical B.ByteString where
    grammar = isomap B.packedBytes grammar

instance Grammatical L.ByteString where
    grammar = isomap B.packedBytes grammar

-- TODO: more types here

data Element a = Pure a | Labelled Label (Grammar a)

instance Show a => Show (Element a) where
    showsPrec n (Pure a) = showParen (n > 10) (showString "Pure " . showsPrec 11 a)
    showsPrec n (Labelled l _) = showParen (n > 10) (showString "Labelled <" . shows l . showString "> <g>")

data Tuple a where
    Nil :: Tuple ()
    (:.:) :: Element a -> Tuple b -> Tuple (a, b)

infixr 9 :.:

{-
instance Show a => Show (Tuple a) where
    showsPrec _ Nil = showString "Nil"
    showsPrec n (c :.: q) = showParen (n > 9) (showsPrec 10 c . showString " :.: " . showsPrec 10 q)
-}

parseStep :: Label -> Tuple a -> Parser (Tuple a)
parseStep _ Nil _ = fail "Label not found"
parseStep l (Labelled l' g :.: q) t | l == l' = (:.: q) . Pure <$> parse g t
parseStep l (c :.: q) t = (c :.:) <$> parseStep l q t

extract :: Tuple a -> Maybe a
extract Nil = Just ()
extract (Pure x :.: xs) = (x, ) <$> extract xs
extract (Labelled _ _ :.: _) = Nothing

buildStep :: Tuple a -> Label -> Generator a
buildStep Nil _l _t () = Left "Label not found"
buildStep (Pure _x :.: xs) l t (_, q) = buildStep xs l t q
buildStep (Labelled l g :.: xs) l' t (c, q) | l == l' = write g t c
                                            | otherwise = buildStep xs l' t q

tuple :: Tuple a -> Grammar a
tuple p = Grammar { parse = parseF, write = writeF, defaultType = TTuple (def p) }
    where
      parseF (TTuple fs) =
        do p' <- go p fs
           case extract p' of
             Just a -> return a
             Nothing -> fail "Not all fields could be parsed"
        where
          go px [] = return px
          go px ((l,t) : xs) = do px' <- parseStep l px t
                                  go px' xs
      parseF _ = fail "Non-matching type, tuple expected"

      writeF (TTuple fs) a = execWriterT (go fs)
        where
          go [] = return ()
          go ((l,t) : xs) = do tell =<< lift (buildStep p l t a)
                               go xs
      writeF _ _ = fail "Non-matching type, tuple expected"

      def :: Tuple a -> [(Label, Type)]
      def Nil = []
      def (Pure _x :.: xs) = def xs
      def (Labelled l g :.: xs) = (l, defaultType g) : def xs

data Variant a where
    V :: Variant Void
    (:|:) :: (Label, Grammar a) -> Variant b -> Variant (Either a b)

infixr 9 :|:

variant :: Variant a -> Grammar a
variant p = Grammar { parse = parseF, write = writeF, defaultType = TVariant (def p) }
    where
      parseF (TVariant fs) =
        do idx <- fromIntegral <$> getWord8
           (l,t) <- case drop idx fs of
             [] -> fail ("Invalid tag index " ++ show idx)
             x:_ -> return x
           go p l t
        where
          go :: Variant a -> Label -> Parser a
          go V l _t = fail ("Invalid label " ++ show l)
          go ((l,g) :|: ps) l' t | l == l' = Left <$> parse g t
                                 | otherwise = Right <$> go ps l' t
      parseF _ = fail "Non-matching type, variant expected"

      writeF (TVariant fs) = execWriterT . go p
        where
          go :: Variant a -> a -> WriterT B.Builder (Either String) ()
          go V _ = error "Void"
          go ((l,g) :|: _) (Left a) =
            case lookup l (zipWith (\i (x,y) -> (x,(i,y))) [0::Int ..] fs) of
              Nothing -> fail ("Invalid label " ++ show l)
              Just (i,t) ->
                do tell $ B.word8 (fromIntegral i)
                   writeW g t a
          go (_ :|: ps) (Right b) = go ps b
      writeF _ = fail "Non-matching type, variant expected"

      def :: Variant a -> [(Label, Type)]
      def V = []
      def ((l,g) :|: xs) = (l, defaultType g) : def xs

class GenericGrammar (rep :: * -> *) where
    repGrammar :: Grammar (rep x)

instance GenericGrammar V1 where
    repGrammar = invmap (\_ -> error "Void") (\_ -> error "Void") void

instance GenericGrammar U1 where
    repGrammar = invmap (\() -> U1) (\U1 -> ()) unit

-- instance GenericGrammar 

gGrammar :: (Generic a, GenericGrammar (Rep a)) => Grammar a
gGrammar = isomap (from generic) repGrammar

{-
leaf :: Int -> Get (IndexTree l)
leaf n = skip n >> return (Leaf n)

leafp :: Get a -> Get (IndexTree l)
leafp p = do n1 <- bytesRead
             _ <- p
             n2 <- bytesRead
             return (Leaf (fromIntegral $ n2-n1))

parseIndex :: Parser (IndexTree Label)
parseIndex (TTuple fs)
    = do us <- mapM parseIndex (fieldTypes fs)
         let ix = makeIndex (map size us)
         return (Node (map fst fs) ix (V.fromList us))
parseIndex (TVariant fs)
    = do i <- fromIntegral <$> getWord8
         (f,t) <- case drop i fs of
            [] -> fail ("Invalid type index " ++ show i)
            fi:_ -> return fi
         ix <- parseIndex t
         return (Node [f] (makeIndex [size ix]) (V.singleton ix))
parseIndex (TVector t qn)
    = do n <- case qn of
                Nothing -> parseVarUInt
                Just n0 -> return n0
         us <- V.replicateM n (parseIndex t)
         let ix = case sizeOf t of
                    Constant m -> Fixed (V.length us) m
                    _ -> makeIndex (V.toList $ V.map size us)
         return (Node [] ix us)
parseIndex TVoid = fail "Uninhabited type"
parseIndex TUnit = return (Leaf 0)
parseIndex TBool = leaf 1
parseIndex t@(TInt _) = leafp (parse int t :: Get Int)
parseIndex t@(TUInt _) = leafp (parse int t :: Get Int)
parseIndex TFloat32 = leaf 4
parseIndex TFloat64 = leaf 8
parseIndex TChar = leafp (parse char TChar)
-}
