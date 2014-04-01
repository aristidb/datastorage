{-# LANGUAGE OverloadedStrings, BangPatterns, ViewPatterns, RecordWildCards, GADTs, RankNTypes, TupleSections, KindSignatures, TypeFamilies, FlexibleInstances, UndecidableInstances, FlexibleContexts, ScopedTypeVariables, TypeOperators, DefaultSignatures, DeriveGeneric, MultiParamTypeClasses #-}
module TypedBinary
{-
(
  Void
, Label(..)
, Type(..)
)
-}
where

-- import IndexTree
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.ByteString.Lazy.Builder.ASCII as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
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
import qualified Data.Binary
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Test.SmallCheck.Series as SC

data Void

instance Show Void where
    show _ = error "Void"

newtype Label = L { getLabel :: String }
    deriving (Eq, Show, Generic)

data Type =
    TVoid |
    TUnit |
    TBool |
    TInt { nbytes :: Maybe Int } |
    TUInt { nbytes :: Maybe Int } |
    TFloat32 |
    TFloat64 |
    TChar |
    TTuple [(Label, Type)] |
    TVariant [(Label, Type)] |
    TVector (Maybe Int) Type
  deriving (Eq, Show, Generic)

fieldTypes :: [(Label, Type)] -> [Type]
fieldTypes = map snd

typeString :: Type -> B.ByteString
typeString = L.toStrict . B.toLazyByteString . typeBuilder

typeBuilder :: Type -> B.Builder
typeBuilder TVoid = B.string7 "void"
typeBuilder TUnit = B.string7 "unit"
typeBuilder TBool = B.string7 "bool"
typeBuilder (TInt l) = B.string7 "int" <> maybe mempty B.intDec l
typeBuilder (TUInt l) = B.string7 "uint" <> maybe mempty B.intDec l
typeBuilder TFloat32 = B.string7 "float32"
typeBuilder TFloat64 = B.string7 "float64"
typeBuilder TChar = B.string7 "char"
typeBuilder (TTuple fs) = fieldsBuilder '{' '}' fs
typeBuilder (TVariant cs) = fieldsBuilder '[' ']' cs
typeBuilder (TVector n t) = B.string7 "vec " <> maybe mempty ((<> B.char7 ' ') . B.intDec) n <> typeBuilder t

fieldsBuilder :: Char -> Char -> [(Label, Type)] -> B.Builder
fieldsBuilder o c xs = B.char7 o <> innerBuilder <> B.char7 c
    where innerBuilder = mconcat (intersperse (B.char7 ';') (map fieldBuilder xs))
          fieldBuilder (L l, t) = B.string7 l <> B.char7 ' ' <> typeBuilder t

typeParser :: A.Parser Type
typeParser =
    TVoid <$ A.string "void" <|>
    TUnit <$ A.string "unit" <|>
    TBool <$ A.string "bool" <|>
    TInt <$> (A.string "int" *> optional A8.decimal) <|>
    TUInt <$> (A.string "uint" *> optional A8.decimal) <|>
    TFloat32 <$ A.string "float32" <|>
    TFloat64 <$ A.string "float64" <|>
    TChar <$ A.string "char" <|>
    TTuple <$> fieldsParser '{' '}' <|>
    TVariant <$> fieldsParser '[' ']' <|>
    A.string "vec " *> (TVector <$> optional (A8.decimal <* A8.char ' ') <*> typeParser)
    A.<?> "type"

fieldsParser :: Char -> Char -> A.Parser [(Label, Type)]
fieldsParser o c = A8.char o *> innerParser <* A8.char c
    where innerParser = fieldParser `A.sepBy` (A8.char ';')
          fieldParser = do l <- some (A8.satisfy (A8.notInClass " ;{}[]")) A.<?> "label"
                           _ <- A8.char ' '
                           t <- typeParser
                           return (L l, t)

getType :: Get Type
getType =
    do str <- fmap L.toStrict getLazyByteStringNul
       case A.parseOnly (typeParser <* A.endOfInput) str of
        Left e -> fail e
        Right x -> return x

instance Monad m => SC.Serial m Label where
    series = L . SC.getNonEmpty <$> SC.series

instance Monad m => SC.Serial m Type where
    series = SC.cons0 TVoid SC.\/
             SC.cons0 TUnit SC.\/
             SC.cons0 TBool SC.\/
             SC.cons1 (TInt . fmap SC.getPositive) SC.\/
             SC.cons1 (TUInt . fmap SC.getPositive) SC.\/
             SC.cons0 TFloat32 SC.\/
             SC.cons0 TFloat64 SC.\/
             SC.cons0 TChar SC.\/
             SC.cons1 TTuple SC.\/
             SC.cons1 TVariant SC.\/
             SC.cons2 (TVector . fmap SC.getPositive)

prop_typeDescription :: Type -> Bool
prop_typeDescription t = A.parseOnly typeParser (typeString t) == Right t

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
sizeOf (TVariant (fieldTypes -> [t])) = sizeOf t
sizeOf (TVariant (fieldTypes -> ts)) = addSize (Constant 1) $ foldl' maxSize (Constant 0) (map sizeOf ts)
sizeOf (TVector Nothing _) = addSize (sizeOf (TUInt Nothing)) (Range 0 Nothing)
sizeOf (TVector (Just n) et) = multSize n (sizeOf et)

isScalar :: Type -> Bool
isScalar (TTuple _) = False
isScalar (TVariant _) = False
isScalar (TVector _ _) = False
isScalar _ = True

type Parser a = Type -> Get a

type Generator a = Type -> a -> Either String B.Builder

data Grammar a = Grammar { parse :: Parser a, write :: Generator a, defaultType :: Type }

parseFull :: Grammar a -> Get a
parseFull g = getType >>= parse g

writeFull :: Grammar a -> Generator a
writeFull g t a = ((typeBuilder t <> B.word8 0) <>) <$> write g t a

writeDefault :: Grammar a -> a -> Either String B.Builder
writeDefault g = writeFull g (defaultType g)

simpleWrite :: Grammar a -> a -> Either String L.ByteString
simpleWrite g x = B.toLazyByteString <$> writeDefault g x

writeW :: Grammar a -> Type -> a -> WriterT B.Builder (Either String) ()
writeW g t x = do a <- lift (write g t x)
                  tell a

instance Invariant Grammar where
    invmap f g (Grammar p w dt) = Grammar (fmap f . p) ((. g) . w) dt

isomap :: Iso' a b -> Grammar a -> Grammar b
isomap m = invmap (view m) (review m)

class Grammatical a where
    grammar :: Grammar a
    default grammar :: (Generic a, GenericGrammar (Rep a)) => Grammar a
    grammar = gGrammar

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

char :: Grammar Char
char = simpleTyped TChar parseF writeF
    where
      parseF = Data.Binary.get -- UTF-8
      writeF = B.charUtf8

instance Grammatical Char where grammar = char

vector :: G.Vector v a => Grammar a -> Grammar (v a)
vector inner = Grammar { parse = parseF, write = writeF, defaultType = def }
    where
      parseF (TVector qn t) =
        do n <- case qn of
                  Just n0 -> return n0
                  Nothing -> parseVarUInt
           G.replicateM n (parse inner t)
      parseF _ = fail "Non-matching type"
      writeF (TVector Nothing t) v = execWriterT $
        do tell (encodeVarUInt (G.length v))
           G.forM_ v (writeW inner t)
      writeF (TVector (Just n) t) v | n == G.length v = execWriterT $ G.forM_ v (writeW inner t)
                                    | otherwise = Left "Non-matching length"
      writeF _ _ = Left "Non-matching type"
      def = TVector Nothing (defaultType inner)

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

data Tuple a where
    TPure :: a -> Tuple a
    TLabelled :: Label -> Grammar a -> Tuple a
    TPlus :: Tuple a -> Tuple b -> Tuple (a, b)

parseStep :: Label -> Tuple a -> Parser (Bool, Tuple a)
parseStep l x@(TLabelled l' g) t | l == l' = (True,) . TPure <$> parse g t
                                 | otherwise = return (False, x)
parseStep _ x@(TPure _) _ = return (False, x)
parseStep l (TPlus x y) t =
    do (s,a) <- parseStep l x t
       if s
         then return (True, TPlus a y)
         else do (s',b) <- parseStep l y t
                 return (s', TPlus a b)

extract :: Tuple a -> Maybe a
extract (TPure a) = Just a
extract (TLabelled _ _) = Nothing
extract (TPlus x y) = (,) <$> extract x <*> extract y

buildStep :: Tuple a -> Label -> Generator a
buildStep (TLabelled l' g) l t c | l == l' = write g t c
                                 | otherwise = Left "Label not found"
buildStep (TPure _) _ _ _ = Left "Label not found"
buildStep (TPlus x y) l t (a, b) = buildStep x l t a <|> buildStep y l t b

tuple :: Tuple a -> Grammar a
tuple p = Grammar { parse = parseF, write = writeF, defaultType = defType p }
    where
      parseF (TTuple fs) =
        do p' <- go p fs
           case extract p' of
             Just a -> return a
             Nothing -> fail "Not all fields could be parsed"
        where
          go px [] = return px
          go px ((l,t) : xs) = do (s, px') <- parseStep l px t
                                  if s
                                    then go px' xs
                                    else fail "Label not found"
      parseF t = case p of
                   TLabelled _l g -> parse g t
                   _ -> fail "Non-matching type, tuple expected"

      writeF (TTuple fs) a = execWriterT (go fs)
        where
          go [] = return ()
          go ((l,t) : xs) = do tell =<< lift (buildStep p l t a)
                               go xs
      writeF t a = case p of
                     TLabelled _l g -> write g t a
                     _ -> fail "Non-matching type, tuple expected"

      defType :: Tuple a -> Type
      defType (TLabelled (L "0") g) = defaultType g
      defType _ = TTuple (def p)

      def :: Tuple a -> [(Label, Type)]
      def (TPure _) = []
      def (TLabelled l g) = [(l, defaultType g)]
      def (TPlus x y) = def x ++ def y

data Variant a where
    VOne :: Label -> Grammar a -> Variant a
    VPlus :: Variant a -> Variant b -> Variant (Either a b)

variant :: Variant a -> Grammar a
variant p = Grammar { parse = parseF, write = writeF, defaultType = TVariant (def p) }
    where
      parseByLabel :: Variant a -> Label -> Parser a
      parseByLabel (VOne l g) l' t | l == l' = parse g t
                                   | otherwise = fail "Invalid label"
      parseByLabel (VPlus a b) l' t = Left <$> parseByLabel a l' t <|> Right <$> parseByLabel b l' t

      parseF (TVariant [(l,t)]) = parseByLabel p l t
      parseF (TVariant fs) =
        do idx <- fromIntegral <$> getWord8
           (l,t) <- case drop idx fs of
             [] -> fail ("Invalid tag index " ++ show idx)
             x:_ -> return x
           parseByLabel p l t
      parseF t = case p of
                   -- special-case for single constructor variants
                   VOne _l g -> parse g t
                   _ -> fail "Non-matching type, variant expected"

      writeF (TVariant [(l,t)]) d = go p d
        where
          go :: Variant a -> a -> Either String B.Builder
          go (VOne l' g) a | l == l' = write g t a
                           | otherwise = Left ("Invalid label " ++ show l')
          go (VPlus x _) (Left a) = go x a
          go (VPlus _ y) (Right b) = go y b
      writeF (TVariant fs) d = execWriterT (go p d)
        where
          go :: Variant a -> a -> WriterT B.Builder (Either String) ()
          go (VOne l g) a =
            case lookup l (zipWith (\i (x,y) -> (x,(i,y))) [0::Int ..] fs) of
              Nothing -> fail ("Invalid label " ++ show l)
              Just (i,t) ->
                do tell $ B.word8 (fromIntegral i)
                   writeW g t a
          go (VPlus x _) (Left a) = go x a
          go (VPlus _ y) (Right b) = go y b
      writeF t d = case p of
                     -- special-case for single constructor variants
                     VOne _l g -> write g t d
                     _ -> fail "Non-matching type, variant expected"

      def :: Variant a -> [(Label, Type)]
      def (VOne l g) = [(l, defaultType g)]
      def (VPlus a b) = def a ++ def b

class GenericGrammar (rep :: * -> *) where
    repGrammar :: Grammar (rep ())

instance GenericGrammar V1 where
    repGrammar = invmap (\_ -> error "Void") (\_ -> error "Void") void

instance GenericGrammar U1 where
    repGrammar = invmap (\() -> U1) (\U1 -> ()) unit

instance Grammatical c => GenericGrammar (K1 R c) where
    repGrammar = invmap K1 unK1 grammar

selectorLabel :: forall (t :: * -> (* -> *) -> * -> *) s f a. Selector s => Int -> t s f a -> Label
selectorLabel i t =
    case selName t of
        "" -> L (show i)
        s -> L s

class TupleGrammar (rep :: * -> *) where
    type TupleT rep :: *
    tfrom :: rep () -> TupleT rep
    tto :: TupleT rep -> rep ()
    tup :: rep () -> Int -> (Int, Tuple (TupleT rep))

instance TupleGrammar U1 where
    type TupleT U1 = ()
    tfrom U1 = ()
    tto () = U1
    tup _ i = (i, TPure ())

instance (Selector s, GenericGrammar f) => TupleGrammar (M1 S s f) where
    type TupleT (M1 S s f) = f ()
    tfrom (M1 x) = x
    tto x = M1 x
    tup _ i = (i + 1, TLabelled (selectorLabel i (undefined :: M1 S s f ())) repGrammar)

instance (TupleGrammar a, TupleGrammar b) => TupleGrammar (a :*: b) where
    type TupleT (a :*: b) = (TupleT a, TupleT b)
    tfrom (x :*: y) = (tfrom x, tfrom y)
    tto (x, y) = (tto x :*: tto y)
    tup _ i = (i'', TPlus x y)
      where
        (i', x) = tup (undefined :: a ()) i
        (i'', y) = tup (undefined :: b ()) i'

class VariantGrammar (rep :: * -> *) where
    type VarT rep :: *
    vfrom :: rep () -> VarT rep
    vto :: VarT rep -> rep ()
    var :: rep () -> Variant (VarT rep)

instance (Constructor c, TupleGrammar f) => VariantGrammar (M1 C c f) where
    type VarT (M1 C c f) = TupleT f
    vfrom (M1 x) = tfrom x
    vto x = M1 (tto x)
    var _ = VOne (L (conName (undefined :: M1 C c f ()))) (tuple (snd (tup (undefined :: f ()) 0)))

instance (VariantGrammar a, VariantGrammar b) => VariantGrammar (a :+: b) where
    type VarT (a :+: b) = Either (VarT a) (VarT b)

    vfrom (L1 x) = Left (vfrom x)
    vfrom (R1 x) = Right (vfrom x)

    vto (Left x) = L1 (vto x)
    vto (Right x) = R1 (vto x)

    var _ = VPlus (var (undefined :: a ())) (var (undefined :: b ()))

instance VariantGrammar f => GenericGrammar (M1 D c f) where
    repGrammar = invmap M1 unM1 $ invmap vto vfrom $ variant $ var (undefined :: f ())

gGrammar :: (Generic a, GenericGrammar (Rep a)) => Grammar a
gGrammar = isomap (from generic) repGrammar

instance (Grammatical a, Grammatical b) => Grammatical (a, b)
instance (Grammatical a, Grammatical b, Grammatical c) => Grammatical (a, b, c)
instance (Grammatical a, Grammatical b, Grammatical c, Grammatical d) => Grammatical (a, b, c, d)
instance Grammatical a => Grammatical (Maybe a)
instance (Grammatical a, Grammatical b) => Grammatical (Either a b)

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
