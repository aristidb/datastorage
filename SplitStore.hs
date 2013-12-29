{-# LANGUAGE FlexibleContexts, ViewPatterns, DeriveDataTypeable #-}

import BlobStore
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Attoparsec.ByteString as A
-- import qualified Data.Attoparsec.ByteString.Lazy as AL
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Attoparsec as PA
import Control.Applicative
import Data.Monoid
import Data.Foldable (foldMap)
import Control.Monad.Catch
import Control.Monad
import Data.Typeable
import Control.Monad.State.Strict

data ObjectTag = PlainData | Chain ObjectTag
    deriving (Show)

tagBuilder :: ObjectTag -> Builder.Builder
tagBuilder PlainData = Builder.string7 "plain:"
tagBuilder (Chain t) = Builder.string7 "chain:" <> tagBuilder t

tagParser :: A.Parser ObjectTag
tagParser = (PlainData <$ A.string (B8.pack "plain:"))
            <|> (Chain <$> (A.string (B8.pack "chain:") *> tagParser))

data TaggedObject = TaggedObject ObjectTag B.ByteString
    deriving (Show)

taggedBuilder :: TaggedObject -> Builder.Builder
taggedBuilder (TaggedObject tag dat) = tagBuilder tag <> Builder.byteString dat

taggedParser :: A.Parser TaggedObject
taggedParser = TaggedObject <$> tagParser <*> A.takeByteString

taggedStore :: (Functor f, MonadCatch f) => Store f Address B.ByteString B.ByteString -> Store f Address TaggedObject TaggedObject
taggedStore = parserStore taggedBuilder taggedParser

data Chained f = Chained ObjectTag (Producer Address f ())

writeChained :: Monad f => Chained f -> Producer B.ByteString f ()
writeChained (Chained t p) = PB.fromLazy (Builder.toLazyByteString $ tagBuilder t) >> (p >-> P.map addressToByteString)

readChained :: MonadCatch f => Producer B.ByteString f () -> f (Chained f)
readChained p = do (r, p') <- runStateT (PA.parse tagParser) p
                   t <- case r of
                     Left (PA.ParsingError { PA.peMessage = msg }) -> throwM $ ParseError msg
                     Right (_, t) -> return t
                   return $ Chained t $ do r <- PA.parseMany addressParse p' >-> P.map snd
                                           case r of
                                             Left (pe, _) -> lift $ throwM $ ParseError $ PA.peMessage pe
                                             Right () -> return ()

{-
data Representation f =
    Embedded B.ByteString |
    ObjectList (Producer Address f ())

writeRepresentation :: Monad f => Representation f -> Producer B.ByteString f ()
writeRepresentation (Embedded x) = yield (B.singleton 0x5A) >> yield x
writeRepresentation (ObjectList xs) = yield (B.singleton 0x7C) >> (xs >-> P.map addressToByteString)

readRepresentation :: (Functor f, MonadCatch f) => Producer B.ByteString f () => f (Representation f)
readRepresentation p = do x <- PB.nextByte p
                          case x of
                            Left () -> throwM (ParseError "Invalid empty representation")
                            Right (0x5A, p') -> Embedded . L.toStrict <$> PB.toLazyM p'
                            Right (0x7C, p') -> return (ObjectList pa)
                                where pa = do r <- PA.parseMany addressParse p' >-> P.map snd
                                              case r of
                                                Left (pe, _) -> lift $ throwM (ParseError $ PA.peMessage pe)
                                                Right () -> return ()
                            Right (_b, _p) -> throwM (ParseError "Unexpected initial representation byte")

-- problem:
-- ObjectList gets a 0x5A prefix too because its parts will be stored as Embeddeds too, recursively
-- idea:
-- tagged stores, so the recursion can keep the object type
-- -}

{-
data Representation =
    Embedded B.ByteString |
    ObjectList [Address]
  deriving (Show)

representationBuilder :: Representation -> Builder.Builder
representationBuilder (Embedded x) = Builder.word8 0 <> Builder.byteString x
representationBuilder (ObjectList as) = Builder.word8 1 <> foldMap addressBuilder as

representationParser :: A.Parser Representation
representationParser = Embedded <$> (A.word8 0 *> A.takeByteString) <|>
                       ObjectList <$> (A.word8 1 *> many addressParse)

data SplitStore f = SplitStore {
      representationStore :: Store f Address Representation Representation
    , producerStore :: Store f Address (Producer B.ByteString f ()) (Producer B.ByteString f ())
    }

splitStream :: (Functor f, MonadCatch f) => (Producer B.ByteString f () -> Producer B.ByteString f ()) -> Store f Address B.ByteString B.ByteString -> SplitStore f
splitStream splitter st = SplitStore rs ps
    where rs = parserStore representationBuilder representationParser st
          ps = streamStore splitter rs

streamStore :: (Functor f, MonadCatch f) => (Producer B.ByteString f () -> Producer B.ByteString f ()) -> Store f Address Representation Representation -> Store f Address (Producer B.ByteString f ()) (Producer B.ByteString f ())
streamStore splitter st = Store { store = doStore, load = \a -> makeProducer <$> load st a }
    where doStore (splitter -> p) = do xs <- P.toListM $ for p $ \o -> yield =<< lift (store st (Embedded o))
                                       case xs of
                                         [x] -> return x
                                         _ -> store st (ObjectList xs)

          makeProducer (Embedded o) = yield o
          makeProducer (ObjectList xs) = mapM_ (makeProducer <=< lift . load st) xs
-}
