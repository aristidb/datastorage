{-# LANGUAGE FlexibleContexts, ViewPatterns, BangPatterns, GeneralizedNewtypeDeriving, DeriveGeneric #-}

import TypedBinary
import BlobStore
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Pipes
import qualified Pipes.ByteString as PB
import Data.Monoid
import Control.Monad.Trans.Free
import GHC.Generics

data PrimaryObject =
    Embedded B.ByteString |
    Chain [Address]
  deriving (Show, Generic)

instance Grammatical PrimaryObject

data SplitStore f = SplitStore {
      addressStore :: Store' f Address (Producer Address f ())
    , producerStore :: Store' f Address (Producer B.ByteString f ())
    }


mkSplitter :: Monad m => (Producer B.ByteString m () -> PB.FreeT (Producer B.ByteString m) m ()) -> (Producer B.ByteString m () -> Producer B.ByteString m ())
mkSplitter f p = go (f p)
    where go x = do r <- lift $ runFreeT x
                    case r of
                      Pure () -> return ()
                      Free p' -> do (v, x') <- lift $ consume p' L.empty
                                    yield v
                                    go x'
          consume px !acc = do r <- next px
                               case r of
                                 Left s -> return (L.toStrict acc, s)
                                 Right (v, p') -> consume p' (acc <> L.fromStrict v)
