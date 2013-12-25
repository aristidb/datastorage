{-# LANGUAGE ConstraintKinds, ViewPatterns, FlexibleContexts, DeriveDataTypeable #-}
module CryptoStore (Key(..), cryptoStore) where

import BlobStore
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import Data.Byteable
import Control.Applicative
import Control.Exception
import Data.Typeable
import Control.Monad.Catch

data Key = Key { addressKey :: B.ByteString, valueAes :: AES.AES }

newtype SecretHash = SecretHashSHA512 B.ByteString
    deriving (Eq)

instance Byteable SecretHash where
    toBytes (SecretHashSHA512 x) = x

data InvalidObjectSize = InvalidObjectSize
  deriving (Show, Typeable)

instance Exception InvalidObjectSize

cryptoStore :: (Functor f, MonadCatch f, Put Address i, Get f Address o) => Key -> Store f SecretHash (Decorated SecretHash) (Decorated SecretHash) -> Store f Address i o
cryptoStore k st = Store { store = doStore, load = doLoad }
    where doStore (decorate -> x@(Decorated a _)) = a <$ store st (Decorated (newAddress k a) (encrypt k x))
          doLoad a = helper =<< load st (newAddress k a)
            where helper (Decorated _ o) | B.length o `rem` 16 == 0 = unroll a (decrypt k a o)
                                         | otherwise                = throwM InvalidObjectSize

newAddress :: Key -> Address -> SecretHash
newAddress k (SHA512Key a) = SecretHashSHA512 . SHA512.hash $ addressKey k `B.append` a

encrypt :: Key -> Decorated Address -> B.ByteString
encrypt k (Decorated a o) = AES.encryptCBC (valueAes k) (toBytes a) (pad o)

decrypt :: Key -> Address -> B.ByteString -> B.ByteString
decrypt k a o = unpad $ AES.decryptCBC (valueAes k) (toBytes a) o

pad :: B.ByteString -> B.ByteString
pad x = B.concat [B.replicate padLength 0, B.singleton 1, x]
    where padLength = 16 - (B.length x + 1) `rem` 16

unpad :: B.ByteString -> B.ByteString
unpad = B.drop 1 . B.dropWhile (== 0)
