{-# LANGUAGE ConstraintKinds, ViewPatterns, FlexibleContexts, DeriveDataTypeable #-}
module CryptoStore (Key(..), cryptoStore) where

import BlobStore
import Crypto.Hash
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Byteable
import Control.Applicative
import Control.Exception
import Data.Typeable
import Control.Monad.Catch

-- The crypto here is not safe!

data Key = Key { addressKey :: B.ByteString, valueAes :: AES.AES }

newtype SecretHash = SecretHashSHA512 (HMAC SHA512)
    deriving (Eq)

instance Byteable SecretHash where
    toBytes (SecretHashSHA512 x) = toBytes x

data InvalidObjectSize = InvalidObjectSize
  deriving (Show, Typeable)

instance Exception InvalidObjectSize

cryptoStore :: (Functor f, MonadCatch f) => Key -> Store f SecretHash (SecretHash, L.ByteString) -> Store f SecretHash L.ByteString
cryptoStore k st = Store { store = doStore, load = doLoad }
    where doStore o = a <$ store st (newAddress k a) (encrypt k a o)
            where a = SecretHashSHA512 (hmac (addressKey k) o)
          doLoad a = helper =<< load st a
            where helper o | B.length o `rem` 16 == 0 = return (decrypt k a o)
                           | otherwise                = throwM InvalidObjectSize

newAddress :: Key -> Address -> SecretHash
newAddress k (SHA512Key a) = SecretHashSHA512 . SHA512.hash $ addressKey k `B.append` a

encrypt :: Key -> Address -> L.ByteString -> B.ByteString
encrypt k a o = AES.encryptCBC (valueAes k) (toBytes a) (pad o)

decrypt :: Key -> Address -> B.ByteString -> B.ByteString
decrypt k a o = unpad $ AES.decryptCBC (valueAes k) (toBytes a) o

pad :: B.ByteString -> B.ByteString
pad x = B.concat [B.replicate padLength 0, B.singleton 1, x]
    where padLength = 16 - (B.length x + 1) `rem` 16

unpad :: B.ByteString -> B.ByteString
unpad = B.drop 1 . B.dropWhile (== 0)
