module CryptoStore where

import BlobStore
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Byteable
import Control.Applicative

data Key = Key { addressKey :: B.ByteString, valueAes :: AES.AES }

cryptoStore :: Functor f => Key -> RawStore f -> RawStore f
cryptoStore k st = Store { store = doStore, load = doLoad }
    where doStore x@(Decorated a _) = store st (Decorated (newAddress k a) (encrypt k x))
          doLoad a = fmap helper <$> load st (newAddress k a)
            where helper (Decorated _ o) = Decorated a (decrypt k a o)

-- TODO: consider whether SHA512Key is appropriate for the output
newAddress :: Key -> Address -> Address
newAddress k (SHA512Key a) = SHA512Key . SHA512.hash $ addressKey k `B.append` a

encrypt :: Key -> Decorated -> L.ByteString
encrypt k (Decorated a o) = L.fromStrict $ AES.encryptCBC (valueAes k) (toBytes a) (L.toStrict $ pad o)

decrypt :: Key -> Address -> L.ByteString -> L.ByteString
decrypt k a o = unpad . L.fromStrict $ AES.decryptCBC (valueAes k) (toBytes a) (L.toStrict o)

pad :: L.ByteString -> L.ByteString
pad x = L.replicate padLength 0 `L.append` L.singleton 1 `L.append` x
    where padLength = 16 - (L.length x + 1) `rem` 16

unpad :: L.ByteString -> L.ByteString
unpad = L.drop 1 . L.dropWhile (== 0)
