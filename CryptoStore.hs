module CryptoStore where

import BlobStore
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString as B
import Data.Byteable
import Control.Applicative

data Key = Key { addressKey :: B.ByteString, valueAes :: AES.AES }

cryptoStore :: Functor f => Key -> RawStore f -> RawStore f
cryptoStore k st = Store { store = doStore, load = doLoad }
    where doStore x@(Decorated a _) = fmap (const a) <$> store st (Decorated (newAddress k a) (encrypt k x))
          doLoad a = fmap helper <$> load st (newAddress k a)
            where helper (Decorated _ o) = Decorated a (decrypt k a o)

-- TODO: consider whether SHA512Key is appropriate for the output
newAddress :: Key -> Address -> Address
newAddress k (SHA512Key a) = SHA512Key . SHA512.hash $ addressKey k `B.append` a

encrypt :: Key -> Decorated -> B.ByteString
encrypt k (Decorated a o) = AES.encryptCBC (valueAes k) (toBytes a) (pad o)

decrypt :: Key -> Address -> B.ByteString -> B.ByteString
decrypt k a o = unpad $ AES.decryptCBC (valueAes k) (toBytes a) o

pad :: B.ByteString -> B.ByteString
pad x = B.concat [B.replicate padLength 0, B.singleton 1, x]
    where padLength = 16 - (B.length x + 1) `rem` 16

unpad :: B.ByteString -> B.ByteString
unpad = B.drop 1 . B.dropWhile (== 0)
