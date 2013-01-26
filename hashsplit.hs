import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Crypto.Hash
import           Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lens
import           Data.Char
import           Data.Word
import           Numeric.Lens
import           System.FilePath
import           System.Posix.Files
import           System.IO
--import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as L

objDir :: String
objDir = "obj"

base16 :: Prism' String S.ByteString
base16 = prism' (concatMap readByte . S.unpack) (fmap S.pack . showBytes)
  where readByte :: Word8 -> String
        readByte x = map (intToDigit . fromIntegral) [x `shiftR` 4, x .&. 0xF]

        showBytes :: String -> Maybe [Word8]
        showBytes [] = Just []
        showBytes (a:b:xs) = (fromIntegral (digitToInt a `shiftL` 4 .|. digitToInt b) :) <$> showBytes xs
        showBytes _ = Nothing

digestByteString :: Prism' S.ByteString (Digest SHA256)
digestByteString = prism' digestToByteString digestFromByteString

digestName :: Prism' String (Digest SHA256)
digestName = base16 . digestByteString

store :: L.ByteString -> IO (Digest SHA256)
store bs = do test <- fileExist path
              when (not test) $ do hPutStrLn stderr ("New object: " ++ objName)
                                   L.writeFile path bs
              return digest
  where
    digest :: Digest SHA256
    digest = hashlazy bs

    objName = review digestName digest
    path = objDir </> objName

load :: Digest SHA256 -> IO L.ByteString
load digest = L.readFile (objDir </> review digestName digest)
