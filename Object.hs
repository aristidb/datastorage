import qualified Data.ByteString as S

data Object = Data S.ByteString | ObjectList [Object]
