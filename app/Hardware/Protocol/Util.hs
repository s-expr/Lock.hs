module Hardware.Protocol.Util where

import Data.Binary
import Data.Binary.Put (putByteString)
import Debug.Trace
import Data.ByteString 
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict, fromStrict)
import Numeric (showHex)

type Byte = Word8
type Addr = Word8

newtype MsgList a = MsgList [a]
instance (Binary a) => Binary (MsgList a) where
  put (MsgList xs) = putByteString $ B.concat $ Prelude.map serial xs
  get = get

serial :: Binary a => a -> ByteString
serial = toStrict . encode

deserial :: Binary a => ByteString -> a
deserial = decode . fromStrict 

decodeBytes :: Binary a => IO ByteString -> IO a
decodeBytes bytesio = bytesio >>= return . deserial 

dumpBS :: ByteString -> IO ()
dumpBS bs = Prelude.putStrLn . unwords $
  [  showHex w "" 
  | w <- unpack bs
  ]
 