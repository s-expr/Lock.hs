module Hardware.Protocol.Util where

type Byte = Word8
type Addr = Word8


serial :: Binary a => a -> ByteString
serial =  toStrict . encode

deserial :: Binary a => ByteString -> a
deserial = decode . fromStrict 

decodeBytes :: IO ByteString -> IO a
decodeBytes bytesio = bytesio >>= return . deserial 

