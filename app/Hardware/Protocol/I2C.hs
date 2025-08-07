{-#LANGUAGE RecordWildCards #-}
module Hardware.Protocol.I2C where

import Control.Concurrent
import System.RaspberryPi.GPIO
import Data.Word
import Data.ByteString 
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Binary 


type Byte = Word8
type Addr = Word8

data I2CDev = I2CDev
  { addr :: Addr
  , name :: String
  }

serial :: Binary a => a -> ByteString
serial =  toStrict . encode

deserial :: Binary a => ByteString -> a
deserial = decode . fromStrict 

decodeBytes :: IO ByteString -> IO ByteString
decodeBytes bytesio = bytesio >>= return . deserial 

write :: Binary a => I2CDev -> a -> IO ()
write I2CDev{..} a = writeI2C addr $ serial a

read :: I2CDev -> Int -> IO ByteString
read I2CDev{..} c = decodeBytes $ readI2C addr c
  
rw ::  Binary a => I2CDev -> a -> Int -> IO ByteString
rw I2CDev{..} input bytes =
  decodeBytes $ writeReadRSI2C addr (serial input) bytes 


{-
exec :: Command -> IO b
exec c = case c of
  Write d toWrite -> writeI2C addr' toWrite >> return ()
  Read d c -> decodeBytes $ readI2C addr' c 
  WriteRead d toWrite c -> decodeBytes $ writeReadRSI2C addr' c
  where 
    addr' = addr d
    decodeBytes bytesio = do
      bytes <- bytesio
      return $ decode $ fromStrict bytes
-}
{-    
--rewrite using fold
execI2C :: Binary a => [Command a] -> IO [ByteString]
execI2C [] = return []
execI2C (cmd : xs) = process cmd
  where
    appendRead bs = do
      bs1 <- bs
      bs2 <- runI2C dev xs
      bs1 : bs2
    addr' = addr dev
    process Write(dev, a, toWrite) = writeI2C addr' args >> runI2C dev xs
    process Read(dev, int, args) = appendRead $ readI2C addr' args 
    process WriteRead(dev, cmd, args, amt) = appendRead $ writeReadRSI2C addr' args amt
    where
      addr' = addr dev
-}
