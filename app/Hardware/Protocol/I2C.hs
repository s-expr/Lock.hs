{-#LANGUAGE RecordWildCards #-}
module Hardware.Protocol.I2C
  ( I2CDev
  , write
  , read
  , readByte
  , rw
  )
where

import Control.Monad.Free
import System.RaspberryPi.GPIO
import Data.Word
import Data.ByteString 
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Binary 
import Util

data I2CDev = I2CDev
  { addr :: Addr
  , name :: String
  }

newtype I2CProgF next = I2CProgF {runProgF :: I2CDev -> IO next}
instance Functor I2CProgF where 
  fmap f (I2CProgF p) = I2CProgF (fmap f . p)
type I2CProg a = Free I2CProgF a

fmap Pure :: I2CProgF a -> I2CProgF (Free I2CProgF a) 

mkProg :: (I2CDev -> IO a) -> I2CProg a
mkProg = liftF . I2CProgF 

write :: Binary a => a -> I2CProg ()
write a = mkProg $ flip writeI2C (serial a) . addr

readByte :: I2CProg ByteString
readByte = mkProg $ flip readI2C 1 . addr

read :: Int -> I2CProg ByteString
read numBytes = mkProg $ flip readI2C numBytes . addr
  
rw ::  Binary a => a -> Int -> I2CProg ByteString
rw a bytes = 
  mkProg $ \dev -> 
    writeReadRSI2C (addr dev) (serial a) bytes 

runI2C :: I2CProg a -> I2CDev -> IO a
runI2C p dev =  iterM step
  where
  step (I2CProgF p) = p dev
