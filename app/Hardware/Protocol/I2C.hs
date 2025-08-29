{-#LANGUAGE RecordWildCards #-}
module Hardware.Protocol.I2C
  ( I2CDev(..)
  , I2CProg
  , write
  , wait
  , read
  , readByte
  , rw
  , runI2C
  , liftIO
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Free
import System.RaspberryPi.GPIO
import Data.Word
import Data.Binary 
import Data.ByteString
import Hardware.Protocol.Util
import Debug.Trace

data I2CDev = I2CDev
  { addr :: Addr
  , name :: String
  }

--may be smart to make indexed by a device type I2CProg d a
--- instance (I2CDevice dev) => I2CProgF dev next
--- newtype I2CProgF dev next = I2CProgF {runProgF :: dev -> IO next} ???
--- class I2CDevice d where
---   addr :: d -> Byte
---   name :: d -> string
--
--this could afford guarantees that programs cannot run on the wrong device

type I2CProg a = Free I2CProgF a
newtype I2CProgF next = I2CProgF {runProgF :: I2CDev -> IO next}

instance Functor I2CProgF where 
  fmap f (I2CProgF p) = I2CProgF (fmap f . p)
  
 
mkProg :: (I2CDev -> IO a) -> I2CProg a
mkProg = liftF . I2CProgF 

liftIO :: IO a -> I2CProg a
liftIO = mkProg . const

getDev :: I2CProg I2CDev
getDev = mkProg $ return

--Time is in ms
wait :: Int -> I2CProg ()
wait ms = liftIO $ threadDelay (ms*1000)

{-
write :: Binary a => a -> I2CProg ()
write a = mkProg $ flip writeI2C (serial a) . addr
-}

write :: Binary a => a -> I2CProg ()
write a = do
  let bs = serial a
  liftIO $ dumpBS bs
  dev <- getDev
  liftIO $ flip writeI2C bs (addr dev)

readByte :: I2CProg ByteString
readByte = mkProg $ flip readI2C 1 . addr

readBytes :: Int -> I2CProg ByteString
readBytes numBytes = mkProg $ flip readI2C numBytes . addr
  
rw ::  Binary a => a -> Int -> I2CProg ByteString
rw a bytes = mkProg $ \dev -> 
  writeReadRSI2C (addr dev) (serial a) bytes 

runI2C :: I2CDev -> I2CProg a -> IO a
runI2C dev =  withGPIO . withI2C . (iterM $ step dev)
  where
  step dev (I2CProgF p) = do 
    result <- p dev  
    result