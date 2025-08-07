module Main where
import Prelude hiding (read)
import Control.Exception
import Debug.Trace
import Hardware.Protocol.I2C 
import System.RaspberryPi.GPIO
import Data.Binary
import Data.ByteString 
{-
class Lock a where
  getName :: a -> String
  getStatus :: a -> LockStatus
  tryLock :: Exception e => a -> IO (Either e LockStatus)
  tryUnlock :: Exception e => a -> IO (Either e LockStatus)



  
bclk :: Int
bclk = 19200000

ticks :: Int
ticks = 1024

clk :: Int
clk = 50

servoTest :: Angle -> Angle -> IO ()
servoTest init end = withGPIO $ do
  servo <- initServo config gpio 
  setAngle servo 0
  mapM ( (setAngle servo) . fromIntegral ) [0,30..180]
  return ()
  where 
    config = ServoConfig (0, 180.0) (2, 7.4) clk
    gpio = GPIOConfig Pin12 Alt5 C0 bclk ticks

main :: IO ()
main = servoTest 0 180
-}

byte = fromIntegral
bytes xs = byte <$> xs


disp :: I2CDev
disp = I2CDev  (byte 0x3D)  "oled"

off :: I2CDev ->  IO ()
off d = write d $ byte 0xAE
 
on :: I2CDev ->  IO ()
on d = write d $ byte 0xAF

reset :: I2CDev -> IO ()
reset = write d $ 0x7F




activate :: I2CDev -> IO () 
activate d = do
  return ()
  where
    read'  = read d
    write' :: Binary a => a -> IO ()
    write' = write d
    rw' :: Binary a => a -> Int -> IO ByteString
    rw'    = rw d

deactivate :: I2CDev -> IO ()
deactivate d = do
  return ()
  where
    read'  = read d
    write' :: Binary a => a -> IO ()
    write' = write d
    rw' :: Binary a => a -> Int -> IO ByteString
    rw'    = rw d

main :: IO ()
main = activate disp >> deactivate disp

  
