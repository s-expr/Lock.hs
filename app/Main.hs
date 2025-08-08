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


main :: IO ()
main = return ()

  
