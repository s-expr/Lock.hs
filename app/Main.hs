module Main where
import Prelude hiding (read)
import Debug.Trace
import Codec.Picture
import Hardware.Peripherals.Display.SSD1306.I2C
import System.Posix.User
import System.RaspberryPi.GPIO
import Data.Binary
import Data.ByteString 
import Hardware.Protocol.Util
import Hardware.Protocol.I2C 

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

treshold :: (Byte, Byte, Byte) -> Bool
treshold (r, g, b)= 
  let thresh = 50 
  in r < thresh 
  && g < thresh
  && b < thresh

group :: [a] -> [(a,a,a)] -> [(a,a,a)]
group (a : b : c : xs) b = (a,b,c) : group xs
group _ b = []

tryLoad :: path ->  IO ()
tryLoad = do
  img <- readImage "./img.png"
  case img of
    Left -> return ()
    Right (ImageRGB8 (Image w h vec)) -> do
      let pixs = map treshold $ group $ toList vec 
      show pixs


main :: IO ()
main = runCmd dev $ do
  externalPower False
  wait 1000
  fullBright False
  wait 3000
  fullBright True
  wait 1000
  fullBright False
  sleep
  tryLoad
  where
    dev = newController






  
