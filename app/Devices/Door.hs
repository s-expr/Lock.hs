module Door where
import Control.Exception
import Servo 

data Door a b = Door
  { lock :: Lock a
  , ajarSensor :: Sensor b
  }

open :: DoorLock -> IO ()
open l = undefined 

close :: DoorLock -> IO ()
close l = undefined 

data LockStatus
  = Unlocked
  | Locked
  | Error 
  | Unknown
  deriving (Show)
 
