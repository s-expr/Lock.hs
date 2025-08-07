module Devices.Adapters.ServoLock where

import Derices.Lock
import Hardware.Actuators.Servo

data ServoLock = ServoLock
  { servo :: Servo
  , unlockAngle :: Angle
  , lockAngle :: Angle
  , status :: LockStatus
  }

mkServoLock :: Servo -> Angle -> Angle -> ServoLock
mkServoLock = ServoLock

instance Lock ServoLock IO where
  unlock s = setAngle ( servo s ) unlockAngle
  lock s = setAngle ( servo s ) lockAngle
  status s = let ang = servoCurAngle . servo $ s
    in if ang == lockAngle s
    then Locked
    else Unlocked

