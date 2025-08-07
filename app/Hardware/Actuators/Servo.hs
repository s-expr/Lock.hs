{-# LANGUAGE RecordWildCards #-} 
module Hardware.Actuators.Servo
  ( GPIOConfig(GPIOConfig)
  , ServoConfig(ServoConfig)
  , Servo(..)
  , PWMChannel(..)
  , Angle
  , initServo
  , setAngle
  ) where

import Data.Word
import Control.Concurrent
import System.RaspberryPi.GPIO

-- TODO: Angle dumping and retrieval when closing Servo instances
-- TODO: keep track of servo angle state
type Ticks = Int
type Freq = Int
type Time = Double
type Angle = Double

data PWMChannel
  = C0
  | C1
  deriving (Show, Enum)

data ServoConfig = ServoConfig
  { scAngleRange :: (Angle, Angle)
  , scAnglePeriod  :: (Double, Double)
  , scClk :: Freq
  }

data GPIOConfig = GPIOConfig
  { cfgPin :: Pin
  , cfgPinMode :: PinMode
  , cfgChan :: PWMChannel
  , cfgBclk :: Freq
  , cfgTickRes :: Ticks
  }

data Servo = Servo
  { servoChan :: PWMChannel
  , servoClk :: Freq
  , servoMaxTicks :: Ticks
  , servoBiasTicks :: Ticks
  , servoAngleRange :: (Angle, Angle)
  , servoCurAngle :: Angle 
  }

fromChan :: PWMChannel -> Word8
fromChan = fromIntegral . fromEnum

periodToTicks :: Freq -> Ticks -> Double -> Ticks
periodToTicks clk tickRes time =
  round $ (time / (1000 / clkD)) * tickResD
  where
    clkD = fromIntegral clk
    tickResD = fromIntegral tickRes
  
initGPIO :: Freq -> GPIOConfig -> IO () 
initGPIO cfgClk GPIOConfig {..} = do

  setPinFunction cfgPin cfgPinMode
  setModePWM chan markspace enablePWM 
  setClockPWM divisor
  setRangePWM chan tickRes

  where
    chan = (fromChan cfgChan)
    tickRes = (fromIntegral cfgTickRes) :: Word32 
    divisor = fromIntegral $ (div cfgBclk  (cfgClk * cfgTickRes)) 
    markspace = 1 
    enablePWM = 1

initServo :: ServoConfig -> GPIOConfig -> IO Servo
initServo sc cfg@GPIOConfig {..} = do
  initGPIO clk cfg
  return servo
  where 
    clk = scClk sc
    (minAngleP, maxAngleP) = scAnglePeriod sc
    periodToTicks' = periodToTicks clk cfgTickRes
    biasTicks = periodToTicks' minAngleP 
    maxTicks = periodToTicks' maxAngleP - biasTicks
    servo = Servo cfgChan clk maxTicks biasTicks (scAngleRange sc) 0
      

angleToTicks :: Servo -> Angle -> Ticks   
angleToTicks s angle  = round $ dutyCycle * maxTicks + bias 
  where
    maxAngle = snd $ servoAngleRange s
    bias = fromIntegral $ servoBiasTicks s
    maxTicks = fromIntegral $ servoMaxTicks s
    dutyCycle = angle / maxAngle

setAngle ::  Servo -> Angle -> IO ()
setAngle s a = do
  print $ "Setting angle to: " ++ show a ++ " (" ++ show ang ++ " Ticks)" 
  setDataPWM (fromChan $ servoChan s) ang
  --threadDelay 500000
  where
    ang = fromIntegral $ angleToTicks s a 

