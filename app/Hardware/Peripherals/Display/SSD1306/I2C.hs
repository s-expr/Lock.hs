{-#LANGUAGE BinaryLiterals #-}
module Hardware.Peripherals.Display.SSD1306.I2C where

import Hardware.Protocol.I2C
import Hardware.Protocol.Util
import Hardware.Peripherals.Display.SSD1306.Class
import Data.Bits
import Foreign.Marshal.Utils


newtype SSD1306 = SSD1306 { dev :: I2CDev }

data Msg 
  = Data [Byte]
  | Cmd [Byte]

runCmd :: SSD1306 -> I2CProg a -> IO a
runCmd (SSD1306 dev) = runI2C dev  

disp :: Addr -> SSD1306
disp addr = SSD1306 $ I2CDev addr "oled"

newController :: SSD1306 
newController = disp 0x3D 

transmit :: Msg -> I2CProg ()
transmit msg = write $ MsgList (ctrlByte msg)
  where 
    ctrlByte :: Msg -> [Byte]
    ctrlByte (Data msg) = 0b01000000 : msg
    ctrlByte (Cmd msg) = 0b00000000 : msg

cmd :: [Byte] -> I2CProg ()
cmd = transmit . Cmd 

opt :: Byte -> Int -> Bool -> Byte
opt byte bitSel should = 
  byte .|. shiftL (fromBool should) bitSel

sleep :: I2CProg ()
sleep = cmd [0xAE]
 
wake :: I2CProg ()
wake = cmd [0xAF]

externalPower :: Bool -> I2CProg () 
externalPower b = do
  cmd [0x8D, opt 0x10 2 b]

writePixels :: ByteString -> I2CProg ()
writePixels = undefined

setContrast :: Byte -> I2CProg ()
setContrast contrast = write (0x81::Byte, contrast)

fullBright :: Bool -> I2CProg ()
fullBright b = cmd [opt 0xA4 0 b]

invert :: Bool -> I2CProg ()
invert b = cmd [opt 0xA7 0 b]
  
reset :: I2CProg ()
reset = do
  sleep
  wake

activate :: I2CProg () 
activate = do 
  wake

deactivate :: I2CProg ()
deactivate = do
  sleep
  