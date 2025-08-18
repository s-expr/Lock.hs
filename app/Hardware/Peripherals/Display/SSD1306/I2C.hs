module Hardware.Peripherals.SSD1306.I2C where

import Hardware.Protocol.I2C
import Hardware.Peripherals.SSD1306.Class

newtype SSD1306I2C = SSD1306I2C { dev :: I2CDev }

instance SSD1306Com SSD1306I2C where
  
  

disp :: Addr -> I2CDev
disp addr = I2CDev  addr "oled"

newCom :: Int -> SSD1306
newCom addr = SSD1306 . (disp $ byte addr)

byte = fromIntegral
bytes xs = byte <$> xs

off :: I2CDev ->  IO ()
off d = write d $ byte 0xAE
 
on :: I2CDev ->  IO ()
on d = write d $ byte 0xAF

reset :: I2CDev -> IO ()
reset = do
  off
  on

activate :: I2CDev -> IO () 
activate d = do
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

