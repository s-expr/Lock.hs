module Hardware.Peripherals.SSD1306.SPI where

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




