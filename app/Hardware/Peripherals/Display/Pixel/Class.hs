module Hardware.Peripherals.Display.Pixel.Class where

class Pixel p where
  getBytes :: p -> ByteString
  getIntensity :: p -> Int
  
