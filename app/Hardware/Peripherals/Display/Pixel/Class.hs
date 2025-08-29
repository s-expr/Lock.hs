module Hardware.Peripherals.Display.Pixel.Class where

import Data.ByteString
import Data.ByteString.Lazy (toStrict, fromStrict)

class Pixel p where
  getBytes :: p -> ByteString
  getIntensity :: p -> Int
  
