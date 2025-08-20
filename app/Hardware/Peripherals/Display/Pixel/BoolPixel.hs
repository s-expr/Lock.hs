module Hardware.Peripherals.Display.Pixel.BoolPixel where

import Class
import Data.Binary

data BoolPixel
     = Off
     | On
     deriving (Show, Enum)

instance Binary BoolPixel where
  put p = putWord8 $ (fromIntegral . fromEnum) p
  get = toEnum <$> toIntegral <$> getWord8 

instance Pixel BoolPixel where
  getBytes = toStrict . encode . fromEnum 
  getIntensity = (255 *) . fromEnum 