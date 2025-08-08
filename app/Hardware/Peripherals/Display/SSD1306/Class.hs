module Hardware.Peripherals.SSD136.Class where

import Hardware.Display.Class
import Data.ByteString 
import Data.Word 

type Page = Word8
type AddrRange = (Word8, Word8)
data BoolPixel
     = Off
     | On
     deriving (Enum)

data Scroll
  = Left
  | Right
  | Up
  | Down
  deriving (Enum)

data DisplayMode
  = Normal
  | Inverted
  deriving (Enum)

data AddrMode
  = Horizontal 
  | Vertical
  | Paging
  deriving (Enum)

data SSD1306 a
  = SSD1306 { impl :: a }

instance Binary (BoolPixel) where
  put p = putWord8 $ (fromIntegral . fromEnum) p
  get =  getWord8 >>= toIntegral >>= toEnum

instance Pixel (BoolPixel) where
  getBytes = toStrict . encode . fromEnum 
  getIntensity = (255 *) . fromEnum 
  
instance Display (SSD136 a) where
  getResolution :: a -> (Int,Int)
  drawPixel :: a  -> p -> (Int, Int) -> m ()
  draw :: a -> Buffer p -> m ()
  clear :: a -> m ()

class Monad m => SSD1306Impl a where
  write :: Word8 -> IO ()
  readPage :: a -> Page -> m ByteString
  initDisplay :: a -> AddrMode -> AddrRange -> m a
  setScroll :: a -> ScrollMode -> m a
  setAddrMode :: a -> m a
  
newOled :: SSD1306Impl a => a -> SSD1306
newOled impl = SSD1306 impl

