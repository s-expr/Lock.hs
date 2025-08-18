module Hardware.Peripherals.SSD136.Class where

import Hardware.Display.Class
import Data.ByteString 
import Data.Word 

type Page = Word8
type AddrRange = (Word8, Word8)

data BoolPixel
     = Off
     | On
     deriving (Show, Enum)

data Scroll
  = Left
  | Right
  | Up
  | Down
  deriving (Show, Enum)

data DisplayMode
  = Normal
  | Inverted
  deriving (Show, Enum)

data AddrMode
  = Horizontal 
  | Vertical
  | Paging
  deriving (Show, Enum)

instance Binary BoolPixel where
  put p = putWord8 $ (fromIntegral . fromEnum) p
  get =  getWord8 >>= toIntegral >>= toEnum

instance Pixel BoolPixel where
  getBytes = toStrict . encode . fromEnum 
  getIntensity = (255 *) . fromEnum 
  
instance (SSD1306Com c) => DisplayController c where
  getResolution :: c -> m (Int, Int)
  getResolution _ = return (128, 64)
  drawPixel :: c  -> p -> (Int, Int) -> m ()
  drawPixel com =  
  draw :: c -> Buffer p -> m ()
  clear :: c -> m ()

class (Monad m) => SSD1306Com a where
  writeScanning :: Word8 -> m ()
  readPage :: a -> m Page
  initDisplay :: a -> AddrMode -> AddrRange -> m a
  setScroll :: a -> ScrollMode -> m a
  setAddrMode :: a -> m a
  
withDisplayController :: 
withDisplayController = _
