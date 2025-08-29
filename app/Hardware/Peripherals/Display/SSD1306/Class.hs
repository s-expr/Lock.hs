module Hardware.Peripherals.Display.SSD1306.Class where

import Hardware.Peripherals.Display.Class
import Data.ByteString 
import Data.Word 

type Page = Word8
type AddrRange = (Word8, Word8)

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

{-
instance (SSD1306Com c) => DisplayController c where
  getResolution :: c -> m (Int, Int)
  getResolution _ = return (128, 64)
  drawPixel :: c  -> p -> (Int, Int) -> m ()
  drawPixel com = undefined
  draw :: c -> Buffer p -> m ()
  clear :: c -> m ()
-}

class SSD1306Com a where
  writeScanning :: a -> Word8 -> m ()
  readPage :: a -> m Page
  initDisplay :: a -> AddrMode -> AddrRange -> m a
  setScroll :: a -> Scroll -> m a
  setAddrMode :: a -> m a
  
withDisplayController :: undefined 
withDisplayController = undefined
