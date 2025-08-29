{-#LANGUAGE ConstraintKinds #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE KindSignatures #-}
module Hardware.Peripherals.Display.Class where

import Hardware.Peripherals.Display.Pixel.Class
import Data.ByteString
import Data.Array

type Buffer p = Array Int p

--TODO: implement elegant failure and error messages
class DisplayController a where
  resolution :: a -> (Int, Int)
  drawPixel :: a  -> p -> (Int, Int) -> m ()
  draw :: a -> Buffer p -> m ()
  clear :: a -> m ()

data Display c p = Display
  { fb :: Buffer p
  , controller :: c
  , scale :: Int
  }

mkDisplay :: (Pixel p, DisplayController c) => Buffer p -> c -> Int -> Display c p
mkDisplay = Display 

getBuffer :: (Pixel p) => Display c p -> Buffer p 
getBuffer = fb

updateBuffer :: (Pixel p) => Display c p -> Buffer p -> m ()
updateBuffer = undefined

setPixel :: (Pixel p) => Display c p -> p -> (Int, Int) -> m ()
setPixel = undefined

drawBuffer :: (Monad m, Pixel p, DisplayController c) => Display c p -> m ()
drawBuffer d = draw dc buf 
  where
    dc = controller d
    buf = fb d

setScale :: Display c p -> Int -> m ()
setScale = undefined

getScale :: Display c p -> Int
getScale = scale

getResolution :: (DisplayController c ) =>  Display c p -> m (Int, Int)
getResolution = undefined
