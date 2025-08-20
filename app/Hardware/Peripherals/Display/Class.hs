{-#LANGUAGE ConstraintKinds #-}
{-#LANGUAGE KindSignatures #-}
module Hardware.Display.Class where

import Data.ByteString
import Data.Array

type Buffer p = Array Int p

--TODO: implement elegant failure and error messages
class (Monad m, Pixel p) => DisplayController a p where
  getResolution :: a -> (Int,Int)
  drawPixel :: a  -> p -> (Int, Int) -> m ()
  draw :: a -> Buffer p -> m ()
  clear :: a -> m ()

data Display c p = Display
  { fb :: Buffer p
  , scale :: Int
  , controller :: c
  }

mkDisplay :: Pixel p, DisplayController c p => Buffer p -> c -> Display c p
mkDisplay = Display 

getBuffer :: Pixel p => Display c p -> Buffer p 
getBuffer = fb

updateBuffer :: Pixel p => Display c p -> Buffer p -> m ()
updateBuffer = _

setPixel :: Pixel p => Display c p -> p -> (Int, Int) -> m ()
setPixel = _ 

drawBuffer = Monad m, Pixel p, DisplayController c p => Display c p -> m ()
drawBuffer d = draw dc buf 
  where
    dc = controller d
    buf = fb d

setScale :: Display c p -> Int -> m ()

getScale :: Display c p -> Int
getScale = scale

getResolution :: DisplayController c p =>  Display c -> m (Int, Int)
getResolution = _
