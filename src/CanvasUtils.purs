module CanvasUtils where

import Prelude

import Data.Lens
import Data.Array
import Control.Monad.Eff
import Graphics.Canvas as C
import Control.Monad.Aff

width :: Number
width = 1024.0
height :: Number
height = 800.0

type Point =
  { x :: Number, y :: Number }

makePoint :: Number -> Number -> Point
makePoint x y = { x: x, y: y }

loadImageData :: String -> Aff _ C.CanvasImageSource
loadImageData src = makeAff (\error success -> C.withImage src success)

pointInRect :: forall r. Point
            -> { pos :: Point, size :: Point | r }
            -> Boolean
pointInRect p obj =
     (obj.pos.x <= p.x && p.x <= obj.pos.x + obj.size.x)
  && (obj.pos.y <= p.y && p.y <= obj.pos.y + obj.size.y)

------------
-- Lenses
------------

x = lens _.x (_ { x = _ })
y = lens _.y (_ { y = _ })

