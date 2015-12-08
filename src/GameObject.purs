module GameObject where

import Prelude

import Data.Lens
import Data.Array
import Data.Maybe
import Math
import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Graphics.Canvas as C
import Signal as S
import Signal.DOM as S

import Utils
import Collisions
import Input as I


-----------
-- Model
-----------

type GameObject =
  { pos :: Point
  , size  :: Point
  , speed :: Number
  , collision :: Boolean
  , color :: String
  }

rect1 :: GameObject
rect1 =
  { pos:  { x: width / 2.0 - 115.0, y: height / 2.0 - 15.0 }
  , size: { x: 40.0, y: 40.0 }
  , speed: 5.0
  , collision: false
  , color: "#0088DD"
  }

rect2 :: GameObject
rect2 =
  { pos:  { x : width / 2.0 - 15.0, y : height / 2.0 - 15.0 }
  , size: { x: 30.0, y: 30.0 }
  , speed: 1.0
  , collision: false
  , color: "#0088DD"
  }

------------
-- Update
------------

moveObj :: Point -> GameObject -> GameObject
moveObj direction rect =
  over (pos <<< y) (+ (direction.y * rect.speed)) <<< over (pos <<< x) (+ (direction.x * rect.speed)) $ rect

------------
-- Render
------------

renderObj :: C.Context2D -> GameObject -> Eff ( canvas :: C.Canvas | _) Unit
renderObj ctx state = do
  C.setFillStyle (if state.collision then "#DD0000" else state.color) ctx
  C.fillRect ctx { x: state.pos.x
                 , y: state.pos.y
                 , w: state.size.x, h: state.size.y }
  pure unit


