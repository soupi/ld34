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
  , collision :: Maybe Point
  , color :: String
  }

rect1 :: GameObject
rect1 =
  { pos:  { x: width / 2.0 - 115.0, y: height / 2.0 - 15.0 }
  , size: { x: 50.0, y: 50.0 }
  , speed: 5.0
  , collision: Nothing
  , color: "#0088DD"
  }

rect2 :: GameObject
rect2 =
  { pos:  { x : width / 2.0 - 15.0, y : height / 2.0 - 15.0 }
  , size: { x: 70.0, y: 70.0 }
  , speed: 0.0
  , collision: Nothing
  , color: "#0088DD"
  }

------------
-- Update
------------

moveObj :: Point -> GameObject -> GameObject
moveObj direction rect =
  case rect.collision of
    Nothing ->
      over (pos <<< y) (+ (direction.y * rect.speed)) <<< over (pos <<< x) (+ (direction.x * rect.speed)) $ rect
    Just dir ->
      let direction = { x: -dir.x, y: -dir.y }
      in
        over (pos <<< y) (+ (direction.y * rect.speed)) <<< over (pos <<< x) (+ (direction.x * rect.speed)) $ rect

undoCollision :: GameObject -> GameObject
undoCollision rect =
  case rect.collision of
    Nothing ->
      rect
    Just dir ->
      let direction = { x: -dir.x, y: -dir.y }
      in
        over (pos <<< y) (+ (direction.y * rect.speed)) <<< over (pos <<< x) (+ (direction.x * rect.speed)) $ rect



------------
-- Render
------------

renderObj :: C.Context2D -> GameObject -> Eff ( canvas :: C.Canvas | _) Unit
renderObj ctx state = do
  C.setFillStyle (if isJust state.collision then "#DD0000" else state.color) ctx
  C.fillRect ctx { x: state.pos.x
                 , y: state.pos.y
                 , w: state.size.x, h: state.size.y }
  C.setFillStyle "white" ctx
  C.fillText ctx (showCol state.collision) state.pos.x state.pos.y
  pure unit

showCol :: Maybe Point -> String
showCol Nothing  = ""
showCol (Just a) = "(" <> show a.x <> "," <> show a.y <> ")"

showX :: Number -> String
showX 0.0 = "Center"
showX 1.0 = "Right"
showX (-1.0) = "Left"

showY :: Number -> String
showY 0.0 = "Center"
showY 1.0 = "Down"
showY (-1.0) = "Up"
