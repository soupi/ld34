module Main where

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

width :: Number
width = 1024.0
height :: Number
height = 800.0

main = do
  Just canvas <- C.getCanvasElementById "canvas"
  context <- C.getContext2D canvas
  frames <- S.animationFrame
  arrowsInputs <- arrows
  let input = const <$> arrowsInputs <*> frames
  let game = S.foldp update initialState input
  S.runSignal (render context <$> game)


arrows = do
  leftInput  <- S.keyPressed leftKeyCode
  rightInput <- S.keyPressed rightKeyCode
  upInput    <- S.keyPressed upKeyCode
  downInput  <- S.keyPressed downKeyCode
  let asNum b = if b then 1.0 else 0.0
  pure $  (\l r u d -> { x: asNum r - asNum l, y: asNum d - asNum u } )
      <$> leftInput
      <*> rightInput
      <*> upInput
      <*> downInput

leftKeyCode = 37
rightKeyCode = 39
upKeyCode = 38
downKeyCode = 40

type State
  = { objs1 :: Array GameObject
    , objs2 :: Array GameObject
    }

initialState :: State
initialState =
  { objs1: [rect1]
  , objs2: [rect2]
  }

update :: Point -> State -> State
update direction state =
  collisionLayers $
  { objs1: map (moveObj direction) state.objs1
  , objs2: map (moveObj direction) state.objs2
  }

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  traverse (renderObj context) state.objs1
  traverse (renderObj context) state.objs2
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

--

type Point =
  { x :: Number, y :: Number }

type GameObject =
  { pos :: Point
  , size  :: Point
  , speed :: Number
  , collided :: Boolean
  , color :: String
  }

rect1 :: GameObject
rect1 =
  { pos:  { x: width / 2.0 - 15.0, y: height / 2.0 - 15.0 }
  , size: { x: 40.0, y: 40.0 }
  , speed: 5.0
  , collided: false
  , color: "#0088DD"
  }

rect2 :: GameObject
rect2 =
  { pos:  { x : width / 2.0 - 15.0, y : height / 2.0 - 15.0 }
  , size: { x: 30.0, y: 30.0 }
  , speed: 1.0
  , collided: false
  , color: "#0088DD"
  }

moveObj :: Point -> GameObject -> GameObject
moveObj direction rect =
  over (pos <<< y) (+ (direction.y * rect.speed)) <<< over (pos <<< x) (+ (direction.x * rect.speed)) $ rect

renderObj :: C.Context2D -> GameObject -> Eff ( canvas :: C.Canvas | _) Unit
renderObj ctx state = do
  C.setFillStyle (if state.collided then "#DD0000" else state.color) ctx
  C.fillRect ctx { x: state.pos.x
                 , y: state.pos.y
                 , w: state.size.x, h: state.size.y }
  pure unit

----------------
-- Collisions
----------------

collisionLayers :: State -> State
collisionLayers state =
  { objs1: state.objs1 `testCollisionWith` state.objs2
  , objs2: state.objs2 `testCollisionWith` state.objs1
  }

testCollisionWith :: Array GameObject -> Array GameObject -> Array GameObject
testCollisionWith objs1 objs2 =
  map (\x -> set collided (any id $ map (collision x) objs2) x) objs1

collision :: GameObject -> GameObject -> Boolean
collision x1 x2 = any id $
     map (flip pointInRect x2) (corners x1)
  <> map (flip pointInRect x1) (corners x2)

corners :: GameObject -> Array Point
corners obj =
  [ makePoint obj.pos.x obj.pos.y
  , makePoint (obj.pos.x + obj.size.x) obj.pos.y
  , makePoint obj.pos.x (obj.pos.y + obj.size.y)
  , makePoint (obj.pos.x + obj.size.x) (obj.pos.y + obj.size.y)
  ]

makePoint x y = {x: x, y: y}

pointInRect :: Point -> GameObject -> Boolean
pointInRect p obj =
     (obj.pos.x <= p.x && p.x <= obj.pos.x + obj.size.x)
  && (obj.pos.y <= p.y && p.y <= obj.pos.y + obj.size.y)

------------
-- Lenses
------------

collided = lens _.collided (_ { collided = _ })
pos = lens _.pos (_ { pos = _ })
x = lens _.x (_ { x = _ })
y = lens _.y (_ { y = _ })
