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
import Control.Monad.Aff
import Graphics.Canvas as C
import Signal as S
import Signal.DOM as S

import Utils
import GameObject
import Collisions
import Input as I


main = do
  Just canvas <- C.getCanvasElementById "canvas"
  context <- C.getContext2D canvas
  input <- I.input
  launchAff $ do
    initState <- initialState
    let game = S.foldp update initState input
    liftEff' $ S.runSignal (render context <$> game)

-----------
-- Model
-----------

type State
  = { objs1 :: Array GameObject
    , objs2 :: Array GameObject
    }

initialState :: Aff _ State
initialState = do
  obj1 <- rect1
  obj2 <- rect2
  pure $
    { objs1: [obj1]
    , objs2: [obj2]
    }

------------
-- Update
------------

update :: Point -> State -> State
update direction state =
  undoCollisions $
  collisionLayers $
  (\state -> { objs1: map (moveObj direction) state.objs1
  , objs2: map (moveObj direction) state.objs2
  }) $ collisionLayers state

collisionLayers :: State -> State
collisionLayers state =
  { objs1: state.objs1 `testCollisionWith` state.objs2
  , objs2: state.objs2 `testCollisionWith` state.objs1
  }

undoCollisions :: State -> State
undoCollisions state =
  { objs1: map undoCollision state.objs1
  , objs2: map undoCollision state.objs2
  }
------------
-- Render
------------

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  traverse (renderObj context) state.objs1
  traverse (renderObj context) state.objs2
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

