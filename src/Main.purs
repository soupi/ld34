module Main where

import Prelude

import Data.Lens
import Data.Array
import Data.List as List
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Control.Monad.Aff
import Graphics.Canvas as C
import Signal as S
import Signal.Time as S
import Signal.DOM as S

import Utils
import CanvasUtils
import Zipper
import TextBar as T
import Input as I
import Screen


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

data State
  = VNScreen (Zipper Screen)
  | Wait S.Time State

initialState :: Aff _ State
initialState = do
  pure $
    VNScreen (screens intro)

------------
-- Update
------------

update :: I.Input -> State -> State
update input currState@(Wait t nextState) =
  if t + S.second / 4.0 <= input.time
  then nextState
  else currState
update input state@(VNScreen screens) =
  if input.screenDir > 0.0
  then
    (Wait input.time <<< VNScreen <<< snd <<< next) screens
  else if input.screenDir < 0.0
  then
    (Wait input.time <<< VNScreen <<< snd <<< back) screens
  else
    state


------------
-- Render
------------

getScreens :: State -> Zipper Screen
getScreens (VNScreen screens) = screens
getScreens (Wait _ state) = getScreens state

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  renderScreen context (current $ getScreens state)
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

