module Main where

import Prelude

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Aff
import Graphics.Canvas as C
import Signal as S
import Signal.Time as S

import Utils
import CanvasUtils
import Zipper
import Input as I
import Screen
import SimScreen as Sim


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
  | Simulation Sim.SimScreen
  | Wait S.Time State
  | Screens State State

finished :: State -> Boolean
finished (VNScreen zipp) = not $ fst $ next zipp
finished (Wait _ _) = false
finished (Simulation sim) = Sim.done sim
finished (Screens s _) = finished s

initialState :: Aff _ State
initialState = do
  comp <- loadImageData "assets/comp.png"
  pure $
    Screens (VNScreen $ screens comp intro)
            (Simulation $ Sim.mkSimScreen comp)

------------
-- Update
------------

update :: I.Input -> State -> State
update input currState@(Screens currScreen nextScreen) =
  if input.screenDir > 0.0 && finished currScreen
  then Wait input.time nextScreen
  else Screens (update input currScreen) nextScreen
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

renderScreens :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
renderScreens ctx (Simulation screen) = Sim.render ctx screen
renderScreens ctx (VNScreen screens) = renderScreen ctx (current screens)
renderScreens ctx (Wait _ state) = renderScreens ctx state
renderScreens ctx (Screens s _) = renderScreens ctx s

render :: C.Context2D -> State -> Eff ( canvas :: C.Canvas | _) Unit
render context state = do
  clearCanvas context
  renderScreens context state
  pure unit

clearCanvas ctx = do
  C.setFillStyle "#1B1C1B" ctx
  C.fillRect ctx { x: 0.0, y: 0.0, w: 1024.0, h: 800.0 }

