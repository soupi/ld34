module Main where

import Prelude

import Data.List (List(..))
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
finished (Screens s1 s2) = finished s1 && finished s2

initialState :: Aff _ State
initialState = do
  comp <- loadImageData "assets/comp2.png"
  pure $
    Screens
      (Screens
        (VNScreen $ screens comp intro) $
        Screens
          (mission01 comp)
          (mission02 comp))
      (VNScreen $ screens comp end_text)

mission01 comp =
  Screens
     (VNScreen $ screens comp mission01_text)
     (Simulation $ Sim.mkSimScreen (zipper {input: Nil, output: Cons 5 Nil} Nil Nil) comp)

mission01_text =
  """
Your first mission is to make the machine print the number 5.

We are counting on you!
"""


mission02 comp =
  Screens
     (VNScreen $ screens comp mission02_text)
     (Simulation $ Sim.mkSimScreen (zipper {input: Cons 4 $ Cons 5 Nil, output: Cons 9 Nil} Nil $
                                      Cons {input: Cons 3 $ Cons 3 Nil, output: Cons 6 Nil} $
                                      Cons {input: Cons (-3) $ Cons 3 Nil, output: Cons 0 Nil} Nil) comp)

mission02_text =
  """
Nice, you have completed the first mission!

Though maybe it's not that impressive, there are only two buttons, how hard can it be?

Next, we need a machine that can add two numbers together.

Build one for us, we'll give you two numbers, you give us their sum.
"""



end_text =
  """
You did it! You completed all of our tasks!

That's amazing!

We have no more tasks for you.

Therefore, you are fired.

Thank you, and goodbye.

The End.
"""


------------
-- Update
------------

update :: I.Input -> State -> State
update input currState@(Screens currScreen nextScreen) =
  if (input.screenDir > 0.0 || isJust input.mouseClick) && finished currScreen
  then Wait input.time nextScreen
  else Screens (update input currScreen) nextScreen
update input currState@(Wait t nextState) =
  if t + S.second / 4.0 <= input.time
  then nextState
  else currState
update input (Simulation sim) = Simulation $ Sim.update input sim
update input state@(VNScreen screens) =
  if input.screenDir > 0.0 || isJust input.mouseClick then
    (VNScreen <<< snd <<< next) screens
  else if input.screenDir < 0.0 then
    (VNScreen <<< snd <<< back) screens
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

