module SimScreen where

import Prelude

import Data.Int
import Data.Lens
import Data.Maybe
import Data.Either
import Data.Traversable
import Data.List as List
import Data.String (split, length, take, drop)
import Control.Monad.Eff
import Control.Monad.Aff
import Graphics.Canvas as C

import Utils
import CanvasUtils
import Zipper
import TextBar as T
import Input as I
import Machine as M

-- import Debug.Trace

-----------
-- Model
-----------

data State
  = ShowInput
  | ShowOutput
  | ShowMachine
  | RunMachine
  | Done

type SimScreen
  = { code :: List.List String
    , currLine :: String
    , gfx :: C.CanvasImageSource
    , compRun :: Boolean
    , inputs  :: List.List Int
    , outputs :: List.List Int
    , machine :: Maybe M.Machine
    , tests :: List.List { input :: List.List String, output :: List.List String }
    , state :: State
    }

mkSimScreen :: C.CanvasImageSource -> SimScreen
mkSimScreen img =
  { code: List.Nil
  , currLine: ""
  , gfx: img
  , compRun: false
  , inputs: List.Nil
  , outputs: List.Nil
  , machine: Nothing
  , tests: List.Nil
  , state: ShowMachine
  }

mkCompScreen :: Aff _ SimScreen
mkCompScreen = do
  map mkSimScreen $ loadImageData "assets/comp2.png"

done :: SimScreen -> Boolean
done _ = false

------------
-- Update
------------

zeroButton =
  { pos:  { x: 315.0, y: 630.0 }
  , size: { x: 131.0, y: 80.0 }
  }

oneButton =
  { pos:  { x: 497.0, y: 627.0 }
  , size: { x: 134.0, y: 82.0 }
  }

resetButton =
  { pos:  { x: 497.0, y: 627.0 }
  , size: { x: 134.0, y: 82.0 }
  }

runButton =
  { pos:  { x: 497.0, y: 627.0 }
  , size: { x: 134.0, y: 82.0 }
  }


updateMouseInput i =
  case i.mouseClick of
    Nothing ->
      i
    Just p ->
      if p `pointInRect` zeroButton then
        set (I.zeroOne <<< I.zero) true i
      else if p `pointInRect` oneButton then
        set (I.zeroOne <<< I.one) true i
      else
        i

update :: I.Input -> SimScreen -> SimScreen
update i state =
  let input = updateMouseInput i
  in
   updateMachine input $
   updateCode input state

updateCode :: I.Input -> SimScreen -> SimScreen
updateCode input state =
  let line = state.currLine <> if input.zeroOne.zero then "0" else "" <> if input.zeroOne.one then "1" else ""
  in
    if length line >= 8
    then over code (List.Cons (take 8 line)) $
         set currLine (drop 8 line) state
    else
         set currLine line state

updateMachine :: I.Input -> SimScreen -> SimScreen
updateMachine input state =
  case state.machine of
    Just m ->
      if M.halted m then
        state { outputs = m.output }
      else
        startMachine input $ state { machine = pure $ tryEval m }
    Nothing ->
      startMachine input state

startMachine :: I.Input -> SimScreen -> SimScreen
startMachine input state =
  if input.screenDir > 0.0 then
    if isJust state.machine then
      state { machine = Nothing }
    else
      let insts = traverse M.translate $ List.reverse state.code
      in case insts of
        Left _ -> state
        Right is -> state { machine = M.mkMachine is state.inputs }
  else state

tryEval :: M.Machine -> M.Machine
tryEval machine =
  case M.eval machine of
    Left _ -> machine
    Right m -> m

------------
-- Render
------------

render :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
render ctx screen = do
  C.drawImage ctx screen.gfx 0.0 0.0
  C.setFillStyle "#CC3388" ctx

  sequence $ List.zipWith (\p txt -> C.fillText ctx txt p.x p.y) (map (getPosition 350.0 230.0) $ List.range 0 40) (List.reverse screen.code)

  C.setFillStyle "#DD4499" ctx
  C.fillText ctx ("> " <> screen.currLine) 350.0 (230.0 + 30.0 * 8.0)

  renderOutput ctx screen
  pure unit

getPosition x y i =
  { x: x + 100.0 * toNumber (i `div` 8)
  , y: y + 30.0  * toNumber (i `mod` 8)
  }


renderOutput :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
renderOutput ctx screen = do
  C.setFillStyle "rgba(235,235,255,0.9)" ctx
  C.fillRect ctx { x: 700.0, y: 100.0, w: 300.0, h: 400.0 }
  C.setFillStyle "#004499" ctx
  sequence $ List.zipWith (\p txt -> C.fillText ctx txt p.x p.y) (map (getPosition 740.0 150.0) $ List.range 0 8) (List.reverse $ map show screen.outputs)
  pure unit

---------------------

currLine = lens _.currLine (_ { currLine = _ })
code = lens _.code (_ { code = _ })
