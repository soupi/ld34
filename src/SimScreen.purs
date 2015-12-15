module SimScreen where

import Prelude

import Data.Int
import Data.Lens
import Data.Maybe
import Data.Either
import Data.Traversable
import Data.List as List
import Data.String (split, length, take, drop)
import Data.String as Str
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
  | TestMachine
  | ShowError String
  | Done

type Test =
  { input :: List.List Int, output :: List.List Int }

type SimScreen
  = { code :: List.List String
    , currLine :: String
    , currInput :: String
    , gfx :: C.CanvasImageSource
    , compRun :: Boolean
    , inputs  :: List.List String
    , outputs :: List.List Int
    , machine :: Maybe M.Machine
    , tests :: Zipper Test
    , state :: State
    }

mkSimScreen :: Zipper Test -> C.CanvasImageSource -> SimScreen
mkSimScreen tests img =
  { code: List.Nil
  , currInput: ""
  , currLine: ""
  , gfx: img
  , compRun: false
  , inputs: List.Nil
  , outputs: List.Nil
  , machine: Nothing
  , tests: tests
  , state: ShowMachine
  }

mkCompScreen :: Zipper Test -> Aff _ SimScreen
mkCompScreen tests = do
  map (mkSimScreen tests) $ loadImageData "assets/comp2.png"

done :: SimScreen -> Boolean
done state =
  case state.state of
    Done -> true
    _ -> false

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
   updateState input $
   resetMachine input $
   updateCode input state

updateCode :: I.Input -> SimScreen -> SimScreen
updateCode input state =
  case state.state of
    ShowMachine ->
        let line = state.currLine <> if input.zeroOne.zero then "0" else "" <> if input.zeroOne.one then "1" else ""
        in
            if length line >= 8
            then over code (List.Cons (take 8 line)) $
                set currLine (drop 8 line) state
            else
                set currLine line state
    _ ->
      state

startMachine :: I.Input -> SimScreen -> SimScreen
startMachine input state =
    if isJust state.machine then
      state { machine = Nothing }
    else
      let insts = traverse M.translate $ List.reverse state.code
      in case insts of
        Left err ->
          state { state = ShowError $ "Code load failed: " <> show err <> "." }
        Right is ->
          case traverse M.strBinToInt state.inputs of
            Left err ->
              state { state = ShowError $ "Input load failed: " <> show err <> "." }
            Right inputs ->
              state { machine = M.mkMachine is inputs }

tryEval :: M.Machine -> M.Machine
tryEval machine =
  case M.eval machine of
    Left _ -> machine
    Right m -> m

resetMachine :: I.Input -> SimScreen -> SimScreen
resetMachine input state =
  if input.screenDir < 0.0 then
    case state.state of
      ShowMachine ->
        state { machine = Nothing, code = List.Nil, inputs = List.Nil, outputs = List.Nil, currLine = "" }
      ShowOutput ->
        state { state = ShowMachine }
      ShowInput ->
        state { state = ShowMachine }
      ShowError _ ->
        state { state = ShowMachine }
      RunMachine ->
        state { state = ShowMachine, machine = Nothing }
      TestMachine ->
        state { state = ShowMachine, machine = Nothing }
      Done ->
        state
  else
    state

updateState :: I.Input -> SimScreen -> SimScreen
updateState input state =
  case state.state of
    ShowMachine ->
      if fst input.io then
        state { state = ShowInput }
      else if snd input.io then
        state { state = ShowOutput }
      else if input.runTests then
        state { state = TestMachine }
      else if input.screenDir > 0.0 then
        state { state = RunMachine }
      else
        state
    ShowInput -> updateInput input $
      if fst input.io then
        state { state = ShowMachine }
      else if snd input.io then
        state { state = ShowOutput }
      else if input.runTests then
        state { state = TestMachine }
      else if input.screenDir > 0.0 then
        state { state = RunMachine }
      else
        state
    ShowOutput ->
      if snd input.io then
        state { state = ShowMachine }
      else if fst input.io then
        state { state = ShowInput }
      else if input.runTests then
        state { state = TestMachine }
      else if input.screenDir > 0.0 then
        state { state = RunMachine }
      else
        state
    TestMachine ->
      case state.machine of
        Nothing ->
          runTest $ state { tests = start state.tests }
        Just mch ->
          if M.halted mch then
            case M.hasErrors mch of
              Just err ->
                state { state = ShowError $ "Run failed: " <> show err <> "." }
              Nothing ->
                if (current state.tests).output == mch.output then
                  if fst $ next state.tests then
                    runTest $ state { tests = snd $ next state.tests }
                  else
                    state { state = Done }
                else
                  state { state = ShowError $ "Test failed: input was: " <> show (current state.tests).input <> ", expected output was: " <> show (List.reverse (current state.tests).output) <> ". got: " <> show (List.reverse mch.output) <> "." }
          else case M.eval mch of
            Left err ->
              state { state = ShowError $ "Run failed: " <> show err <> "." }
            Right newMachine ->
              state { machine = Just newMachine }

    RunMachine -> do
        case state.machine of
          Just m ->
            if M.halted m then
              case M.hasErrors m of
                Just err ->
                  state { state = ShowError $ "Run failed: " <> show err <> "." }
                Nothing ->
                  state { state = ShowOutput, outputs = m.output, machine = Nothing }
            else
              case M.eval m of
                Left err ->
                  state { state = ShowError $ "Run failed: " <> show err <> "." }
                Right newMachine ->
                  state { machine = Just newMachine }

          Nothing ->
            startMachine input state
    _ ->
      state


    Done ->
      state
    ShowError _ ->
      if input.screenDir < 0.0 then
        state { state = ShowMachine }
      else
        state

runTest state =
  let insts = traverse M.translate $ List.reverse state.code
  in case insts of
    Left err ->
      state { state = ShowError $ "Loading Code failed: " <> show err <> "." }
    Right is ->
      state { machine = M.mkMachine is (current state.tests).input }


updateInput :: I.Input -> SimScreen -> SimScreen
updateInput input state =
  case state.state of
    ShowInput ->
      let line = state.currInput <> if input.zeroOne.zero then "0" else "" <> if input.zeroOne.one then "1" else ""
      in
        if length line >= 5
        then over inputs (List.Cons (take 5 line)) $
            set currInput (drop 5 line) state
        else
            set currInput line state
    _ -> state

------------
-- Render
------------

render :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
render ctx screen = do
  C.drawImage ctx screen.gfx 0.0 0.0
  C.setFillStyle "#CC3388" ctx

  sequence $ List.zipWith (\p txt -> C.fillText ctx txt p.x p.y) (map (getPosition 350.0 230.0) $ List.range 0 40) (List.reverse screen.code)

  C.setFont "18px monospace" ctx

  case screen.state of
    ShowMachine -> do
      renderMachine ctx screen
    ShowInput ->
      renderInput ctx screen
    ShowOutput ->
      renderOutput ctx screen
    RunMachine ->
      pure unit
    TestMachine -> do
      renderMachine ctx screen
      renderInput ctx screen
      renderOutput ctx screen
    ShowError err ->
      renderError err ctx screen
    Done ->
      renderDone ctx screen

  case screen.state of
    ShowMachine -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Zeros and Ones" 120.0 40.0
    ShowInput -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Inputs" 120.0 40.0
    ShowOutput -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Outputs" 120.0 40.0
    TestMachine -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Running Tests..." 120.0 40.0
    RunMachine -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Running..." 120.0 40.0
    ShowError err -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Error" 120.0 40.0
    Done -> do
        C.setFillStyle "white" ctx
        C.fillText ctx "Done" 120.0 40.0

  pure unit

getPosition x y i =
  { x: x + 100.0 * toNumber (i `div` 8)
  , y: y + 30.0  * toNumber (i `mod` 8)
  }

renderMachine :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
renderMachine ctx screen = do
  C.setFillStyle "#DD4499" ctx
  C.fillText ctx ("> " <> screen.currLine) 350.0 (230.0 + 30.0 * 8.0)
  pure unit

renderOutput :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
renderOutput ctx screen = do
  C.setFillStyle "rgba(235,235,255,0.9)" ctx
  C.fillRect ctx { x: 700.0, y: 100.0, w: 300.0, h: 400.0 }
  C.setFillStyle "#004499" ctx
  sequence $ List.zipWith (\p txt -> C.fillText ctx txt p.x p.y) (map (getPosition 740.0 150.0) $ List.range 0 8) (List.reverse $ map show screen.outputs)
  pure unit

renderInput :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
renderInput ctx screen = do
  C.setFillStyle "rgba(235,235,255,0.9)" ctx
  C.fillRect ctx { x: 40.0, y: 100.0, w: 300.0, h: 400.0 }
  C.setFillStyle "#004499" ctx
  sequence $ List.zipWith (\p txt -> C.fillText ctx txt p.x p.y) (map (getPosition 80.0 150.0) $ List.range 0 8) (List.reverse screen.inputs)

  C.setFillStyle "#DD4499" ctx
  C.fillText ctx ("> " <> screen.currInput) 80.0 (150.0 + 30.0 * 10.0)

  pure unit

renderError :: String -> C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
renderError err ctx state = do
  C.setFillStyle "rgba(235,135,155,0.9)" ctx
  C.fillRect ctx { x: 80.0, y: 20.0, w: width - 160.0, h: 100.0 }
  C.setFillStyle "white" ctx
  C.setFont "16px monospace" ctx
  let texts = map (splitLine err) (List.range 0 (Str.length err / 80))
  sequence $ List.zipWith (\i text -> C.fillText ctx text 120.0 (60.0 + 30.0 * toNumber i)) (List.range 0 (List.length texts)) texts
  C.setFont "18px monospace" ctx

  pure unit

renderDone :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
renderDone ctx state = do
  C.setFillStyle "rgba(35,235,155,0.9)" ctx
  C.fillRect ctx { x: 80.0, y: 20.0, w: width - 160.0, h: 100.0 }
  C.setFillStyle "white" ctx
  C.fillText ctx "Great Job!" 120.0 60.0
  pure unit


splitLine text i =
  let n = i * 80
  in
    case Str.uncons $ Str.drop (n+79) text of
      Nothing ->
        Str.take 80 $ Str.drop n text
      Just {head: x, tail: xs} ->
        if x == ' ' then
          Str.take 80 $ Str.drop n text
        else case Str.uncons $ Str.drop (n+80) text of
          Nothing ->
            Str.take 80 $ Str.drop n text
          Just {head: x, tail: xs} ->
            if x == ' ' then
              Str.take 80 $ Str.drop n text
            else
              (Str.take 80 $ Str.drop n text) <> "-"



---------------------

currInput = lens _.currInput (_ { currInput = _ })
currLine = lens _.currLine (_ { currLine = _ })
code = lens _.code (_ { code = _ })
inputs = lens _.inputs (_ { inputs = _ })
