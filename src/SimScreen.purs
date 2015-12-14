module SimScreen where

import Prelude

import Data.Int
import Data.Lens
import Data.Maybe
import Data.Traversable
import Data.List as List
import Data.String (split, length, take, drop)
import Control.Monad.Eff
import Control.Monad.Aff
import Graphics.Canvas as C

import CanvasUtils
import Zipper
import TextBar as T
import Input as I
import Machine as M

-- import Debug.Trace

-----------
-- Model
-----------

type SimScreen
  = { code :: List.List String
    , currLine :: String
    , gfx :: C.CanvasImageSource
    , compRun :: Boolean
    , inputs  :: List.List String
    , outputs :: List.List String
    , machine :: Maybe M.Machine
    , tests :: List.List { input :: List.List String, output :: List.List String }
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
      line = state.currLine <> if input.zeroOne.zero then "0" else "" <> if input.zeroOne.one then "1" else ""
  in
    if length line >= 8
    then over code (List.Cons (take 8 line)) $
         set currLine (drop 8 line) state
    else
         set currLine line state

------------
-- Render
------------

render :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
render ctx screen = do
  C.drawImage ctx screen.gfx 0.0 0.0
  C.setFillStyle "#CC3388" ctx

  sequence $ List.zipWith (\p txt -> C.fillText ctx txt p.x p.y) (map getPosition $ List.range 0 40) (List.reverse screen.code)

  C.setFillStyle "#DD4499" ctx
  C.fillText ctx ("> " <> screen.currLine) 350.0 (230.0 + 30.0 * 8.0)
  pure unit

getPosition i =
  { x: 350.0 + 100.0 * toNumber (i `div` 8)
  , y: 230.0 + 30.0  * toNumber (i `mod` 8)
  }


currLine = lens _.currLine (_ { currLine = _ })
code = lens _.code (_ { code = _ })
