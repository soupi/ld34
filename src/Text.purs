module TextBar where

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

import CanvasUtils
import Input as I


-----------
-- Model
-----------

position :: Point
position = { x: 20.0, y: height - 250.0 }

size :: Point
size = { x: width - 40.0, y: 200.0 }

data State
  = ShowAll
  | ShowNothing
  | ShowSome String

type TextBar =
  { pos :: Point
  , size  :: Point
  , speed :: Number
  , text :: String
  , bgcolor :: String
  , color :: String
  , state :: State
  }

mkText :: String -> TextBar
mkText str =
    { pos: position
    , size: size
    , speed: 1.0
    , bgcolor: "#1B1C1B"
    , color: "white"
    , text: str
    , state: ShowAll
    }



------------
-- Update
------------

update :: Point -> TextBar -> TextBar
update _ bar =
  bar

------------
-- Render
------------

render :: C.Context2D -> TextBar -> Eff ( canvas :: C.Canvas | _) Unit
render ctx bar = do
  case bar.state of
    ShowAll -> do
     C.setFillStyle bar.bgcolor ctx
     C.fillRect ctx { x: bar.pos.x , y: bar.pos.y
                    , w: bar.size.x, h: bar.size.y }
     C.setFillStyle bar.color ctx
     C.fillText ctx bar.text (bar.pos.x + 20.0) (bar.pos.y + 20.0)
     pure unit
    _ ->
     pure unit
  pure unit

