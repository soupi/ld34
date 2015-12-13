module SimScreen where

import Prelude

import Data.Maybe
import Data.Array
import Data.List as List
import Data.String (split)
import Control.Monad.Eff
import Control.Monad.Aff
import Graphics.Canvas as C

import CanvasUtils
import Zipper
import TextBar as T
import Input as I


-----------
-- Model
-----------

type SimScreen
  = { code :: List.List String
    , currLine :: String
    , gfx :: C.CanvasImageSource
    }

mkSimScreen :: C.CanvasImageSource -> SimScreen
mkSimScreen img =
  { code: List.Nil
  , gfx: img
  , currLine: ""
  }

mkCompScreen :: Aff _ SimScreen
mkCompScreen = do
  map mkSimScreen $ loadImageData "assets/comp.png"

done :: SimScreen -> Boolean
done _ = false

------------
-- Update
------------

update :: I.Input -> SimScreen -> SimScreen
update _ x = x

------------
-- Render
------------

render :: C.Context2D -> SimScreen -> Eff ( canvas :: C.Canvas | _) Unit
render context screen = do
  C.drawImage context screen.gfx 0.0 0.0
  pure unit

