module Screen where

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


-----------
-- Model
-----------

type Screen
  = { textbar :: T.TextBar
    , gfx :: String -- color for now
    }

mkScreen :: T.TextBar -> Screen
mkScreen t =
  { textbar: t
  , gfx: "#888"
  }

scrErr :: Screen
scrErr =
  { textbar: T.mkText "No screens available"
  , gfx: "#900"
  }

screens :: String -> Zipper Screen
screens txts =
  case List.toList $ scrArr txts of
    List.Nil ->
      zipper scrErr List.Nil List.Nil
    List.Cons x xs ->
      zipper x List.Nil xs

scrArr :: String -> Array Screen
scrArr txts =
  (map (mkScreen <<< T.mkText) <<< filter (/="") <<< split "\n\n") txts


intro :: String
intro =
  """Welcome to <Company>.

For the past 20 our scientists and engineers have been working on a top secret project.

At last, they have succeeded in creating a marvelous machine, a computing machine.

We call it 'The Computing Machine'.

You had the fortune to be selected as one of the chosen few to operate The Computing Machine.

At <Company>, we have a lot of challenges waiting to be solved.

Fortunately for you, The Computing Machine is really simple to operate, it only has two buttons!

All you have to do is insert the right combination of the two buttons, and The Computing Machine will do the rest!

Good Luck!"""

------------
-- Render
------------

renderScreen :: C.Context2D -> Screen -> Eff ( canvas :: C.Canvas | _) Unit
renderScreen context screen = do
  C.setFillStyle screen.gfx context
  C.fillRect context { x: 0.0, y: 100.0, w: width, h: height - 200.0 }
  T.render context screen.textbar
  pure unit

