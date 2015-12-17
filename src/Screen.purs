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
    , gfx :: Maybe C.CanvasImageSource
    }

mkScreen :: C.CanvasImageSource -> T.TextBar -> Screen
mkScreen img t =
  { textbar: t
  , gfx: Just img
  }

scrErr :: Screen
scrErr =
  { textbar: T.mkText "No screens available"
  , gfx: Nothing
  }

screens :: C.CanvasImageSource -> String -> Zipper Screen
screens img txts =
  case List.toList $ scrArr img txts of
    List.Nil ->
      zipper scrErr List.Nil List.Nil
    List.Cons x xs ->
      zipper x List.Nil xs

scrArr :: C.CanvasImageSource -> String -> Array Screen
scrArr img txts =
  (map (mkScreen img <<< T.mkText) <<< filter (/="") <<< split "\n\n") txts


intro :: String
intro =
  """Welcome to CompCompany.

For the past 40 years our scientists and engineers have been working on a top
secret project.

At last, they have succeeded in creating a marvelous machine,
a computing machine.

We call it 'The Computing Machine'.

You had the fortune to be selected as one of the chosen few to operate The Computing Machine.

At CompCompany, we have a lot of challenges waiting to be solved.

Fortunately for you, The Computing Machine is really simple to operate, it only has two buttons!

All you have to do is insert the right combination of the two buttons, and The Computing Machine will do the rest!

Our engineering team will explain the rest.

Good Luck!

...

.....

Alright! We are going to give you a few missions now.

Just write your solution with the 0 and 1 buttons and press the TEST button (or press T on your keyboard)
For us to test your solution. If you pass our test, we will give you another mission.

If you just want to try things yourself, use the RUN button (or press Enter) to test things yourself.

To enter input or view output for yourself, just touch the paper slips
on the side of The Computing Machine.

And if anything goes wrong, you can always press the RESET or POWER button (or Esc) to go back
or start fresh.

That's it.

Oh, and just scroll to the Machine Manual at the bottom of the page.
Make sure to end all your programs with HALT. If your program does not stop,
just power the machine down and try again.

Good luck!
"""

------------
-- Render
------------

renderScreen :: C.Context2D -> Screen -> Eff ( canvas :: C.Canvas | _) Unit
renderScreen context screen = do
  case screen.gfx of
    Just gfx -> do
      C.drawImage context gfx 0.0 0.0
      pure unit
    Nothing -> do
      C.setFillStyle "#900" context
      C.fillRect context { x: 0.0, y: 100.0, w: width, h: height - 200.0 }
      pure unit
  T.render context screen.textbar
  pure unit

