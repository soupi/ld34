module Collisions where

import Prelude

import Data.Lens
import Data.Array
import Data.Maybe
import Math
import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Graphics.Canvas as C
import Signal as S
import Signal.DOM as S

import Utils

----------------
-- Collisions
----------------

testCollisionWith :: forall a b. Array { collision :: Boolean, size :: Point, pos :: Point | a }
                  -> Array { collision :: Boolean, size :: Point, pos :: Point | b }
                  -> Array { collision :: Boolean, size :: Point, pos :: Point | a }
testCollisionWith objs1 objs2 =
  map (\x -> set collision (any id $ map (testCollision x) objs2) x) objs1

testCollision :: forall a b. { collision :: Boolean, size :: Point, pos :: Point | a }
              -> { collision :: Boolean, size :: Point, pos :: Point | b }
              -> Boolean
testCollision x1 x2 = any id $
     map (flip pointInRect x2) (corners x1)
  <> map (flip pointInRect x1) (corners x2)

corners :: forall r. { collision :: Boolean, size :: Point, pos :: Point | r }
        -> Array Point
corners obj =
  [ makePoint obj.pos.x obj.pos.y
  , makePoint (obj.pos.x + obj.size.x) obj.pos.y
  , makePoint obj.pos.x (obj.pos.y + obj.size.y)
  , makePoint (obj.pos.x + obj.size.x) (obj.pos.y + obj.size.y)
  ]

pointInRect :: forall r. Point
            -> { pos :: Point, size :: Point | r }
            -> Boolean
pointInRect p obj =
     (obj.pos.x <= p.x && p.x <= obj.pos.x + obj.size.x)
  && (obj.pos.y <= p.y && p.y <= obj.pos.y + obj.size.y)

------------
-- Lenses
------------

collision = lens _.collision (_ { collision = _ })
pos = lens _.pos (_ { pos = _ })

