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

testCollisionWith :: forall a b. Array { collision :: Maybe Point, size :: Point, pos :: Point | a }
                  -> Array { collision :: Maybe Point, size :: Point, pos :: Point | b }
                  -> Array { collision :: Maybe Point, size :: Point, pos :: Point | a }
testCollisionWith objs1 objs2 =
  map (\x -> set collision (foldl addCollisions Nothing $ map (collisionDetection x) objs2) x) objs1

collisionDetection :: forall a b. { size :: Point, pos :: Point | a }
          -> { size :: Point, pos :: Point | b }
          -> Maybe Point
collisionDetection a b =
  if testCollision a b then collisionDirection a b else Nothing

testCollision :: forall a b. { size :: Point, pos :: Point | a }
              -> { size :: Point, pos :: Point | b }
              -> Boolean
testCollision a b =
  if
      a.pos.x >= b.pos.x + b.size.x
   || a.pos.y >= b.pos.y + b.size.y
   || a.pos.x + a.size.x <= b.pos.x
   || a.pos.y + a.size.y <= b.pos.y
  then
    false
  else
    true

cornerRects :: forall r. { size :: Point, pos :: Point | r }
        -> Array { size :: Point, pos :: Point }
cornerRects obj =
  let size = { x: obj.size.x / 2.0, y: obj.size.y / 2.0 }
  in
    [ { pos: makePoint obj.pos.x obj.pos.y, size: size }
    , { pos: makePoint (obj.pos.x + obj.size.x / 2.0) obj.pos.y, size: size }
    , { pos: makePoint obj.pos.x (obj.pos.y + obj.size.y / 2.0), size: size }
    , { pos: makePoint (obj.pos.x + obj.size.x / 2.0) (obj.pos.y + obj.size.y / 2.0), size: size }
    ]

corners :: forall r. { size :: Point, pos :: Point | r }
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


collisionDirection :: forall a b. { size :: Point, pos :: Point | a }
                   -> { size :: Point, pos :: Point | b }
                   -> Maybe Point
collisionDirection a b =
  foldl addCollisions Nothing $
  zipWith (\result test -> if test then Just result else Nothing)
          [makePoint (-1.0) (-1.0), makePoint 1.0 (-1.0), makePoint (-1.0) 1.0, makePoint 1.0 1.0]
          (map (testCollision b) (cornerRects a))

addCollisions :: Maybe Point -> Maybe Point -> Maybe Point
addCollisions Nothing a = a
addCollisions a Nothing = a
addCollisions (Just a) (Just b) =
  let x = if a.x == 0.0 then 0.0 else if a.x == b.x then a.x else a.x + b.x
      y = if a.y == 0.0 then 0.0 else if a.y == b.y then a.y else a.y + b.y
  in
      Just { x: x, y: y }

------------
-- Lenses
------------

collision = lens _.collision (_ { collision = _ })
pos = lens _.pos (_ { pos = _ })

