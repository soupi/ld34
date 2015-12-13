module Input where

import Prelude

import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Signal as S
import Signal.Time as S
import Signal.DOM as S

import CanvasUtils

input = do
  frames <- S.animationFrame
  let time = S.every (S.millisecond * 17.0)
  arrowsInputs <- arrows
  sdir <- screenDirection
  pure (buildInput <$> time <*> arrowsInputs <*> sdir <*> frames)

type Input
  = { arrows  :: Point
    , time :: S.Time
    , screenDir :: Number
    }

buildInput :: S.Time -> Point -> Number -> _ -> Input
buildInput t a sdir _ =
  { arrows: a
  , time: t
  , screenDir: sdir
  }

arrows = do
  leftInput  <- S.keyPressed leftKeyCode
  rightInput <- S.keyPressed rightKeyCode
  upInput    <- S.keyPressed upKeyCode
  downInput  <- S.keyPressed downKeyCode
  pure $  (\l r u d -> { x: asNum r - asNum l, y: asNum d - asNum u } )
      <$> leftInput
      <*> rightInput
      <*> upInput
      <*> downInput

leftKeyCode = 37
rightKeyCode = 39
upKeyCode = 38
downKeyCode = 40

enterKeyCode = 13
backspaceKeyCode = 8

screenDirection = do
  foreward <- S.keyPressed enterKeyCode
  back <- S.keyPressed backspaceKeyCode
  pure $  (\f b -> asNum f - asNum b)
      <$> foreward
      <*> back

asNum b = if b then 1.0 else 0.0
