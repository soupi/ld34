module Input where

import Prelude

import Data.Foldable
import Data.Traversable
import Control.Apply
import Control.Monad.Eff
import Signal as S
import Signal.DOM as S

import Utils

input = do
  frames <- S.animationFrame
  arrowsInputs <- arrows
  pure (const <$> arrowsInputs <*> frames)


arrows = do
  leftInput  <- S.keyPressed leftKeyCode
  rightInput <- S.keyPressed rightKeyCode
  upInput    <- S.keyPressed upKeyCode
  downInput  <- S.keyPressed downKeyCode
  let asNum b = if b then 1.0 else 0.0
  pure $  (\l r u d -> { x: asNum r - asNum l, y: asNum d - asNum u } )
      <$> leftInput
      <*> rightInput
      <*> upInput
      <*> downInput

leftKeyCode = 37
rightKeyCode = 39
upKeyCode = 38
downKeyCode = 40


