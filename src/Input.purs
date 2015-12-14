module Input where

import Prelude hiding (zero, one)

import Data.Lens
import Data.Int
import Data.Maybe
import Control.Apply
import Signal as S
import Signal.Time as S
import Signal.DOM as S

import CanvasUtils
import Utils

input = do
  frames <- S.animationFrame
  let time = S.every (S.millisecond * 17.0)
  arrowsInputs <- arrows
  sdir <- screenDirection
  z <- zero'
  o <- one'
  mouse <- mouseClick
  winDim <- S.windowDimensions
  ioSignal <- io
  rt <- tKey
  pure $ S.sampleOn frames
        (buildInput
         <$> time
         <*> arrowsInputs
         <*> sdir
         <*> once z
         <*> once o
         <*> mouse
         <*> winDim
         <*> ioSignal
         <*> rt)

type Input
  = { arrows  :: Point
    , time :: S.Time
    , screenDir :: Number
    , zeroOne :: ZeroOne
    , mouseClick :: Maybe Point
    , io :: Tuple Boolean Boolean
    , runTests :: Boolean
    }

buildInput :: S.Time -> Point -> Number -> Boolean -> Boolean -> Maybe Point -> S.DimensionPair -> Tuple Boolean Boolean -> Boolean -> Input
buildInput t a sdir z o m winDim io rt =
  { arrows: a
  , time: t
  , screenDir: sdir
  , zeroOne: { zero: z, one: o }
  , io: io
  , runTests: rt
  , mouseClick: map (\p -> { x: p.x - ((toNumber winDim.w - width) / 2.0)
                           , y: p.y - 5.0 }) m
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
escKeyCode = 27

iKeyCode = 73
oKeyCode = 79

tKeyCode = 84

tKey = do
  once <$> S.keyPressed tKeyCode

io = do
  i <- S.keyPressed iKeyCode
  o <- S.keyPressed oKeyCode
  pure $ Tuple <$> once i <*> once o

screenDirection = do
  foreward <- S.keyPressed enterKeyCode
  back <- S.keyPressed escKeyCode
  pure $  (\f b -> asNum f - asNum b)
      <$> once foreward
      <*> once back

asNum b = if b then 1.0 else 0.0

type ZeroOne
  = { zero :: Boolean, one :: Boolean }

zero' = do
  z  <- S.keyPressed 48
  z2 <- S.keyPressed 96
  pure $  (||)
      <$> z
      <*> z2

one' = do
  o   <- S.keyPressed 49
  o2  <- S.keyPressed 97
  pure $  (||)
      <$> o
      <*> o2

mouseClick = do
  pos <- S.mousePos
  down <- S.mouseButton 0
  pure $ (\p d -> if d then Just { x: toNumber p.x, y: toNumber p.y } else Nothing)
      <$> pos
      <*> once down


once :: S.Signal Boolean -> S.Signal Boolean
once sig =
  (&&) <$> sig
       <*> (S.since (S.millisecond * 10.0) $ S.dropRepeats sig)


zeroOne = lens _.zeroOne (_ { zeroOne = _ })
zero = lens _.zero (_ { zero = _ })
one = lens _.one (_ { one = _ })
