module Zipper where

import Prelude
import Data.List

import Utils

data Zipper a
  = Zipper a (List a) (List a)

zipper :: forall a. a -> List a -> List a -> Zipper a
zipper = Zipper

current :: forall a. Zipper a -> a
current (Zipper x _ _) = x

next :: forall a. Zipper a -> Tuple Boolean (Zipper a)
next zipp@(Zipper curr before after) =
  case after of
    Nil ->
      Tuple false zipp
    Cons x xs ->
      Tuple true $ Zipper x (Cons curr before) xs

back :: forall a. Zipper a -> Tuple Boolean (Zipper a)
back zipp@(Zipper curr before after) =
  case before of
    Nil ->
      Tuple false zipp
    Cons x xs ->
      Tuple true $ Zipper x xs (Cons curr after)

start :: forall a. Zipper a -> Zipper a
start zipp@(Zipper _ Nil _) = zipp
start zipp = start (snd $ back zipp)
