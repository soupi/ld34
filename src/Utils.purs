module Utils where

data Tuple a b
  = Tuple a b

fst :: forall a b. Tuple a b -> a
fst (Tuple x _) = x
snd :: forall a b. Tuple a b -> b
snd (Tuple _ x) = x

