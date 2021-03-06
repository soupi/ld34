module Machine where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Either
import Data.List
import Data.String as S
import Data.Lens
import Control.Bind

import Utils
import Zipper

import Debug.Trace

type Machine =
  { code   :: Zipper Instruction
  , stack  :: List Int
  , input  :: List Int
  , output :: List Int
  }

mkMachine :: List Instruction -> List Int -> Maybe Machine
mkMachine inst input =
  case uncons inst of
    Nothing ->
      Nothing
    Just {head: x, tail: xs} ->
      Just { code: zipper x Nil xs
           , stack: Nil
           , input: input
           , output: Nil
           }


showMachine :: Machine -> String
showMachine machine =
    "Code" <> "\nStack: "
    <> show machine.stack  <> "\nInput: "
    <> show machine.input <> "\nOutput: "
    <> show machine.output <> "\n"

---

code   = lens _.code   (_ { code   = _ })
stack  = lens _.stack  (_ { stack  = _ })
input  = lens _.input  (_ { input  = _ })
output = lens _.output (_ { output = _ })

---

data Instruction
  = HALT -- stops
  | PRINT -- print top value on the stack (push to output)
  | LOADVAL Int -- pushes an imediate value to the stack
  | LOADIN -- push value from input to the stack
  | POP -- discard from stack
  | JUMP -- go foreward N times in code, N = top of the stack (Negative N means backwards)
  | JUMPIF -- JUMPs if value before top of the stack is zero
  | COMPARE -- compare two values of the top of the stack - zero means equal
  | ADD -- adds top two on stack
  | EMPTY -- pushs zero on the stack if it is empty
  | DUP -- duplicates top element

data Error
  = StackUnderflow
  | UnknownInstruction
  | InputError

instance showError :: Show Error where
  show StackUnderflow = "Stack Underflow"
  show UnknownInstruction = "Unknown Instruction"
  show InputError = "Input Error"

throwErr :: Error -> Either Error _
throwErr = Left

eval :: Machine -> Either Error Machine
eval machine =
  movePC =<<
    case current machine.code of
        HALT ->
          pure machine
        JUMP ->
          pure machine
        JUMPIF ->
          pure machine
        DUP ->
          maybe (throwErr StackUnderflow) (\x -> pure $ over stack (Cons x) machine) (head machine.stack)
        PRINT ->
          maybe (throwErr StackUnderflow) (\x -> pure $ over output (Cons x) machine) (head machine.stack)
        EMPTY ->
          pure $ over stack (\s -> if null s then Cons 0 s else Cons 1 s) machine
        LOADVAL n ->
          pure $ over stack (Cons n) machine
        LOADIN -> fromMaybe (throwErr InputError) do
          {head: x, tail: xs} <- uncons machine.input
          pure $ pure $ set input xs $ over stack (Cons x) machine
        POP ->
          maybe (throwErr StackUnderflow) (\s -> pure $ set stack s machine) (tail machine.stack)
        ADD ->
          maybe (throwErr StackUnderflow)
                (\(Tuple x y) -> pure $ over stack (Cons (if x + y > 15 then x + y - 15 else if x + y < -15 then 15 - (x+y) else x+y) <<< fromJust <<< tail <<< fromJust <<< tail) machine)
                (takeTwo machine.stack)
        COMPARE ->
          maybe (throwErr StackUnderflow)
                (\(Tuple x y) -> pure $ over stack (Cons (if x == y then 0 else if x > y then 1 else (-1))) machine)
                (takeTwo machine.stack)



takeTwo :: forall a. List a -> Maybe (Tuple a a)
takeTwo (Cons x (Cons y _)) = Just $ Tuple x y
takeTwo _ = Nothing

movePC :: Machine -> Either Error Machine
movePC machine =
  case current machine.code of
    HALT ->
      pure $ machine
    PRINT ->
      pure $ over code (moveBy 1) machine
    LOADVAL _ ->
      pure $ over code (moveBy 1) machine
    LOADIN ->
      pure $ over code (moveBy 1) machine
    POP ->
      pure $ over code (moveBy 1) machine
    JUMP ->
      maybe (throwErr StackUnderflow) (\x -> pure $ over stack (fromJust <<< tail) $ over code (moveBy x) machine) (head machine.stack)
    COMPARE ->
      pure $ over code (moveBy 1) machine
    JUMPIF ->
      maybe (throwErr StackUnderflow) (\(Tuple a b) -> pure $ over stack (fromJust <<< tail) $ over code (if b /= 0 then moveBy a else moveBy 1) machine) (takeTwo machine.stack)
    ADD ->
      pure $ over code (moveBy 1) machine
    EMPTY ->
      pure $ over code (moveBy 1) machine
    DUP ->
      pure $ over code (moveBy 1) machine


machineTest :: Machine
machineTest =
  { code: zipper (LOADVAL 5) Nil (Cons PRINT $ Cons HALT Nil)
  , stack: Nil
  , input: Nil
  , output: Nil
  }

moveBy :: forall a. Int -> Zipper a -> Zipper a
moveBy n zipp
  | n >  0 = moveBy (n-1) (snd $ next zipp)
  | n <  0 = moveBy (n+1) (snd $ back zipp)
  | otherwise = zipp


halted :: Machine -> Boolean
halted machine =
  case current machine.code of
    HALT -> true
    _    -> isLeft $ eval machine

hasErrors :: Machine -> Maybe Error
hasErrors machine =
  case eval machine of
    Left err -> Just err
    Right _  -> Nothing

translate :: String -> Either Error Instruction
translate txt =
  case Tuple (S.take 3 txt) (S.drop 3 txt) of
    Tuple "111" "11111" ->
      pure HALT
    Tuple "010" "10101" ->
      pure PRINT
    Tuple "000" "00000" ->
      pure EMPTY
    Tuple "100" str ->
      LOADVAL <$> strBinToInt str
    Tuple "000" "10000" ->
      pure LOADIN
    Tuple "001" "10000" ->
      pure POP
    Tuple "000" "10001" ->
      pure JUMP
    Tuple "000" "10101" ->
      pure JUMPIF
    Tuple "001" "10011" ->
      pure COMPARE
    Tuple "010" "01000" ->
      pure ADD
    Tuple "110" "01100" ->
      pure DUP
    _ ->
      throwErr UnknownInstruction



strBinToInt :: String -> Either Error Int
strBinToInt str = fromMaybe (Left UnknownInstruction) $ map Right do
  {head: n1, tail: str1} <- S.uncons str
  {head: n2, tail: str2} <- S.uncons str1
  {head: n3, tail: str3} <- S.uncons str2
  {head: n4, tail: str4} <- S.uncons str3
  {head: n5, tail: str5} <- S.uncons str4
  r1 <- if n1 == '0' then Just 1 else if n1 == '1' then Just (-1) else Nothing
  r2 <- if n2 == '0' then Just 0 else if n2 == '1' then Just 1 else Nothing
  r3 <- if n3 == '0' then Just 0 else if n3 == '1' then Just 1 else Nothing
  r4 <- if n4 == '0' then Just 0 else if n4 == '1' then Just 1 else Nothing
  r5 <- if n5 == '0' then Just 0 else if n5 == '1' then Just 1 else Nothing
  pure $ (r5 * 1 + r4 * 2 + r3 * 4 + r2 * 8) * r1

