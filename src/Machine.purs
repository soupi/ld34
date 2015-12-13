module Machine where

import Prelude
import Data.Maybe
import Data.Either
import Data.List
import Data.Lens
import Control.Bind

import Utils
import Zipper

type Machine =
  { code   :: Zipper Instruction
  , stack  :: List Int
  , input  :: List Int
  , output :: List Int
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
  | PUSHVAL Int -- pushes an imediate value to the stack
  | PUSHIN -- push value from input to the stack
  | POPIN -- discard from input
  | POP -- discard from stack
  | SKIP -- go foreward N times in code, N = top of the stack (Negative N means backwards)
  | SKIPIF -- SKIPs if value before top of the stack is zero
  | COMPARE -- compare two values of the top of the stack - zero means equal
  | ADD -- adds top two on stack
  | XOR -- XORs top two on stack
  | EMPTY -- pushs zero on the stack if it is empty

data Error
  = StackUnderflow
  | UnknownInstruction
  | InputError

instance showError :: Show Error where
  show StackUnderflow = "StackUnderflow"
  show UnknownInstruction = "UnknownInstruction"
  show InputError = "InputError"

throwErr :: Error -> Either Error _
throwErr = Left

eval :: Machine -> Either Error Machine
eval machine =
  movePC =<<
    case current machine.code of
        HALT ->
          pure machine
        SKIP ->
          pure machine
        SKIPIF ->
          pure machine
        PRINT ->
          maybe (throwErr StackUnderflow) (\x -> pure $ over output (Cons x) machine) (head machine.stack)
        EMPTY ->
          pure $ over stack (\s -> if null s then Cons 0 s else Cons 1 s) machine
        PUSHVAL n ->
          pure $ over stack (Cons n) machine
        PUSHIN ->
          maybe (throwErr InputError) (\x -> pure $ over stack (Cons x) machine) (head machine.input)
        POPIN  ->
          maybe (throwErr InputError) (\i -> pure $ set input i machine) (tail machine.input)
        POP ->
          maybe (throwErr StackUnderflow) (\s -> pure $ set stack s machine) (tail machine.stack)
        XOR ->
          maybe (throwErr StackUnderflow)
                (\(Tuple x y) -> pure $ over stack (Cons (x + y)) machine)
                (takeTwo machine.stack)
        ADD ->
          maybe (throwErr StackUnderflow)
                (\(Tuple x y) -> pure $ over stack (Cons (x + y)) machine)
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
    PUSHVAL _ ->
      pure $ over code (moveBy 1) machine
    PUSHIN ->
      pure $ over code (moveBy 1) machine
    POPIN ->
      pure $ over code (moveBy 1) machine
    POP ->
      pure $ over code (moveBy 1) machine
    SKIP ->
      maybe (throwErr StackUnderflow) (\x -> pure $ over code (moveBy x) machine) (head machine.stack)
    COMPARE ->
      pure $ over code (moveBy 1) machine
    SKIPIF ->
      maybe (throwErr StackUnderflow) (\x -> pure $ over code (if x == 0 then moveBy x else moveBy 1) machine) (head machine.stack)
    ADD ->
      pure $ over code (moveBy 1) machine
    XOR ->
      pure $ over code (moveBy 1) machine
    EMPTY ->
      pure $ over code (moveBy 1) machine


machineTest :: Machine
machineTest =
  { code: zipper (PUSHVAL 5) Nil (Cons PRINT $ Cons HALT Nil)
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
    _    -> false