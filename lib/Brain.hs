module Brain (
BrainAST(..)
) where

data BrainAST =
  RightShift
  | LeftShift
  | Increment
  | Decrement
  | Output
  | Input
  | Bracket [BrainAST]
  deriving(Show, Eq)
