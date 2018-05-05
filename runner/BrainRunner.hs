module BrainRunner (
  execute
)
where

import Brain
import Control.Monad.State.Lazy
import Data.Word
import Data.Char

type CellType = Word8

baseValue :: CellType
baseValue = 0

data Tape = Tape {
  leftSlice :: [CellType],
  center :: CellType,
  rightSlice :: [CellType]
}

data BrainState = BrainState {
  tape :: Tape
}

type BrainRunner a = StateT BrainState IO a

startingTape :: Tape
startingTape = Tape [] baseValue []

leftShift :: BrainRunner ()
leftShift = do
  st <- get
  let oldTape = tape st
  let newTape = case leftSlice oldTape of
        [] -> Tape [] baseValue (center oldTape : rightSlice oldTape)
        (x:xs) -> Tape xs x (center oldTape : rightSlice oldTape)
  let newState = st{tape=newTape}
  put newState
  return ()

rightShift :: BrainRunner ()
rightShift = do
  st <- get
  let oldTape = tape st
  let newTape = case rightSlice oldTape of
        [] -> Tape (center oldTape : leftSlice oldTape) baseValue []
        (x:xs) -> Tape (center oldTape : leftSlice oldTape) x xs
  let newState = st{tape=newTape}
  put newState
  return ()

getCenter :: BrainRunner CellType
getCenter = do
  st <- get
  let brainTape = tape st
  return $ center brainTape

setCenter :: CellType -> BrainRunner ()
setCenter newCenter = do
  st <- get
  let oldTape = tape st
  let newTape = oldTape {center=newCenter}
  put (st {tape=newTape})

increment :: BrainRunner ()
increment = do
  oldCenter <- getCenter
  setCenter (oldCenter + 1)

decrement :: BrainRunner ()
decrement = do
  oldCenter <- getCenter
  setCenter (oldCenter - 1)

outputCenter :: BrainRunner ()
outputCenter = do
  tapeCenter <- getCenter
  let outputChar = chr $ fromEnum tapeCenter
  lift $ putStr [outputChar]

inputCenter :: BrainRunner ()
inputCenter = do
  newCenter <- lift $ getChar
  setCenter $ toEnum $ ord $ newCenter

executeBlock :: [BrainAST] -> BrainRunner ()
executeBlock = mapM_ executeCommand

executeCommand :: BrainAST -> BrainRunner ()
executeCommand RightShift = rightShift
executeCommand LeftShift = leftShift
executeCommand Increment = increment
executeCommand Decrement = decrement
executeCommand Output = outputCenter
executeCommand Input = inputCenter
executeCommand cmd@(Bracket cmds) = do
  cen <- getCenter
  if cen == 0 then return ()
  else do
    executeBlock cmds
    executeCommand cmd

execute :: [BrainAST] -> IO ()
execute cmds = do
  let bstate = BrainState startingTape
  evalStateT (executeBlock cmds) bstate
