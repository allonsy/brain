module Main where

{- Alec Snyder
 - brainfuck interpreter
-}
import Control.Monad.Trans.State.Lazy
import System.IO
import Data.Char
import Control.Monad.IO.Class
import System.Environment

data Deque a = Deque {
  top :: [a],
  pointer :: a,
  bottom :: [a]
} deriving(Show)

type BrainState = StateT (Deque Int) IO

accepted :: [Char]
accepted = ['+', '-', '>', '<', '.', ',', '[', ']']

zeroList :: [Int]
zeroList = 0 : zeroList

blankDeque :: Deque Int
blankDeque = Deque zeroList 0 zeroList

increment :: BrainState ()
increment = do
  d <- get
  put $ Deque (top d) ((pointer d) +1) (bottom d)

decrement :: BrainState ()
decrement = do
  d <- get
  put $ Deque (top d) ((pointer d) -1) (bottom d)

moveDequeRight :: Deque a -> Deque a
moveDequeRight (Deque t p (b : bs)) = Deque (p:t) b bs

moveRight :: BrainState ()
moveRight = do
  d <- get
  put $ moveDequeRight d

moveDequeLeft :: Deque a -> Deque a
moveDequeLeft (Deque (t:ts) p b) = Deque ts t (p:b)

moveLeft :: BrainState ()
moveLeft = do
  d <- get
  put $ moveDequeLeft d

printChar :: BrainState ()
printChar = do
  d <- get
  let toPrint = pointer d
  let charToPrint = chr toPrint
  liftIO $ putChar charToPrint

getBrainChar :: BrainState ()
getBrainChar = do
  d <- get
  c <- liftIO $ getChar
  let intVal = ord c
  put $ Deque (top d) intVal (bottom d)

isZero :: Deque Int -> Bool
isZero (Deque t 0 b) = True
isZero _ = False

isNotZero :: Deque Int -> Bool
isNotZero d = not $ isZero d

executeBrain :: String -> BrainState ()
executeBrain [] = return ()
executeBrain (x:xs)
  | x == '+' = increment    >> executeBrain xs
  | x == '-' = decrement    >> executeBrain xs
  | x == '.' = printChar    >> executeBrain xs
  | x == ',' = getBrainChar >> executeBrain xs
  | x == '<' = moveLeft     >> executeBrain xs
  | x == '>' = moveRight    >> executeBrain xs
  | x == '[' = do
    d <- get
    if isZero d
      then executeBrain $ skipBracket xs
      else do
        nextInst <- executeBracket xs xs
        executeBrain nextInst
  | x == ']' = error "Extra closed bracket included!"
  | otherwise = error "Invalid Character"

executeBracket :: String -> String -> BrainState String
executeBracket str [] = error "Missing closing bracket"
executeBracket str (x:xs)
  | x /= '[' && x /= ']' = executeBrain [x] >> executeBracket str xs
  | x == ']' = do
    d <- get
    if isZero d
      then return xs
      else executeBracket str str
  | x == '[' = do
    nextInst <- executeBracket xs xs
    executeBracket str nextInst

skipBracket :: String -> String
skipBracket str = skipBracket' str 1 where
  skipBracket' str 0 = str
  skipBracket' [] _ = error "Closing bracket not found!"
  skipBracket' (x:xs) n
    | x == '[' = skipBracket' xs (n+1)
    | x == ']' = skipBracket' xs (n-1)
    | otherwise = skipBracket' xs n

format :: String -> String
format = filter (\x -> x `elem` accepted)

validate :: [String] -> IO ()
validate [] = error "Please provide a filename as an argument!"
validate _ = return ()

main :: IO ()
main = do
  args <- getArgs
  validate args
  prog <- readFile $ head args
  let program = format prog
  _ <- runStateT (executeBrain program) blankDeque
  return ()
