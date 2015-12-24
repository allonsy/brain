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

type InputStack = (String, String)

type BrainState = StateT (InputStack, Deque Int) IO

accepted :: [Char]
accepted = ['+', '-', '>', '<', '.', ',', '[', ']']

zeroList :: [Int]
zeroList = 0 : zeroList

blankDeque :: Deque Int
blankDeque = Deque zeroList 0 zeroList

increment :: BrainState ()
increment = do
  (i, d) <- get
  put $ (i, Deque (top d) ((pointer d) +1) (bottom d))

decrement :: BrainState ()
decrement = do
  (i,d) <- get
  put $ (i , Deque (top d) ((pointer d) -1) (bottom d))

moveDequeRight :: Deque a -> Deque a
moveDequeRight (Deque t p (b : bs)) = Deque (p:t) b bs

moveRight :: BrainState ()
moveRight = do
  (i,d) <- get
  put $ (i, moveDequeRight d)

moveDequeLeft :: Deque a -> Deque a
moveDequeLeft (Deque (t:ts) p b) = Deque ts t (p:b)

moveLeft :: BrainState ()
moveLeft = do
  (i,d) <- get
  put $ (i, moveDequeLeft d)

consume :: BrainState ()
consume = do
  ((con, uncon),d) <- get
  debug $ "Consuming " ++ (show (head uncon))
  put $ (((head uncon):con, tail uncon), d)

unconsume :: BrainState ()
unconsume = do
  ((con, uncon), d) <- get
  put $ ((tail con, (head con):uncon), d)

debug :: String -> BrainState ()
debug message = liftIO $ putStrLn message

printChar :: BrainState ()
printChar = do
  (i, d) <- get
  let toPrint = pointer d
  let charToPrint = chr toPrint
  liftIO $ putChar charToPrint

getBrainChar :: BrainState ()
getBrainChar = do
  (i,d) <- get
  liftIO $ putStrLn "Here!"
  c <- liftIO $ getChar
  let intVal = ord c
  put $ (i,Deque (top d) intVal (bottom d))

isZero :: Deque Int -> Bool
isZero (Deque t 0 b) = True
isZero _ = False

isNotZero :: Deque Int -> Bool
isNotZero d = not $ isZero d

executeBrainState :: BrainState ()
executeBrainState = do
  (i,d) <- get
  if (snd i == []) then return ()
    else executeChar >> executeBrainState

executeChar :: BrainState ()
executeChar = do
  (i,d) <- get
  executeHelper i

executeHelper :: InputStack -> BrainState ()
executeHelper (con, []) = error "Empty"
executeHelper (con, '>':uncon) = do
  moveRight
  consume
executeHelper (con, '<':uncon) = do
  moveLeft
  consume
executeHelper (con, '.':uncon) = do
  printChar
  consume
executeHelper (con, ',':uncon) = do
  getBrainChar
  consume
executeHelper (con, '+':uncon) = do
  increment
  consume
executeHelper (con, '-':uncon) = do
  decrement
  consume
executeHelper (con, '[':uncon) = do
  moveForward
executeHelper (con, ']':uncon) = do
  moveBack
executeHelper _ = error "undefined"

moveForward :: BrainState ()
moveForward = do
  (i, d) <- get
  if isZero d then do
    spinForward
  else do
    consume
    executeChar

spinForward :: BrainState ()
spinForward = do
  ((con, uncon), d) <- get
  if (head uncon) == ']' then do
    debug "caught"
    consume
    executeChar
  else debug "next" >> consume >> spinForward

moveBack :: BrainState ()
moveBack = do
  (i,d) <- get
  if isNotZero d then do
    spinBack
  else do
    consume
    executeChar
spinBack :: BrainState ()
spinBack = do
  ((con, uncon), d) <- get
  if (head con) == '[' then do
    consume
    executeChar
  else unconsume >> spinBack

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
  _ <- runStateT (executeBrainState) (([], program),blankDeque)
  return ()
