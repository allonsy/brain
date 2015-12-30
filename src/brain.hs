module Main where

{- Alec Snyder
 - brainfuck interpreter
-}
import Control.Monad.Trans.State.Lazy
import System.IO
import Data.Char
import Control.Monad.IO.Class
import System.Environment
import Control.Monad

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

consume :: BrainState Char
consume = do
  ((con, uncon),d) <- get
  put $ (((head uncon):con, tail uncon), d)
  return $ head uncon

unconsume :: BrainState Char
unconsume = do
  ((con, uncon), d) <- get
  put $ ((tail con, (head con):uncon), d)
  return $ head con

isEnd :: BrainState Bool
isEnd = do
  ((con,uncon),d) <- get
  return $ uncon == []

-- debug :: String -> BrainState ()
-- debug message = liftIO $ putStrLn message

printChar :: BrainState ()
printChar = do
  (i, d) <- get
  let toPrint = pointer d
  let charToPrint = chr toPrint
  liftIO $ putChar charToPrint

getBrainChar :: BrainState ()
getBrainChar = do
  (i,d) <- get
  c <- liftIO $ getChar
  let intVal = ord c
  put $ (i,Deque (top d) intVal (bottom d))

isZero :: Deque Int -> Bool
isZero (Deque t 0 b) = True
isZero _ = False

isNotZero :: Deque Int -> Bool
isNotZero d = not $ isZero d

isBrainZero :: BrainState Bool
isBrainZero = do
  (i,d) <- get
  return $ isZero d

isBrainNotZero :: BrainState Bool
isBrainNotZero = do
  (i,d) <- get
  return $ isNotZero d


executeBrainState :: BrainState ()
executeBrainState = do
  val <- isEnd
  if val then do
    return ()
  else do
    ch <- consume
    executeChar ch
    executeBrainState

executeChar :: Char -> BrainState ()
executeChar '>' = do
  moveRight
executeChar '<' = do
  moveLeft
executeChar '.' = do
  printChar
executeChar ',' = do
  getBrainChar
executeChar '+' = do
  increment
executeChar '-' = do
  decrement
executeChar '[' = do
  val <- isBrainZero
  if val then do
    spin consume ['['] False
  else
    return ()
executeChar ']' = do
  val <- isBrainNotZero
  if val then do
    _ <- unconsume
    spin unconsume [']'] True
  else
    return ()
executeChar _ = error "undefined"

stackBracket :: Char -> [Char] -> [Char]
stackBracket _ [] = error "The bracket system has encountered an error"
stackBracket '[' (b:bs)
  | b == ']' = bs
  | otherwise = '[' : b : bs
stackBracket ']' (b:bs)
  | b == '[' = bs
  | otherwise = ']' : b : bs
stackBracket _ bs = bs

spin :: BrainState Char -> [Char] -> Bool -> BrainState ()
spin f ls garbage = do
  readHead <- f
  let newLS = stackBracket readHead ls
  if newLS == [] then do
    when garbage $ do
      _ <- consume
      return ()
    return ()
  else spin f newLS garbage

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
