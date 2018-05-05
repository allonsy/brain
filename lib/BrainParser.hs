module BrainParser(
  parseBrainAST
) where

import Brain
import Control.Monad.State.Lazy

data ParserState = ParserState {
  inputProgram :: [[Char]],
  location :: (Int, Int)
}

type BrainParser a = StateT ParserState (Either String) a

getNextChar :: BrainParser (Maybe Char)
getNextChar = do
  st <- get
  let (newChar, newState) = processChar (inputProgram st) st
  put newState
  return newChar

processChar :: [[Char]] -> ParserState -> (Maybe Char, ParserState)
processChar [] st = (Nothing, st)
processChar ([]:xs) st = processChar xs (st {inputProgram=xs, location=incrementRow (location st)})
processChar ((y:ys):xs) st = (Just y, st {inputProgram=ys:xs, location=incrementColumn (location st)})

incrementRow :: (Int, Int) -> (Int, Int)
incrementRow (x,y) = (x+1, y)

incrementColumn :: (Int, Int) -> (Int, Int)
incrementColumn (x,y) = (x, y+1)

runParseBrainAST :: BrainParser [BrainAST]
runParseBrainAST = do
  nextChar <- getNextChar
  case nextChar of
    Nothing -> return []
    Just ch -> do
      if isAcceptable ch then do
        cmd <- parseSingleChar ch
        rest <- runParseBrainAST
        return (cmd : rest)
      else
        runParseBrainAST

parseBrainAST :: [[Char]] -> Either String [BrainAST]
parseBrainAST program =
  evalStateT runParseBrainAST (ParserState program (0,0))

parseSingleChar :: Char -> BrainParser BrainAST
parseSingleChar '>' = return RightShift
parseSingleChar '<' = return LeftShift
parseSingleChar '+' = return Increment
parseSingleChar '-' = return Decrement
parseSingleChar '.' = return Output
parseSingleChar ',' = return Input
parseSingleChar '[' = do
  bracket <- parseBracket
  return $ Bracket bracket
parseSingleChar ']' = failParse $ "Found ']' without matching '['"
parseSingleChar c = failParse $ "Unknown Character: '" ++ [c] ++ "'"

parseBracket :: BrainParser [BrainAST]
parseBracket = do
  ch <- getNextChar
  case ch of
    Nothing -> failParse "Couldn't find matching ']'"
    (Just ']') -> return []
    (Just c) -> do
      if isAcceptable c then do
        bst <- parseSingleChar c
        restBst <- parseBracket
        return (bst : restBst)
      else
        parseBracket

failParse :: String -> BrainParser a
failParse str = do
  st <- get
  let (row, col) = location st
  let headerString = "Line: " ++ (show row) ++ ", Column: " ++ (show col)
  let errorString = unlines [headerString, str]
  lift $ Left errorString

isAcceptable :: Char -> Bool
isAcceptable '>' = True
isAcceptable '<' = True
isAcceptable '+' = True
isAcceptable '-' = True
isAcceptable '.' = True
isAcceptable ',' = True
isAcceptable '[' = True
isAcceptable ']' = True
isAcceptable _ = False
