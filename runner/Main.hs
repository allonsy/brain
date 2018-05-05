module Main where

import BrainParser
import qualified BrainRunner as BR
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fname = head args
  prog <- readFile fname
  let progLines = lines prog
  let parsedProgram = parseBrainAST progLines
  case parsedProgram of
    Left err -> putStrLn err
    Right cmds -> BR.execute cmds
