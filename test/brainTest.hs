module Main where
{-
 - Testing module
-}

import Brain
import Test.Hspec
import Test.QuickCheck
import Control.Monad.Trans.State.Lazy
import Control.Exception
import Control.Monad
import System.Process

checkFormat :: Bool
checkFormat = format test1 == test1 &&
  format test2 == "..><,,..>" &&
  format test3 == "..[]>+-..]]" &&
  format test4 == [] where
    test1 = "..>[]>>.,<+-"
    test2 = "he..><,,..>lo"
    test3 = "ja..[]>+-@m3..]]"
    test4 = []

checkRight :: IO Bool
checkRight = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  (i,d) <- execStateT moveRight (([],[]), blank)
  return $ d == Deque [4,1,2,3] 5 [6,7]

checkLeft :: IO Bool
checkLeft = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  (i,d) <- execStateT moveLeft (([],[]), blank)
  return $ d == Deque [2,3] 1 [4,5,6,7]

checkIncrement :: IO Bool
checkIncrement = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  (i,d) <- execStateT increment (([],[]), blank)
  return $ d == Deque [1,2,3] 5 [5,6,7]

checkDecrement :: IO Bool
checkDecrement = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  (i,d) <- execStateT decrement (([],[]), blank)
  return $ d == Deque [1,2,3] 3 [5,6,7]

checkConsume :: IO Bool
checkConsume = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  (i,((con, uncon),d)) <- runStateT consume (([],">...<"), blank)
  return $ i == '>' && con == ">" && uncon == "...<" && d == blank

checkSkipBracket :: IO Bool
checkSkipBracket = do
  let blank = Deque [1,2,3] 0 [5,6,7]
  (i,((con, uncon),d)) <- runStateT executeBrainState (([],"[[+]]"), blank)
  return $ i == () && con == "]]+[[" && uncon == "" && d == blank

checkLoops :: IO Bool
checkLoops = do
  let blank = Deque [1,2,3] 4 [5,6,7,0,8]
  (i,((con, uncon),d)) <- runStateT executeBrainState (([],"[>]"), blank)
  return $ i == () && con == "]>[" && uncon == "" && d == Deque [7,6,5,4,1,2,3] 0 [8]

checkUnconsume :: IO Bool
checkUnconsume = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  (i,((con, uncon),d)) <- runStateT unconsume ((">..<",">...<"), blank)
  return $ i == '>' && con == "..<" && uncon == ">>...<" && d == blank

checkUnmatchedEndBracket :: IO ()
checkUnmatchedEndBracket = do
  let blank = Deque [1,2,3] 0 [5,6,7]
  print "Begin"
  (i,d) <- runStateT executeBrainState (("","[++"), blank)
  when (i == ()) $ print "Hello again"
  print "End"
  return ()

checkUnmatchedBeginBracket :: IO ()
checkUnmatchedBeginBracket = do
  let blank = Deque [1,2,3] 4 [5,6,7]
  _ <- runStateT executeBrainState (("","++]"), blank)
  return ()

checkZero :: Bool
checkZero = isZero $ Deque [1,2,3] 0 [4,5,6]

checkNotZero :: Bool
checkNotZero = isNotZero $ Deque [1,2,3] 4 [5,6,7]

checkLikeStacks :: Bool
checkLikeStacks = stackBracket '[' "[" == "[[" &&
  stackBracket ']' "]" == "]]"

checkMatchingBrackets :: Bool
checkMatchingBrackets = stackBracket '[' "][[]]" == "[[]]" &&
  stackBracket ']' "[[[]]" == "[[]]"

main = do
  hspec $ describe "Testing program filtering" $ do
    it "should filter correctly" $ checkFormat `shouldBe` True

  hspec $ describe "Testing State operations" $ do
    it "should move right correctly"   $ checkRight     `shouldReturn` True
    it "should move left correctly"    $ checkLeft      `shouldReturn` True
    it "should increment correctly"    $ checkIncrement `shouldReturn` True
    it "should decrement correctly"    $ checkDecrement `shouldReturn` True
    it "should consume correctly"      $ checkConsume   `shouldReturn` True
    it "should unconsume correctly"    $ checkUnconsume `shouldReturn` True
    it "should show zero correctly"    $ checkZero      `shouldBe`     True
    it "shoule show nonzero correctly" $ checkNotZero   `shouldBe`     True

  hspec $ describe "Testing bracket operations" $ do
    it "should error on empty stack" $ evaluate (stackBracket 'a' []) `shouldThrow` anyException
    it "should ignore non stack chars" $ (stackBracket 'a' "[]") `shouldBe` "[]"
    it "should append like stack symbols" $ checkLikeStacks `shouldBe` True
    it "should cancel out matching brackets" $ checkMatchingBrackets `shouldBe` True
    it "should skip open brackets on zero"   $ checkSkipBracket `shouldReturn` True
    it "should loop properly"                $ checkLoops `shouldReturn` True

  hspec $ describe "Test bad programs" $ do
    it "Should error on unmatched end bracket"   $ checkUnmatchedEndBracket `shouldThrow` anyException
    it "Should error on unmatched begin bracket" $ checkUnmatchedBeginBracket `shouldThrow` anyException

  hspec $ describe "Total functionality" $ do
    it "Should execute hello world properly" $ callCommand "sh test/testHello.sh" `shouldReturn` ()
