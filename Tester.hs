-- {-# OPTIONS -Weverything #-}
-- {-# OPTIONS -W-no-unsafe #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Widentities #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wmissing-export-lists #-}
{-# OPTIONS -Wpartial-fields #-}
{-# OPTIONS -funbox-strict-fields #-}
{-# OPTIONS -fhide-source-paths #-}
{-# OPTIONS -freverse-errors #-}


{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE BangPatterns #-}
module Tester (main) where

import Data.List
import Data.Maybe
import Control.Monad
import System.Info
import System.Environment
import Control.Exception
import System.IO.Unsafe
import System.Timeout


import Tests
-- import ConnectFourWithTwist

import Inf2d1 hiding (main)


data Result = Passed | TestError {
  _testFunction :: !Function,
  _testText :: !String,
  _expectedOutput :: !String,
  _actualOutput :: !String
}

instance Show Result where
  show (TestError _ _ _ "TIMEOUT") = "∞"
  show (TestError _ _ _ "UNDEFINED") = "?"
  show (TestError _ _ _ "ERROR") = "е"
  show Passed = if os == "mingw32" then "√" else "✔"
  show _ = if os == "mingw32" then "Х" else "✗"


data Function =
  Next |
  CheckArrival |
  Explored |
  Cost |
  GetHR |
  Eval |
  BFS |
  DFS |
  ASS |
  ABS |
  AB deriving (Show)

inputError :: a
inputError = error "ERROR: wrong input type"

class Test t where
  test :: Function -> t -> Result

instance Test TestNext where
  test Next = testerNext
  test _ = inputError

instance Test TestCheckArrival where
  test CheckArrival = testerCheckArrival
  test _ = inputError

instance Test TestExplored where
  test Explored = testerExplored
  test _ = inputError

instance Test TestCost where
  test Cost = testerCost
  test _ = inputError

instance Test TestGetHR where
  test GetHR = testerGetHR
  test _ = inputError

instance Test TestEval where
  test Eval = testerEval
  test _ = inputError

instance Test TestSearch where
  test = testerSearch

instance Test TestAlphaBeta where
  test AB = testerAlphaBeta
  test _ = inputError


inclusiveIntercalate :: [a] -> [[a]] -> [a]
inclusiveIntercalate a b = a ++ intercalate a b ++ a

topRow, midRow, botRow :: [Int] -> String
topRow xs = "┌─" ++ intercalate "─┬─" [replicate x '─' | x <- xs] ++ "─┐"
midRow xs = "├─" ++ intercalate "─┼─" [replicate x '─' | x <- xs] ++ "─┤"
botRow xs = "└─" ++ intercalate "─┴─" [replicate x '─' | x <- xs] ++ "─┘"

padToLength :: Int -> String -> String
padToLength l s = s ++ replicate (l - length s) ' '

tabRow :: [Int] -> [String] -> String
tabRow xs ss = "│ " ++ intercalate " │ " [padToLength l s | (l,s) <- zip xs ss] ++ " │"

pad :: Int -> [[String]] -> [[String]]
pad i = map (\ss -> take i $ ss ++ repeat "")

tabularise :: [[String]] -> String
tabularise a = unlines . ([topRow xs] ++) . (++ [botRow xs]) . intersperse (midRow xs) . map (tabRow xs) $ padded
  where
    mx = maximum . map length $ a
    padded = pad mx a
    xs = map maximum . transpose . map (map length) $ a


wrapSolution :: Branch -> Maybe Branch
wrapSolution [] = Nothing
wrapSolution b = Just b

-- prettyPrintMaybeBranch :: Maybe Branch -> String
-- prettyPrintMaybeBranch = maybe "Nothing" prettyPrintBranch

-- prettyPrintBranch :: Branch -> String
-- prettyPrintBranch = ("\t" ++) . intercalate " -> " . map show . reverse

-- prettyPrintBranchList :: [Branch] -> String
-- prettyPrintBranchList = unlines . map prettyPrintBranch

-- prettyPrintResults :: [[Result]] -> String
-- prettyPrintResults = tabularise . map (map show)

descriptivePrintResult :: Result -> String
descriptivePrintResult Passed = ""
descriptivePrintResult (TestError f t e a) = 
  "Failed test: \t\t" ++ t ++ "\n" ++
  "On function: \t\t" ++ show f ++ "\n" ++
  "Expected output: \t" ++ e ++ "\n" ++
  "Actual output: \t\t" ++ a

getResult :: (Eq a, Show a) => Function -> String -> a -> a -> Result
getResult f s expected output = unsafePerformIO result
  where
    -- handleError :: Exception e => e -> IO Result
    -- handleError (ErrorCall e) = return $ TestError f s (show expected) "UNDEFINED"
    -- handleError (SomeException e) = return $ TestError f s (show expected) "ERROR"
    -- handleSuccess :: Maybe Bool -> IO Result
    -- handleSuccess = return doIt
    --   where
    --     doIt Nothing = TestError f s (show expected) "TIMEOUT"
    --     doIt (Just False) = TestError f s (show expected) (show output)
    --     doIt (Just True) = Passed
    analyse :: Bool -> Result
    analyse True = Passed
    analyse False = TestError f s (show expected) (show output)
    handleError :: IO Result -> IO Result
    handleError =
      flip catches
        [Handler (\(_ :: ErrorCall) -> return $ TestError f s (show expected) "UNDEFINED")]--,
        --  Handler (\(_ :: SomeException) -> return $ TestError f s (show expected) "ERROR")]
    execute :: IO (Maybe Result)
    execute = do
      args <- getArgs
      let noTimeout = "--no-timeout" `elem` args
      let timer = if noTimeout then -1 else 5000000
      timeout timer . handleError . evaluate . analyse $ (output == expected)
    result = do
      let timeoutError = TestError f s (show expected) "TIMEOUT"
      fromMaybe timeoutError <$> execute

testerNext :: TestNext -> Result
testerNext (TestNext branch graph out) = analyse . prettify $ next inp1 inp2
  where
    inp1 = reverse branch
    inp2 = concat . adjMtx $ graph
    prettify :: [Branch] -> [Branch]
    prettify = sort . map reverse
    analyse :: [Branch] -> Result
    analyse = getResult Next "testNext" out

testerCheckArrival :: TestCheckArrival -> Result
testerCheckArrival (TestCheckArrival inp1 inp2 out) = analyse $ checkArrival inp1 inp2
  where
    analyse :: Bool -> Result
    analyse = getResult CheckArrival "testCA" out

testerExplored :: TestExplored -> Result
testerExplored (TestExplored inp1 inp2 out) = analyse $ explored inp1 inp2
  where
    analyse :: Bool -> Result
    analyse = getResult Explored "testExplored" out

testerCost :: TestCost -> Result
testerCost (TestCost graph branch out) = analyse $ cost inp1 inp2
  where
    inp1 = concat . adjMtx $ graph
    inp2 = reverse branch
    analyse :: Int -> Result
    analyse = getResult Cost "testCost" out

testerGetHR :: TestGetHR -> Result
testerGetHR (TestGetHR inp1 inp2 out) = analyse $ getHr inp1 inp2
  where
    analyse :: Int -> Result
    analyse = getResult GetHR "testGetHR" out

testerEval :: TestEval -> Result
testerEval (TestEval inp1 out) = analyse $ eval inp1
  where
    analyse :: Int -> Result
    analyse = getResult Eval "testEval" out

testerSearch :: Function -> TestSearch -> Result
testerSearch f testObj = analyse . prettify $ case f of
    BFS -> breadthFirstSearch graph end next [[start]] []
    DFS -> depthLimitedSearch graph end next [[start]] depthLimit []
    ASS -> aStarSearch graph end next getHr heuristicTable cost [[start]] []
    _ -> inputError
  where
    prettify :: Maybe Branch -> Maybe Branch
    prettify = fmap reverse
    analyse :: Maybe Branch -> Result
    analyse = getResult f (description testObj) solution
    graph = concat . adjMtx . graphObj $ testObj
    start = startNode testObj
    end = endNode testObj
    solution = wrapSolution . getSolution $ testObj
    getSolution = case f of
      BFS -> bfs
      DFS -> dfs
      ASS -> ass
      _   -> inputError
    depthLimit = testDepth testObj
    heuristicTable = hrTable . graphObj $ testObj

testerAlphaBeta :: TestAlphaBeta -> Result
testerAlphaBeta (TestAlphaBeta testDescription inp2 inp1 out) = analyse $ alphabeta inp1 inp2
  where
    analyse :: Int -> Result
    analyse = getResult AB testDescription out

testAll :: Test t => Function -> [t] -> [Result]
testAll = map . test

runTestsFunctions :: [[Result]]
runTestsFunctions = [rn, rca, re, rc ,rghr, rev]
  where
    rn = testAll Next testsListNext
    rca = testAll CheckArrival testsListCheckArrival
    re = testAll Explored testsListExplored
    rc = testAll Cost testsListCost
    rghr = testAll GetHR testsListGetHR
    rev = testAll Eval testsListEval

funcList :: [Function]
funcList = [BFS, DFS, ASS]
runTestsSearches :: [[Result]]
runTestsSearches = map (`testAll` testsListSearch) funcList
                   ++ [testAll AB testsListAlphaBeta]

main :: IO ()
main = do
  -- putStrLn "AAARAINBOWAAA"
  args <- getArgs
  -- let user = if null args then "" else head args
  let meme = "--no-meme" `notElem` args
  putStrLn "Testing functions:"
  let testResultsTableFunc = map (map show) runTestsFunctions
  let labelsFunc = zipWith (:) ["Next","CheckArrival", "Explored", "Cost", "GetHR", "Eval"] testResultsTableFunc
  putStrLn . tabularise $ labelsFunc
  putStrLn "Testing searches:"
  let testResultsTable = map (map show) runTestsSearches
  let labels = zipWith (:) ["Breath-first search","Depth-first search","A* search", "Alpha-Beta"] testResultsTable
  putStrLn . tabularise $ labels
  let errors = [descriptivePrintResult e | e@TestError{} <- concat (runTestsFunctions ++ runTestsSearches)]
  let passTest = "All tests passed"
  let summary = case errors of
        [] -> passTest ++ if meme then ", enjoy a LambdaMan to cheer up." else ""
        _ -> inclusiveIntercalate ("\n" ++ replicate 50 '─' ++ "\n") errors
  putStrLn summary
  lambda <- readFile $ if os == "mingw32" then "lambda.txt" else "lambdaC.txt"
  wrong <- readFile "wrong.txt"
  when meme $ if null errors
    then putStrLn lambda
    else putStrLn wrong