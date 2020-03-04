-- {-# OPTIONS -Weverything #-}
-- {-# OPTIONS -W-no-unsafe #-}
{-# OPTIONS -Wall #-}
-- {-# OPTIONS -Wincomplete-uni-patterns #-}
-- {-# OPTIONS -Wincomplete-record-updates #-}
-- {-# OPTIONS -Wcompat #-}
-- {-# OPTIONS -Widentities #-}
-- {-# OPTIONS -Wredundant-constraints #-}
-- {-# OPTIONS -Wmissing-export-lists #-}
-- {-# OPTIONS -Wpartial-fields #-}
-- {-# OPTIONS -funbox-strict-fields #-}
-- {-# OPTIONS -fhide-source-paths #-}
-- {-# OPTIONS -freverse-errors #-}


{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE BangPatterns #-}
module Tester (main) where

import Data.List
import Data.Maybe
import Control.Monad
import System.Info
-- import Data.List.Split
import System.Environment
import Control.Exception
import Unsafe.Coerce
import System.IO.Unsafe
import System.Timeout
-- import System.CPUTime




import Tests
import Autotests
-- import ConnectFourWithTwist

import Inf2d1 hiding (main)


data Result = Passed | TestError {
  _testFunction :: !Function,
  _testText :: !String,
  _input :: ![String],
  _expectedOutput :: !String,
  _actualOutput :: !String
}

instance Show Result where
  show (TestError _ _ _ _ "TIMEOUT") = "∞"
  show (TestError _ _ _ _ "UNDEFINED") = "?"
  show (TestError _ _ _ _ "ERROR") = "е"
  show Passed = "√"
  show _ = "Х"
  -- show Passed = if os == "mingw32" then "√" else "✔"
  -- show _ = if os == "mingw32" then "Х" else "✗"


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

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []

squarize :: [a] -> [[a]]
squarize ls = chunksOf (i 15) ls
  where
    n = length ls
    i 10 = 10
    i j
      | n `mod` j == 0 = j
      | otherwise = i (j-1)

tabularise :: [[String]] -> String
tabularise a = unlines . ([topRow xs] ++) . (++ [botRow xs]) . intersperse (midRow xs) . map (tabRow xs) $ padded
  where
    mx = maximum . map length $ a
    padded = pad mx a
    xs = map maximum . transpose . map (map length) $ a

justify :: Int -> String -> String
justify n s = sps ++ s ++ sps ++ replicate (lp `mod` 2) ' '
  where
    lp = n - length s
    n2 = lp `div` 2
    sps = replicate n2 ' '

-- boxIt :: [String] -> String
-- boxIt ss = unlines $ [topRow [mx]]
--         ++ map makeMid ss
--         ++ [botRow [mx]]
--   where
--     mx = maximum . map safeLen $ ss
--     safeLen s = showOff $ length s - if '\10084' `elem` s then 2 else 0
--     makeMid :: String -> String
--     makeMid = ("│ " ++ ) . ( ++ " │"). justify mx
--     -- padded = pad mx ss
--     -- xs = map maximum . transpose . map (map length) $ a

boxItWA :: [String] -> String
boxItWA [s1,s2] = unlines $ map ("  " ++) [topRow [mx], makeMid1, makeMid2, botRow [mx]]
  where
    mx = length s2 - 1
    makeMid1, makeMid2 :: String
    makeMid1 = "│ " ++ justify mx s1 ++ " │"
    makeMid2 = "│ " ++ s2 ++ " │"

wrapSolution :: Branch -> Maybe Branch
wrapSolution [] = Nothing
wrapSolution b = Just b

-- prettyPrintMaybeBranch :: Maybe Branch -> String
-- prettyPrintMaybeBranch = maybe "Nothing" prettyPrintBranch

prettyPrintBranch :: Branch -> String
prettyPrintBranch = intercalate " -> " . map show . reverse

-- prettyPrintBranchList :: [Branch] -> String
-- prettyPrintBranchList = unlines . map prettyPrintBranch

descriptivePrintResult :: Result -> String
descriptivePrintResult Passed = ""
descriptivePrintResult (TestError f t i e a) =
  "Failed test: \t\t" ++ t ++ "\n" ++
  "On function: \t\t" ++ show f ++ "\n" ++
    (if "Autogenerated" `isPrefixOf` t && hide
      then ""
      else "Inputs: " ++ drop 1 (unlines $ map ("\t\t\t"++) i))
  ++
  "Expected output: \t" ++ e ++ "\n" ++
  "Actual output: \t\t" ++ a
  where
    args = unsafePerformIO getArgs
    hide = "--show-detailed-auto" `notElem` args


getResult :: (Eq a, Show a) => Function -> String -> [String] -> a -> a -> Result
getResult f s i expected output = unsafePerformIO result
  where
    analyse :: Bool -> Result
    analyse True = Passed
    analyse False = TestError f s i (show expected) (show output)
    handleError :: IO Result -> IO Result
    handleError =
      flip catches
        -- [Handler (\(_ :: TypeError) -> return $ TestError f s i (show expected) "UNDEFINED")]--,
        [Handler (\(_ :: ErrorCall) -> return $ TestError f s i (show expected) "UNDEFINED")]--,
    execute :: IO (Maybe Result)
    execute = do
      args <- getArgs
      let noTimeout = "--no-timeout" `elem` args
      let timer = if noTimeout then -1 else 5000000
      case f of
        ASS -> timeout timer . handleError . evaluate . analyse $ compareCosts (unsafeCoerce output) (unsafeCoerce expected)
        _ -> timeout timer . handleError . evaluate . analyse $ (output == expected)
    result = do
      let timeoutError = TestError f s i (show expected) "TIMEOUT"
      fromMaybe timeoutError <$> execute
    compareCosts :: Maybe Branch -> Maybe Branch -> Bool
    compareCosts b1 b2 = evalBCost b1 == evalBCost b2
    evalBCost :: Maybe Branch -> Int
    evalBCost b = maybe 0 (cost graph . reverse) b
    graph :: [Int]
    graph = read $ drop 7 $ i !! 3

testerNext :: TestNext -> Result
testerNext (TestNext branch graph out) = analyse . prettify $ next inp1 inp2
  where
    inp1 = reverse branch
    inp2 = concat . adjMtx $ graph
    prettify :: [Branch] -> [Branch]
    prettify = sort . map reverse
    inps = ["Branch: "++prettyPrintBranch inp1, "Graph: "++show graph]
    analyse :: [Branch] -> Result
    analyse = getResult Next "testNext" inps out

testerCheckArrival :: TestCheckArrival -> Result
testerCheckArrival (TestCheckArrival inp1 inp2 out) = analyse $ checkArrival inp1 inp2
  where
    inps = ["Node1: "++show inp1, "Node2: "++show inp2]
    analyse :: Bool -> Result
    analyse = getResult CheckArrival "testCA" inps out

testerExplored :: TestExplored -> Result
testerExplored (TestExplored inp1 inp2 out) = analyse $ explored inp1 inp2
  where
    inps = ["Node: "++show inp1, "Nodes: "++show inp2]
    analyse :: Bool -> Result
    analyse = getResult Explored "testExplored" inps out

testerCost :: TestCost -> Result
testerCost (TestCost graph branch out) = analyse $ cost inp1 inp2
  where
    inp1 = concat . adjMtx $ graph
    inp2 = reverse branch
    inps = ["Graph: "++show graph, "Branch: "++prettyPrintBranch inp2]
    analyse :: Int -> Result
    analyse = getResult Cost "testCost" inps out

testerGetHR :: TestGetHR -> Result
testerGetHR (TestGetHR inp1 inp2 out) = analyse $ getHr inp1 inp2
  where
    inps = ["HR Table: "++show inp1, "Node: "++show inp2]
    analyse :: Int -> Result
    analyse = getResult GetHR "testGetHR" inps out

testerEval :: TestEval -> Result
testerEval (TestEval inp1 out) = analyse $ eval inp1
  where
    inps = ["Game: "++show inp1]
    analyse :: Int -> Result
    analyse = getResult Eval "testEval" inps out

testerSearch :: Function -> TestSearch -> Result
testerSearch f testObj = analyse . prettify $ case f of
    BFS -> breadthFirstSearch graph end next [start] []
    DFS -> depthLimitedSearch graph end next [start] depthLimit []
    ASS -> aStarSearch graph end next getHr heuristicTable cost [start] []
    _ -> inputError
  where
    prettify :: Maybe Branch -> Maybe Branch
    prettify = fmap reverse
    inps = [
        "Starting branch: "++ prettyPrintBranch start
      , "Destination: "++show end
      , "Depth limit: "++show depthLimit
      , "Graph: "++show graph
      , "Heuristic: "++show heuristicTable]
    analyse :: Maybe Branch -> Result
    analyse = getResult f (description testObj) inps solution
    graph = concat . adjMtx . graphObj $ testObj
    start = reverse $ startBranch testObj
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
    inps = ["Player: "++show inp1, "Game: "++show inp2]
    analyse :: Int -> Result
    analyse = getResult AB testDescription inps out

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

runTestsAuto :: [Result]
runTestsAuto = concatMap (`testAll` testsListAuto) funcList

mainTestFuncs :: IO ()
mainTestFuncs = do
  putStrLn "Testing functions:"
  let testResultsTableFunc = map (map show) runTestsFunctions
  let labelsFunc = zipWith (:) ["Next","CheckArrival", "Explored", "Cost", "GetHR", "Eval"] testResultsTableFunc
  putStrLn . tabularise $ labelsFunc

mainTestSearches :: IO ()
mainTestSearches = do
  putStrLn "Testing searches:"
  let testResultsTable = map (map show) runTestsSearches
  let labels = zipWith (:) ["Breath-first search","Depth-first search","A* search", "Alpha-Beta"] testResultsTable
  putStrLn . tabularise $ labels

mainTestAuto :: IO ()
mainTestAuto = do
  putStrLn "Autotests:"
  let testResultsTable = squarize $ map show runTestsAuto
  -- let labels = zipWith (:) ["10×10","20×20","30×30"] testResultsTable
  putStrLn . tabularise $ testResultsTable

main :: IO ()
main = do
  args <- getArgs
  -- let user = if null args then "" else head args
  let meme = "--no-meme" `notElem` args
  let showNErrors = if "--show-all" `notElem` args then 5::Int else 1000::Int
  mainTestFuncs
  mainTestSearches

  let runnedTests = concat (runTestsFunctions ++ runTestsSearches)
  let errors = [descriptivePrintResult e | e@TestError{} <- runnedTests]
  let errorsExcluding = [descriptivePrintResult e | e@TestError{} <- runnedTests, _actualOutput e /= "UNDEFINED"]
  let autoErrors = [descriptivePrintResult e | e@TestError{} <- runTestsAuto]
  when (null errors) mainTestAuto

  let totErrors = if null errors then autoErrors else errors
  let showErrors = if null errors
        then autoErrors
        else (if null errorsExcluding then ["Some undefined stuff"] else errorsExcluding)
  let passTest = "All tests passed"
  let summary = if null totErrors
       then passTest ++ if meme then ", enjoy a LambdaMan to cheer up." else ""
       else "Failed " ++ show (length totErrors) ++ " tests. Here are the first 5 errors (excluding undefined).\n" ++inclusiveIntercalate ("\n" ++ replicate 50 '─' ++ "\n") (take showNErrors showErrors)
  putStrLn summary
  lambda <- readFile $ if os == "mingw32" then "lambda.txt" else "lambdaC.txt"
  wrong <- readFile "wrong.txt"
  when meme $ if null (errors ++ totErrors)
    then putStrLn lambda
    else putStrLn wrong

  putStrLn ""
  let emjs = if os == "mingw32" then "♥✶" else "❤️ ⭐"
  let sjme = if os == "mingw32" then "✶♥" else "⭐❤️ "
  let starMsg =
        [
          "If you like my beautiful autotester please star the repo at"
        , emjs ++ "️ https://github.com/lollobaldo/Inf2D-cw1--autotester/stargazers " ++ sjme
        ]
  putStrLn . boxItWA $ starMsg
  putStrLn ""
