-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}

import Data.List
import System.Info
import Inf2d1


type Matrix = [[Int]]

-- instance Show Matrix
--   where
--     show = tabularise

newtype TableCell = TableCell String
instance Show TableCell where
  show (TableCell s) = s

data GraphObj = GraphObj {
  description :: String,
  adjMtx :: Matrix,
  hrTable :: [Int]
}

data TestSearch = TestSearch {
  graphObj :: GraphObj,
  startNode :: Node,
  endNode :: Node,
  testDepth :: Int,
  bfs :: Branch,
  dfs :: Branch,
  ass :: Branch
}

data TestNext = TestNext {
  inpBranch :: Branch,
  inpGraph :: GraphObj,
  expected :: [Branch]
}

data Result = Passed | TestError {
  testFunction :: Function,
  testText :: String,
  testGraph :: GraphObj
}
instance Show Result where
  show Passed = if os == "mingw32" then "√" else "✔"--"v"--"✔"--"x"--"✓"--"✔"--"\xE2\x9C\x94"--"✔"
  show _ = if os == "mingw32" then "X" else "✗"--"✗"--"x"--"❌"--"x"--"✗"--"❌"--"x"--"❌"✕

data Function = Next | BFS | DFS | ASS deriving (Show)

data InputNext = InputNext {
  in1 :: Branch,
  in2 :: Graph
}

class Input a
instance Input InputNext

class Testable a where
  test :: a -> Result

instance Testable TestNext where
  test = testNext


emptyGraph = GraphObj "Empty graph"
 [[]]
  []

simple = GraphObj "Simple graph"
 [[0, 1, 0],
  [0, 0, 1],
  [0, 0, 0]]
  [1, 1, 1]

simpleLoop = GraphObj "Simpe graph with a loop"
 [[0, 1, 0],
  [1, 0, 1],
  [0, 0, 0]]
  [1, 1, 1]

simpleGraph = GraphObj "Graph 1"
 [[0, 1, 1, 0, 0],
  [0, 0, 0, 1, 0],
  [0, 0, 0, 0, 1],
  [0, 0, 0, 0, 1],
  [0, 0, 0, 0, 0]]
  [1, 1, 1, 1, 1]

hugeGraph = GraphObj "Medium-sized graph"
 [[0, 3, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0],
  [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]
  [5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 2, 0]


testSimple = TestSearch simple 0 2 5 [0,1,2] [0,1,2] [0,1,2]
testSimpleLoop = TestSearch simpleLoop 0 2 5 [0,1,2] [0,1,2] [0,1,2]
testSimpleImp = TestSearch simpleLoop 0 3 5 [] [] []
testBranching = TestSearch simpleGraph 0 4 5 [0,2,4] [0,1,3,4] [0,2,4]
testGraphStd = TestSearch hugeGraph 0 12 4 [0,3,6,12] [0,1,5,10,12] [0,2,5,11,12]
testGraphDpt = TestSearch hugeGraph 0 12 3 [0,3,6,12] [0,3,6,12] [0,2,5,11,12]

testSearchList = [
  testSimple,
  testSimpleLoop,
  testSimpleImp,
  testBranching,
  testGraphStd,
  testGraphDpt]

testNextEmpty = TestNext [] emptyGraph [[]]
testNextSimple0 = TestNext [0] simple [[0,1]]
testNextSimple1 = TestNext [0,1] simple [[0,1,2]]
testNextGraph0 = TestNext [0] simpleGraph [[0,1],[0,2]]
testNextGraph1 = TestNext [0,1] simpleGraph [[0,1,3]]

testNextList :: [TestNext]
testNextList = [
  testNextEmpty,
  testNextSimple0,
  testNextSimple1,
  testNextGraph0,
  testNextGraph1]




-- main = do
--   -- hSetEncoding stdout utf8
--   T.putStrLn "Алексей Кулешевичčušpajž日本語✕✗❌"

main = do
  -- putStrLn "čušpajž日本語✕"--"✗"--"❌"--"x"--"❌"✕
  runTestsNext testNextList
  runTestsSearch testSearchList

-- ┌──┬──┐
-- │  │  │
-- ├──┼──┤
-- │  │  │
-- └──┴──┘

topRow, midRow, botRow :: [Int] -> String
topRow xs = "┌─" ++ intercalate "─┬─" [replicate x '─' | x <- xs] ++ "─┐"
midRow xs = "├─" ++ intercalate "─┼─" [replicate x '─' | x <- xs] ++ "─┤"
botRow xs = "└─" ++ intercalate "─┴─" [replicate x '─' | x <- xs] ++ "─┘"

padToLength :: Int -> String -> String
padToLength l s = s ++ replicate (l - length s) ' '

tabRow :: [Int] -> [String] -> String
tabRow xs ss = "│ " ++ intercalate " │ " [padToLength l s | (l,s) <- zip xs ss] ++ " │"
  -- inclusiveIntercalate "│" . map ((' ':) . (++" "))

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

prettyPrintMaybeBranch :: Maybe Branch -> String
prettyPrintMaybeBranch = maybe "Nothing" prettyPrintBranch

prettyPrintBranch :: Branch -> String
prettyPrintBranch = ("\t" ++) . intercalate " -> " . map show . reverse

prettyPrintBranchList :: [Branch] -> String
prettyPrintBranchList = unlines . map prettyPrintBranch

prettyPrintResults :: [[Result]] -> String
prettyPrintResults = tabularise . map (map show)


testNext :: TestNext -> Result
testNext test = analyse $ next branch graph
  where
    solution = map reverse . expected $ test
    branch = reverse . inpBranch $ test
    graph = concat . adjMtx . inpGraph $ test
    analyse :: [Branch] -> Result
    analyse result
      | sort result == solution = Passed
      | otherwise = TestError Next "" (inpGraph test)

testFunctions :: Function -> TestNext -> Result
testFunctions Next = testNext
testFunctions _ = error "not implemented yet"

testSearch :: Function -> TestSearch -> Result
testSearch search test = analyse $ case search of
    BFS -> breadthFirstSearch graph end next [[start]] []
    DFS -> depthLimitedSearch graph end next [[start]] depthLimit []
    ASS -> aStarSearch graph end next getHr heuristicTable cost [[start]] []
  where
    analyse :: Maybe Branch -> Result
    analyse result
      | result == solution = Passed
      | otherwise = TestError search "" (graphObj test)
    graph = concat . adjMtx . graphObj $ test
    start = startNode test
    end = endNode test
    solution = wrapSolution . reverse . getSolution $ test
    getSolution = case search of
      BFS -> bfs
      DFS -> dfs
      ASS -> ass
    depthLimit = testDepth test
    heuristicTable = hrTable . graphObj $ test


-- runTestsNext :: [TestNext] -> IO()
-- runTestsNext ts = do
--     putStrLn "Testing functions:"
--     -- let sols = map testNext t
--     let testResultsTable = map (map show) . transpose $ map applyTestNext ts
--     let labels = zipWith (:) ["Next"] testResultsTable
--     putStrLn . tabularise $ labels

--     -- putStrLn $ prettyPrintResults [sols]
--     -- let count = length . filter (=="Passed!") $ sols
--     -- putStrLn "Testing finished!"
--     -- putStrLn $ "Passed " ++ show count ++ "/" ++ show (length t) ++ " tests."

searchList, funcList :: [Function]
searchList = [BFS, DFS, ASS]
funcList = [Next, Next]

applyTestNext :: TestNext -> [Result]
applyTestNext t = map (flip testFunctions t) funcList
applyTestSearch :: TestSearch -> [Result]
applyTestSearch t = map (flip testSearch t) searchList


runTestsNext :: [TestNext] -> IO()
runTestsNext ts = do
    putStrLn "Testing functions:"
    let testResultsTable = map (map show) . transpose $ map applyTestNext ts
    let labels = zipWith (:) ["Next","Next"] testResultsTable
    putStrLn . tabularise $ labels

runTestsSearch :: [TestSearch] -> IO()
runTestsSearch ts = do
    putStrLn "Testing searches:"
    let testResultsTable = map (map show) . transpose $ map applyTestSearch ts
    let labels = zipWith (:) ["Breath-first search","Depth-first search","A* search"] testResultsTable
    putStrLn . tabularise $ labels
