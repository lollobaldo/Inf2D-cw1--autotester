module Generator (main) where

import Test.QuickCheck
import Control.Monad
import Data.List
import Data.List.Split
import System.IO.Unsafe

import qualified Inf2d1 as I
import ConnectFourWithTwist


statW :: Bool -> Int -> [(Int, Gen Int)]
statW False n = [(1, choose (0,0)), (1, choose(0,n))]
statW True n = [(5, choose (0,0)), (1, choose(0,n))]


-- rowGenW :: Int -> Gen [Int]
-- rowGenW n = vectorOf n (frequency (statW 10))

-- mtxGen :: Bool -> Int -> Gen [[Int]]
-- mtxGen True n = vectorOf n $ rowGenW n
-- mtxGen False n = vectorOf n $ rowGenU n

adjGen :: Bool -> Int -> Gen [Int]
adjGen b n = vectorOf (n*n) $ frequency (statW b 10)

hrGen :: Int -> Gen [Int]
hrGen n = vectorOf n $ choose (1,10)

getMtx :: IO [Int]
getMtx = generate $ adjGen True 10

getSearchTest :: Bool -> Int -> Int -> String
getSearchTest b n i = "testAuto" ++ show i ++ "size" ++ show n ++
    " = TestSearch \"Autogenerated graph " ++ show i ++ " (size" ++ show n ++ ")\" ("
    ++ graphObj ++ ") [0] 5 5 " ++ showSols
  where
    graphObj = "GraphObj \"AutoGraph" ++ show i ++ "\" " ++ show (chunksOf n graph) ++ " " ++ show hr
    hr = unsafePerformIO . generate $ hrGen n
    graph = unsafePerformIO . generate $ adjGen b n
    showSols :: String
    showSols = concatMap (show . wrapSol) sols
    wrapSol :: Maybe I.Branch -> [Int]
    wrapSol Nothing = []
    wrapSol (Just b) = reverse b
    sols :: [Maybe I.Branch]
    sols =
      [
        I.breadthFirstSearch graph 5 I.next [[0]] []
      , I.depthLimitedSearch graph 5 I.next [[0]] 5 []
      , I.aStarSearch graph 5 I.next I.getHr hr I.cost [[0]] []
      ]

getAllTests :: [String]
getAllTests = fmap (getSearchTest False 10) [0..9]
  ++ fmap (getSearchTest False 15) [0..9]
  ++ fmap (getSearchTest False 25) [0..9]
  ++ fmap (getSearchTest True 20) [0..9]
  ++ fmap (getSearchTest True 30) [0..9]
  ++ fmap (getSearchTest True 50) [0..9]

header = ["module Autotests where", "import Tests"]

exports :: String
exports = "testsListAuto=[" ++
  intercalate "," ["testAuto"++ show i ++ "size" ++ show n | n <- [10, 15, 20, 25, 30, 50], i <- [0..9]]
  ++ "]"

getFile :: String
getFile = unlines $ header ++ [exports] ++ getAllTests

main :: IO ()
main = writeFile "Autotests.hs" getFile


isValidGame0 :: Game -> Bool
isValidGame0 gg =
  let [c,h] = map length . group . filter (/= -1) . sort $ gg
  in c == h || c == h-1

isValidGame1 :: Game -> Bool
isValidGame1 gg = all isValidCol cols
  where
    cols = transpose $ chunksOf 4 gg
    isValidCol [] = True
    isValidCol (-1:xs) = isValidCol xs
    isValidCol xs = -1 `elem` xs