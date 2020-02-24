module Generator (main) where

import Test.QuickCheck
import Control.Monad
import Data.List
import Data.List.Split
import System.IO.Unsafe

import Inf2d1
import ConnectFourWithTwist
import qualified Tester as T

adj1 =
 [[0, 1, 1, 0, 0],
  [0, 0, 0, 1, 0],
  [0, 0, 0, 0, 1],
  [0, 0, 0, 0, 1],
  [0, 0, 0, 0, 0]]

stat :: [(Int, Gen Int)]
stat = [(1, choose (0,0)), (1, choose(0,1))]

rowGenU :: Int -> Gen [Int]
rowGenU n = vectorOf n (frequency stat)
rowGenW :: Int -> Gen [Int]
rowGenW n = vectorOf n (choose (0,n-1))

adjGen :: Bool -> Int -> Gen [[Int]]
adjGen True n = vectorOf n $ rowGenW n
adjGen False n = vectorOf n $ rowGenU n

getMtx :: IO [[Int]]
getMtx = generate $ adjGen False 10

printMtx :: IO [[Int]] -> IO ()
printMtx mtx = fmap f mtx >>= putStrLn
  where
    f :: [[Int]] -> String
    f = show
    -- f = intercalate "\n" . map show
    -- f = intercalate "\n" . map ((++",") . init . tail . show)


-- generator :: String
-- generator = fmap show . getMtx
--   [ | x <- [0..9]]

-- getGraphObj :: IO [[Int]] -> IO String
-- getGraphObj io = fmap ("GraphObj \"Autogenerated graph\" " ++ show xs ++ show (replicate 1 n)) xs
--   where
--     xs = unsafePerformIO io
--     n = fmap length xs


-- testAuto0 = TestSearch "Autogenerated graph (10 nodes)" auto0 0 5 3 [0,4,5] [0,3,2,5] [0,6,5]
-- testAuto1 = TestSearch "Autogenerated graph (10 nodes, small depth)" auto0 0 5 2 [0,4,5] [0,4,5] [0,6,5]

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
    isValidCol xs = elem -1 xs