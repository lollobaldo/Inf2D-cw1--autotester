-- Inf2d Assignment 1 2019-2020
-- Matriculation number: s1853050

-- {-# OPTIONS -Wall #-}
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


{-|
  Module      : Inf2d1
  Description : Inf2d Assignment 1
  Maintainer  : s1853050@ed.ac.uk
  Tester:     : https://github.com/lollobaldo/Inf2D-cw1--autotester
-}

module Inf2d1 where

import Data.List (elemIndices, elemIndex)


import ConnectFourWithTwist

-----------------
-- MY IMPORTS! --
-----------------
import Data.List (insertBy, sortBy)
import Data.Function (on)


-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph= [Node]


-------------------------------
-- Section 1: Uniform Search --
-------------------------------

-- | Not used, calculated on the fly
numNodes :: Int
numNodes = 4

-- | next, takes current branch and a graph
-- | returns reachable branches.
-- | Uses helper function `getNeighbours`: see bottom
-- | Get all nodes reachable by Node `b`, and cons them to the branch
next :: Branch -> Graph -> [Branch]
next [] = const []
next branch@(b:_) = map (:branch) . getNeighbours b


-- | checkArrival, takes the current node and the destination node
-- | returns if the current node is the destination
-- | As Nodes are represented by 'Int', we can just check for equality
checkArrival::Node -> Node -> Bool
checkArrival = (==)


-- | explored, takes a node and a list of explored nodes
-- | returns if the node has been explored already
-- | Just checks for membership in the list
explored:: Node -> [Node] ->Bool
explored = elem



-- | Breadth-first search.
-- | Takes graph, destination, helper functons and a frontier list
-- | Implemented using a helper function as graph, destination and next function
-- | don't change throughout the recursive calls. This way these values are
-- | shared through the search (faster compiled code and easier to follow logic).
breadthFirstSearch :: Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
breadthFirstSearch graph destination next = doBFS
  where
    -- | Actually does the search. Takes a list of branches on the frontier and
    -- | a list of already explored nodes.
    -- | Return Nothing if no Nodes are left to be explored
    -- | Return Just b if b is a solution
    -- | Skip if already explored
    -- | Else expand it, adding the reachable nodes to the END of the frontier
    doBFS :: [Branch] -> [Node] -> Maybe Branch
    doBFS [] _ = Nothing
    doBFS (b:bs) visited
      | isSolution b = Just b
      | (flip explored visited . head) b = doBFS bs visited
      | otherwise = doBFS (bs ++ next b graph) (head b : visited)
    isSolution :: Branch -> Bool
    isSolution = checkArrival destination . head


-- | Depth-first search.
-- | Takes graph, destination, helper functons, depth limit and a frontier list
-- | Implemented using a helper function as graph, destination and next function
-- | don't change throughout the recursive calls. This way these values are
-- | shared through the search (faster compiled code and easier to follow logic).
depthLimitedSearch::Graph -> Node -> (Branch -> Graph -> [Branch]) -> [Branch] -> Int -> [Node] -> Maybe Branch
depthLimitedSearch graph destination next = doDLS
  where
    -- | Actually does the search. Takes a list of branches on the frontier, the
    -- | maximum depth and a list of already explored nodes.
    -- | Return Nothing if no Nodes are left to be explored
    -- | Return Just b if b is a solution
    -- | Skip if already explored OR reaches depth
    -- | Else expand it, adding the reachable nodes to the BEGINNING of the frontier
    -- | NOTE: does not make use of the explored list, as it changes based on the depth.
    -- | A solution could've been to drop the last nodes in the list based on the depth,
    -- | however, in my opinion a cleaner approach is not to use the explored list
    -- | and just check the current branch for loops.
    -- | therefore I use the explored' function
    doDLS :: [Branch] -> Int -> [Node] -> Maybe Branch
    doDLS [] _ _ = Nothing
    doDLS (b:bs) maxDepth _
      | isSolution b = Just b
      | length b > maxDepth || explored' b = doDLS bs maxDepth undefined
      | otherwise = doDLS (next b graph ++ bs) maxDepth undefined
    explored' (n:ns) = n `elem` ns
    isSolution :: Branch -> Bool
    isSolution = checkArrival destination . head



--------------------------------
-- Section 4: Informed Search --
--------------------------------


-- | cost, takes a graph and a branch
-- | Returns the cost of that branch as the sum of the cost of every edge
-- | An edge is an ordered pair of nodes
-- | getEdges takes a branch and return all the edges in it
-- | implemented as functor application with <*> and eta reduction
-- | getCost gets the edge cost from the adjacency matrix
cost :: Graph -> Branch -> Int
cost graph = sum . map getCost . getEdges
  where
    getEdges :: Branch -> [(Node, Node)]
    getEdges = zip <*> tail
    n = getNumNodes graph
    getCost :: (Node, Node) -> Int
    getCost (b,a) = drop (n * a) graph !! b


-- | getHr, takes a heuristic table and a node
-- | Returns the heuristic for that node
-- | implemented as (!!) as the heuristic for the ith node is at the ith position in the table
getHr:: [Int] -> Node -> Int
getHr = (!!)


-- | A-Star search.
-- | Takes graph, destination, helper functons, hr table and a frontier list
-- | Implemented using a helper function as graph, destination, next function,
-- | getHr, hrTable and cost don't change throughout the recursive calls.
-- | This way these values are shared through the search
-- | (faster compiled code and easier to follow logic).
aStarSearch::Graph -> Node -> (Branch -> Graph -> [Branch]) -> ([Int] -> Node -> Int) -> [Int] ->
  (Graph -> Branch -> Int) -> [Branch] -> [Node] -> Maybe Branch
aStarSearch graph destination next getHr hrTable cost = doASS
  where
    -- | Actually does the search. Takes a list of branches on the frontier and
    -- | a list of already explored nodes.
    -- | Return Nothing if no Nodes are left to be explored
    -- | Return Just b if b is a solution
    -- | Skip if already explored
    -- | Else expand it, adding the reachable nodes to the frontier ordered by
    -- | the lowest totalCost (totalCost=cost+heuristic).
    -- | Uses the helper function insertOrderedBy (see below) to insert taking
    -- | advantage of the already ordered frontier (more efficient)
    doASS :: [Branch] -> [Node] -> Maybe Branch
    doASS [] _ = Nothing
    doASS frontier@(b:bs) visited
      | isSolution b = Just b
      | (flip explored visited . head) b = doASS bs visited
      | otherwise = doASS (newFrontier frontier) (head b : visited)
    newFrontier :: [Branch] -> [Branch]
    newFrontier (b:bs) = insertOrderedBy comparison bs (next b graph)
    isSolution :: Branch -> Bool
    isSolution = checkArrival destination . head
    comparison :: Branch -> Branch -> Ordering
    comparison = compare `on` getTotalCost
    getTotalCost :: Branch -> Int
    getTotalCost b@(n:_) = cost graph b + getHr hrTable n



----------------------
-- Section 5: Games --
----------------------


-- | eval, takes a Game,
-- | Returns the valuation for that game
-- | Uses checkWin from ConnectFourWithTwist.hs
eval :: Game -> Int
eval game
  | checkWin game humanPlayer = 1
  | checkWin game compPlayer = -1
  | otherwise = 0


-- | Added type synonym for clearer type signatures
type Bounds = (Int, Int)

-- | alphabeta, takes a playing role and a game
-- | Returns the best evaluation value using alphabeta pruning
-- | Uses (-1, 1) as bounds as we already know these are the best and worst evaluations
alphabeta:: Role -> Game -> Int
alphabeta 0 = minValue (-1, 1) 0
alphabeta 1 = maxValue (-1, 1) 1

-- | maxValue function as described in the algorithm
-- | Return evaluation value if we reached a terminal state
-- | Calls the helper function forLoopMax to run the loop otherwise
maxValue :: Bounds -> Role -> Game -> Int
maxValue bounds player game
  | terminal game = eval game
  | otherwise = forLoopMax player (movesAndTurns game player) bounds (-1)

-- | forLoopMax, takes a player, a list of possible games, and the current
-- | (alpha, beta) bounds for the pruning. Also takes the current v value
-- | Returns the new v value if greater than the upper bound
-- | Keeps looping with updated lower bound otherwise.
forLoopMax :: Role -> [Game] -> Bounds -> Int -> Int
forLoopMax _ [] _ v = v
forLoopMax player (game:gs) (a,b) v
  | v' >= b = v'
  | otherwise = forLoopMax player gs (max a v', b) v'
  where
    v' = max v $ minValue (a,b) (switch player) game


-- | minValue function as described in the algorithm
-- | Return evaluation value if we reached a terminal state
-- | Calls the helper function forLoopMin to run the loop otherwise
minValue :: Bounds -> Role -> Game -> Int
minValue b p game
  | terminal game = eval game
  | otherwise = forLoopMin p (movesAndTurns game p) b 1


-- | forLoopMin, takes a player, a list of possible games, and the current
-- | (alpha, beta) bounds for the pruning. Also takes the current v value
-- | Returns the new v value if lower than the lower bound
-- | Keeps looping with updated upper bound otherwise.
forLoopMin :: Role -> [Game] -> Bounds -> Int -> Int
forLoopMin _ [] _ v = v
forLoopMin player (game:gs) (a,b) v
  | v' <= a = v'
  | otherwise = forLoopMin player gs (a, min b v') v'
  where
    v' = min v $ maxValue (a,b) (switch player) game


-- | minimax, undefined
minimax:: Role -> Game -> Int
minimax = undefined


--------------------
-- Util functions --
--------------------

-- Join two lists. O(n^2)
-- NOTE: ORDERED iff first list is ordered
-- Way more efficient than appending list and then ordering
insertOrderedBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertOrderedBy f = foldr (insertBy f)

-- Get # of nodes in a graph
-- Simply sqrt of size of adjacent matrix
getNumNodes :: Graph -> Int
getNumNodes = ceiling . sqrt . fromIntegral . length

-- Get the neighbours of a node from an adjacency matrix
-- Takes the nth row, if element is not 0, add its index to the return list
getNeighbours :: Node -> Graph -> [Node]
getNeighbours node graph = helper $ zip [0..] iThRow
  where
    helper :: [(Node, Int)] -> [Node]
    helper [] = []
    helper ((_,0):ns) = helper ns
    helper ((i,_):ns) = i : helper ns
    iThRow = take n . drop (n * node) $ graph
    n = getNumNodes graph
