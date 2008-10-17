-- Randomness Utilities
module RandomUtils (
    allPaths,
    mkRandomPaths,
    newRandomList,
    newRandomNamedList,
    permute,
    treeNodes
) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.MST
import List
import System.Random


-- Cartesian product
cartesianProduct :: [a] -> [(a,a)]
cartesianProduct =
    (snd . collect)
    where
        pair x y = (x,y)
        collect = 
            foldr (\x (l,c) -> (x:l, (map (pair x) l) ++ c)) ([],[])

-- Make a complete graph from the given edge labels
mkComplete :: Int -> [Float] -> Gr () Float
mkComplete size weights =
    let
        nodes = take size $ zip (iterate (+1) 1) (repeat ())
        edges = cartesianProduct $ take size $ map fst nodes
        ledges = zipWith (\(x, y) w -> (x, y, w)) edges weights
    in
    undir $ mkGraph nodes ledges


-- Make a random tree of the given size, with node 
-- labels from [1..size]
mkRandomTree size = do
    root <- getStdRandom ( randomR (1, size) )
    gen <- newStdGen
    let weights = randomRs (0, 1) gen in
        return $ msTreeAt root $ mkComplete size weights


-- Makes a random tree of the given size, returning 
-- (parent, child)  pairs labeled with the given function.
mkRandomPaths :: Int -> ( Int -> a ) -> IO [(a, a)]
mkRandomPaths size naming = do
    tree <- mkRandomTree size
    return $ map (\(x:y:t) -> (y, x)) $ named tree
    where
        paths tree = filter (\(LP x) -> 1 < length x) tree
        named tree= map (\(LP path) -> map (\(x, _) -> naming x) path) $ paths tree
--
-- Path Finding
--

-- Get the root node of a tree
root :: [(a,a)] -> a 
root t =
    fst $ head t

-- Get all nodes of a tree
treeNodes :: [(a,a)] -> [a]
treeNodes t =
    (root t) : (map snd t)

-- Find all parents of a node (in order)
parents :: Eq a => a -> [(a, a)] -> [a]
parents n =
    foldr addParent [n]
    where
        addParent (p,c) (n:l) | n==c = p : n : l
                              | otherwise = n : l

-- Take two paths starting at a common node and find the minimal path between them
path :: Eq a => [a] -> [a] -> [a]
path l1 l2 =
    let
        (s2, rs1, h) = foldl builder (l2, [], head l1) l1
    in
    concat [rs1, [h], s2]
    where
        builder ([], r, h) a = ( [], a:r, h )
        builder (x:l, r, h) a
            | x==a = ( l, r, x )
            | otherwise = ( x:l, a:r, h )

-- Find a path between two nodes in the given tree
findPath :: Eq a => [(a,a)] -> a -> a -> [a]
findPath t n1 n2 =
    let
        p1 = parents n1 t
        p2 = parents n2 t
    in
    path p1 p2

allPaths :: Eq a => [(a,a)] -> [[a]]
allPaths t =
    foldr (\ (x,y) l -> (bothPaths t x y)++l) [] $ cartesianProduct $ treeNodes t
    where
        bothPaths t n1 n2 =
            let
                pth = findPath t n1 n2
            in
            [pth, reverse pth]



newRandomList x = do
    --getStdRandom ( randomR (0.0 :: Float, 1.1) )
    gen <- newStdGen
    return $ randomRs x gen

newRandomNamedList x naming = do
    rl <- newRandomList x
    return $ map naming rl


permute l = do
    rl <- newRandomList (0.0 :: Float, 1.0)
    let zl = zip rl l
    let sorted = sort zl
    return $ map snd sorted
