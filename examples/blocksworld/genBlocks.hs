{-# LANGUAGE 
    FlexibleContexts,
    OverlappingInstances,
    ParallelListComp,
    RankNTypes,
    ScopedTypeVariables
  #-}
{-# OPTIONS_GHC
 -fcontext-stack=30
 -Wall
#-}
module Main where

import Data.List
import System.Environment
import System.Random
import Test.QuickCheck
import Text.Printf

import Planning.PDDL.PDDL3_0
-- import HTNTranslation.Translation

-- Constants for adding stack predicates and items.
stackArity :: Int
stackArity = 1
stackSize :: Int
stackSize = 3

-- Should be something other than a double list for efficiency...
extensions :: [[Integer]]
extensions = 
    map snd $ iterate ext' (0, repeat 1)
    where
        ext' :: (Integer, [Integer]) -> (Integer, [Integer])
        ext' (n, prev) =
            (n + 1, [ (prev !! k) * (n + fromIntegral k) + (prev !! (k+1)) | k <- [0..] ])

pairwiseMap :: (t -> t -> a) -> [t] -> [a]
pairwiseMap _ [] = []
pairwiseMap f l =
    [ f p1 p2 | p1 <- l | p2 <- tail l ]

ratios :: [[Double]]
ratios = map (pairwiseMap (\ x y -> fromIntegral y / fromIntegral x)) extensions

tableProb :: Int -> Int -> Double
tableProb free rooted = 
    let rat = ratios !! free !! rooted in
    rat / (rat + fromIntegral free + fromIntegral rooted)

makeStateWithBlocks :: [(Bool, [a])] -> Gen [(Bool, [a])]
makeStateWithBlocks blocks = do
    let freeTowers = filter (not . fst) blocks
    let rootedTowers = filter fst blocks
    let frt = length freeTowers - 1
    let nrt = length rootedTowers
    let p = tableProb frt nrt
    let m = maxBound `div` 2 :: Int
    let weight = floor $ p * fromIntegral m
    if null freeTowers then return blocks else do
        selected <- frequency [
            (weight, tableBlocks freeTowers rootedTowers), 
            (m - weight, stackBlocks freeTowers rootedTowers)]
        makeStateWithBlocks selected
    where
        tableBlocks (h:restFree) rootedTowers = do
            return $ (True, snd h) : rootedTowers ++ restFree
        stackBlocks (h:restFree) rootedTowers = do
            let choices = rootedTowers ++ restFree
            n <- choose (0, length choices - 1)
            let (before, m:after) = splitAt n choices
            return $ before ++ (fst m, snd h ++ snd m) : after

makeState :: [a] -> Gen [[a]]
makeState blocks = do
    let freeTowers = [(False, [b]) | b <- blocks]
    towers <- makeStateWithBlocks freeTowers
    return $ map snd towers
    
            
            
newRandomList :: (Random a) => (a, a) -> IO [a]
newRandomList range = do
    g <- newStdGen
    let l = randomRs range g
    return l

-- Randomly permute a list
permute :: (Ord b) => [b] -> IO [b]
permute l = do
    -- generate random list of floats
    rl <- newRandomList (0.0 :: Float, 1.0)
    -- zip and sort list according to floats
    let zl = zip rl l
    let sorted = sort zl
    -- return sorted list.
    return $ map snd sorted

makeBlockStack :: forall t f . ((:<:) Const t, (:<:) (Atomic (Expr t)) f) => 
    (String, String, String) -> (Expr t) -> [String] -> [Expr f]
makeBlockStack _ _ [] = []
makeBlockStack (ontable, clear, onblock) _ names =
    eAtomic clear [eConst $ head names :: Expr t] :
    eAtomic ontable [eConst $ last names :: Expr t] :
    [ eAtomic onblock [eConst top, eConst bottom :: Expr t] 
    | top <- names | bottom <- tail names ]


bStateNames :: ([Char], [Char], [Char])
bStateNames = ("on-table", "clear", "on")
bGoalNames :: ([Char], [Char], [Char])
bGoalNames = ("goal-on-table", "goal-clear", "goal-on")

-- Set up the initial and goal states of a problem
makeBlockProblem ::
    String
    -> [String]
    -> [[String]]
    -> [[String]]
    -> PDDLProblem
makeBlockProblem pname blockNames stacks gstacks =
    let
        consts = [ eTyped (eConst b :: Expr Const) ["BLOCK"] | b <- blockNames]
        mkInitStack = makeBlockStack bStateNames (undefined :: ConstTermExpr)
        mkGoalStack = makeBlockStack bStateNames (undefined :: TermExpr)
    in 
    setName pname $
    setDomainName "blocks" $
    setConstants consts $
    setInitial (eAtomic "hand-empty" ([] :: [ConstTermExpr]) 
        : concatMap mkInitStack stacks :: [InitLiteralExpr]) $
    setGoal (Just $ eAnd 
        (concatMap mkGoalStack gstacks :: [PreferenceGDExpr])) 
        emptyProblem

    

genBlocks :: [String] -> IO [[String]]
genBlocks blocks = do
    samples <- sample' $ makeState blocks
    --return $ generate 1 gen $ makeState blocks
    return $ head samples

addGoalsAndTask :: [[String]] -> PDDLProblem -> PDDLProblem
addGoalsAndTask gstacks prob =
    let 
        startTask = eAtomic "start_achieve-goals" ([] :: [ConstTermExpr])
        goals :: [InitLiteralExpr]
        mkGoalStack = makeBlockStack bGoalNames (undefined :: ConstTermExpr)
        goals = concatMap mkGoalStack  gstacks
        initial = startTask : goals ++ getInitial prob
    in
    setInitial initial prob

makeBlocksFiles :: String -> Int -> IO ()
makeBlocksFiles fname blockCount = do
    let blocks = ['b': show n | n <- [1..blockCount]]
    initState <- genBlocks blocks
    goalState <- genBlocks blocks
    let prob = makeBlockProblem fname blocks initState goalState
    -- let hprob = translateProblem stackSize stackArity $ addGoalsAndTask goalState prob
    
    writeFile (fname ++ ".noh.pddl") (show $ pddlDoc prob)
    -- writeFile (fname ++ ".htn.pddl") (show hprob)

main :: IO ()
main = do
    [fname, blockStart, blockEnd, blockStep] <- getArgs
    let blockCounts = toCount blockStart blockEnd blockStep
    let fmtStr = fname ++ "_%0" ++ (show $ length blockEnd) ++ "d"
    sequence_ [
        makeBlocksFiles (printf fmtStr blocks) blocks
        | blocks <- blockCounts ]
        
    where
        toCount startStr endStr stepStr = 
            takeWhile (<= read endStr) $ 
            iterate (+ read stepStr) $ 
            read startStr
    
