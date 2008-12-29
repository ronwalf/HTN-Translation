{-# OPTIONS
 -fglasgow-exts
 -fallow-overlapping-instances
 -fallow-undecidable-instances
 -fcontext-stack=40
#-}
module Main where

import Control.Monad
import Data.Function
import Data.List
import System.Environment
import System.Random
import Test.QuickCheck

import Planning.PDDL.PDDL3_0
import HTNTranslation.Translation

-- Constants for adding stack predicates and items.
stackArity = 1
stackSize = 3

-- Should be something other than a double list for efficiency...
extensions :: [[Integer]]
extensions = 
    map snd $ iterate ext' (0, repeat 1)
    where
        ext' :: (Integer, [Integer]) -> (Integer, [Integer])
        ext' (n, prev) =
            (n + 1, [ (prev !! k) * (n + fromIntegral k) + (prev !! (k+1)) | k <- [0..] ])

pairwiseMap f [] = []
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
            let (before, mod:after) = splitAt n choices
            return $ before ++ (fst mod, snd h ++ snd mod) : after

makeState :: [a] -> Gen [[a]]
makeState blocks = do
    let freeTowers = [(False, [b]) | b <- blocks]
    towers <- makeStateWithBlocks freeTowers
    return $ map snd towers
    
            
            

newRandomList range = do
    g <- newStdGen
    let l = randomRs range g
    return l

-- Randomly permute a list
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
makeBlockStack (ontable, clear, onblock) template names =
    eAtomic clear [eConst $ head names :: Expr t] :
    eAtomic ontable [eConst $ last names :: Expr t] :
    [ eAtomic onblock [eConst top, eConst bottom :: Expr t] 
    | top <- names | bottom <- tail names ]


bStateNames = ("on-table", "clear", "on")
bGoalNames = ("goal-on-table", "goal-clear", "goal-on")

-- Set up the initial and goal states of a problem
makeBlockProblem pname blockNames stacks gstacks =
    let
        consts = [ eTyped (eConst b :: Expr Const) (eConst "BLOCK") | b <- blockNames]
        mkInitStack = makeBlockStack bStateNames (undefined :: ConstTermExpr)
        mkGoalStack = makeBlockStack bStateNames (undefined :: TermExpr)
    in 
    setName pname $
    setDomainName "blocks" $
    setConstants consts $
    setInitial (concatMap mkInitStack stacks :: [InitLiteralExpr]) $
    setGoal (Just $ eAnd 
        (concatMap mkGoalStack gstacks :: [PreferenceGDExpr])) 
        emptyProblem

    

genBlocks :: [String] -> IO [[String]]
genBlocks blocks = do
    gen <- newStdGen
    return $ generate 1 gen $ makeState blocks

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


main = do
    [fname, blocksStr] <- getArgs
    let blockCount = read blocksStr
    let blocks = ['b': show n | n <- [1..blockCount]]
    initState <- genBlocks blocks
    goalState <- genBlocks blocks
    let prob = makeBlockProblem fname blocks initState goalState
    let hprob = translateProblem stackSize stackArity $ addGoalsAndTask goalState prob
    
    writeFile (fname ++ ".pddl") (show prob)
    writeFile (fname ++ ".hpddl") (show hprob)

