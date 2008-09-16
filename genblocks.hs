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

import Planning.PDDL.PDDL3_0
import HTNTranslation.Translation

-- Constants for adding stack predicates and items.
stackArity = 1
stackSize = 6

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

    

genBlocks :: Int -> [String] -> IO [[String]]
genBlocks towerCount blocks = do
    bnames <- permute blocks 
    towers <- return (sort . (take $ length blocks)) `ap` newRandomList (1, towerCount)
    return $ map (map snd) $ groupBy ((==) `on` fst) $ zip towers bnames


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
    [towersStr, blocksStr, fname] <- getArgs
    let basename = fname ++ towersStr ++ "_" ++ blocksStr
    let blockCount = read blocksStr
    let towerCount = read towersStr 
    let bnames = ['b' : show n | n <- [1 .. blockCount]]
    initState <- genBlocks towerCount bnames
    goalBlocks <- return (take $ blockCount `div` 2 ) `ap` permute bnames
    goalState <- genBlocks (towerCount `div` 2) goalBlocks
    
    let prob = makeBlockProblem basename bnames initState goalState
    let hprob = translateProblem stackSize stackArity $ addGoalsAndTask goalState prob
    
    writeFile (basename ++ ".pddl") (show prob)
    writeFile (basename ++ ".hpddl") (show hprob)

