module HTNTranslation.TestProgressionBounds where

import Control.Monad
import Data.List
import Test.HUnit


import HTNTranslation.HTNPDDL
import qualified HTNTranslation.ProgressionBounds as PB


taskA x = eAtomic "a" [eConst x :: TermExpr]
taskB x = eAtomic "b" [eConst x :: TermExpr]
taskAA x y = eAtomic "aa" [eConst x :: TermExpr, eConst y]
taskBB x y = eAtomic "bb" [eConst x :: TermExpr, eConst y]
taskC = eAtomic "c" ([] :: [TermExpr])
taskD = eAtomic "d" ([] :: [TermExpr])
taskE = eAtomic "e" ([] :: [TermExpr])
taskF = eAtomic "f" ([] :: [TermExpr])

sortTaskBounds :: [([String], Int)] -> [([String], Int)]
sortTaskBounds = map (\(n,b) -> (sort n, b))

testDomain :: StandardHTNDomain
testDomain = setActions actions emptyHDomain
    where
    actions =
        [ setName "Action_A" $
            setTaskHead (Just $ taskA "p") $
            defaultMethod
        , setName "Action_B" $
            setTaskHead (Just $ taskB "p") $
            defaultMethod
        , setName "Method_AA" $
            setTaskHead (Just $ taskAA "p" "q") $
            setTaskLists [(Nothing, [taskA "p", taskA "q"])] $
            defaultMethod
        , setName "Method_BB" $
            setTaskHead (Just $ taskBB "p" "q") $
            setTaskLists [(Nothing, [taskB "p"]), (Nothing, [taskA "q"])] $
            defaultMethod
        , setName "Method_Cend" $
            setTaskHead (Just $ taskC) $
            defaultMethod
        , setName "Method_CC" $
            setTaskHead (Just $ taskC) $
            setTaskLists 
                [ (Just "A", [taskAA "p" "q"])
                , (Just "B", [taskBB "p" "q"])
                , (Just "C", [taskC])] $
            setTaskConstraints [("A", "C"), ("B", "C")] $
            defaultMethod
        , setName "Method_Dend" $
            setTaskHead (Just $ taskD) $
            defaultMethod
        , setName "Method_D" $
            setTaskHead (Just $ taskD) $
            setTaskLists [(Nothing, [taskC, taskD, taskC])] $
            defaultMethod
        , setName "Method_Eend" $
            setTaskHead (Just $ taskE) $
            defaultMethod
        , setName "Method_E" $
            setTaskHead (Just $ taskE) $
            setTaskLists [(Nothing, [taskD]), (Nothing, [taskD])] $
            defaultMethod
        , setName "Method_EF" $
            setTaskHead (Just $ taskE) $
            setTaskLists [(Nothing, [taskC, taskF])] $
            defaultMethod
        , setName "Method_F" $
            setTaskHead (Just $ taskF) $
            setTaskLists [(Nothing, [taskD, taskE])] $
            defaultMethod
        ]
    

findMethodsTest1 = TestLabel "findMethods Test 1" $ TestCase $
    assertEqual "found wrong methods" ["Method_CC", "Method_Cend"] $ sort $ map getName $ PB.findMethods testDomain "c"

findMethodsTest2 = TestLabel "findMethods Test 2" $ TestCase $
    assertEqual "found wrong methods" ["Method_E", "Method_EF", "Method_Eend"] $ 
    sort $ map getName $ PB.findMethods testDomain "e"

findReachableTasksTest1 = TestLabel "findReachableTasks Test Reachable 1" $ TestCase $
    let
    problem = 
        setTaskHead (Just $ eAtomic "c" ([] :: [ConstTermExpr])) $
        (emptyHProblem :: StandardHTNProblem)
    in
    assertEqual "Should be empty" ["a", "aa", "b", "bb", "c"] $ sort $ PB.findReachableTasks testDomain problem

findReachableTasksTest2 = TestLabel "findReachableTasks Test Reachable 2" $ TestCase $
    let
    problem = 
        setTaskHead (Just $ eAtomic "e" ([] :: [ConstTermExpr])) $
        (emptyHProblem :: StandardHTNProblem)
    in
    assertEqual "Should be empty" ["a", "aa", "b", "bb", "c", "d", "e", "f"] $ sort $ PB.findReachableTasks testDomain problem

taskCyclesTest1 = TestLabel "Task Cycles Test 1" $ TestCase $
    let
    problem =
        setTaskHead (Just $ eAtomic "c" ([] :: [ConstTermExpr])) $
        (emptyHProblem :: StandardHTNProblem)
    in
    assertEqual "Mismatched task cycle list" [["a"], ["aa"], ["b"], ["bb"], ["c"]] $ 
    sort $ map sort $
    PB.taskCycles testDomain problem

taskCyclesTest2 = TestLabel "Task Cycles Test 2" $ TestCase $
    let
    problem =
        setTaskHead (Just $ eAtomic "e" ([] :: [ConstTermExpr])) $
        (emptyHProblem :: StandardHTNProblem)
    in
    assertEqual "Mismatched task cycle list" [["a"], ["aa"], ["b"], ["bb"], ["c"], ["d"], ["e", "f"]] $ 
    sort $ map sort $
    PB.taskCycles testDomain problem

boundsGameTest1 = TestLabel "boundsGame Test 1" $ TestCase $
    let
    tA = taskA "p"
    tB = taskB "p"
    action = setName "Method_CC" $
        setTaskHead (Just $ taskC) $
        setTaskLists 
            [ (Just "A", [tA])
            , (Just "B", [tB])
            , (Just "C", [taskC])] $
        setTaskConstraints [("A", "C"), ("B", "C")] $
        defaultMethod :: StandardMethod 
    in
    assertEqual "Wrong task weight for tail recursive method" 3 $
    PB.boundsGame action [([taskName $ tA, taskName $ tB], 1)]

boundsGameTest2 = TestLabel "boundsGame Test 2" $ TestCase $
    let
    tA = taskAA "p" "q" 
    tB = taskBB "p" "q" 
    action = setName "Method_CC" $
        setTaskHead (Just $ taskC) $
        setTaskLists 
            [ (Just "A", [tA])
            , (Just "B", [tB])
            , (Just "C", [taskC])] $
        setTaskConstraints [("A", "C"), ("B", "C")] $
        defaultMethod :: StandardMethod 
    in
    assertEqual "Wrong task weight for tail recursive method" 5 $
    PB.boundsGame action [([taskName $ tA, taskName $ tB], 2)]

boundsGameTest3 = TestLabel "boundsGame Test 3" $ TestCase $
    let
    tA = taskA "p" 
    tB = taskB "p" 
    tAA = taskAA "p" "q" 
    tBB = taskBB "p" "q" 
    action = setName "Method_CC" $
        setTaskHead (Just $ taskC) $
        setTaskLists 
            [ (Just "A", [tA, tAA])
            , (Just "B", [tB, tBB])
            , (Just "C", [taskC])] $
        setTaskConstraints [("A", "C"), ("B", "C")] $
        defaultMethod :: StandardMethod 
    in
    assertEqual "Wrong task weight for tail recursive method" 5 $
    PB.boundsGame action 
        [ ([taskName $ tA, taskName $ tB], 1)
        , ([taskName $ tAA, taskName $ tBB], 2)]

boundsGameTest4 = TestLabel "boundsGame Test 4" $ TestCase $
    let
    tA = taskA "p" 
    tB = taskB "p" 
    tAA = taskAA "p" "q" 
    tBB = taskBB "p" "q" 
    action = setName "Method_CC" $
        setTaskHead (Just $ taskC) $
        setTaskLists 
            [ (Just "A1", [tA])
            , (Just "A2", [tB])
            , (Just "B1", [tBB])
            , (Just "B2", [tAA])
            , (Just "B3", [tBB])
            , (Just "C", [taskC])] $
        setTaskConstraints 
        [ ("A1", "B1")
        , ("A1", "B2")
        , ("A1", "B3")
        , ("A2", "B1")
        , ("A2", "B2")
        , ("A2", "B3")
        , ("B1", "C")
        , ("B2", "C")
        , ("B3", "C")] $
        defaultMethod :: StandardMethod 
    in
    assertEqual "Wrong task weight for tail recursive method" 7 $
    PB.boundsGame action 
        [ ([taskName $ tA, taskName $ tB], 1)
        , ([taskName $ tAA, taskName $ tBB], 2)]


boundsGameTest5 = TestLabel "boundsGame Test 5" $ TestCase $
    let
    tA = taskA "p" 
    action = setName "Action_A" $
        setTaskHead (Just $ tA) $
        defaultMethod :: StandardMethod 
    in
    assertEqual "Wrong task weight for action" 1 $
    PB.boundsGame action []

boundsGameTest6 = TestLabel "boundsGame Test 6" $ TestCase $
    let
    tA = taskA "p"
    action = setName "Method_AAA" $
        setTaskHead (Just $ taskAA "p" "q") $
        setTaskLists 
            [ (Just "A", [tA])
            , (Just "B", [tA])
            , (Just "C", [tA])] $
        defaultMethod :: StandardMethod 
    in
    assertEqual "Wrong task weight for tail recursive method" 3 $
    PB.boundsGame action [([taskName $ tA], 1)]


boundsProgressionTest1 = TestLabel "boundsProgression Test 1" $ TestCase $
    let
    problem = 
        setTaskHead 
        (Just $ eAtomic "a" [eConst "p" :: ConstTermExpr]) $
        emptyHProblem  :: StandardHTNProblem
    in
    assertEqual "Wrong bounds for problem tasks" (Just [(["a"], 1)]) $
    liftM sortTaskBounds $
    PB.boundProgression testDomain problem

boundsProgressionTest2 = TestLabel "boundsProgression Test 2" $ TestCase $
    let
    problem = 
        setTaskHead 
        (Just $ eAtomic "c" ([] :: [ConstTermExpr])) $
        emptyHProblem  :: StandardHTNProblem
    in
    assertEqual "Wrong bounds for problem tasks" (Just $ sort [(["c"], 5), (["aa"], 2), (["bb"], 2), (["a"], 1), (["b"], 1)]) $
    liftM (sort . sortTaskBounds) $
    PB.boundProgression testDomain problem


tests = TestLabel "Progression Bound Tests" $ TestList $
    [ findMethodsTest1
    , findMethodsTest2
    , findReachableTasksTest1
    , findReachableTasksTest2
    , taskCyclesTest1
    , taskCyclesTest2
    , boundsGameTest1
    , boundsGameTest2
    , boundsGameTest3
    , boundsGameTest4
    , boundsGameTest5
    , boundsGameTest6
    , boundsProgressionTest1
    , boundsProgressionTest2
    ]
