module HTNTranslation.Test where

import Data.List
import Prelude hiding (and, const, not, or)
import qualified Prelude
import Test.HUnit

import HTNTranslation.HTNPDDL
import HTNTranslation.Translation

vars :: [Expr Var]
vars = [ eVar $ 'v' : show n | n <- [0..5] ]


stackVarsTest = TestLabel "StackVarTest" $
    TestCase $
    let
        vars = take 5 $ iterate (\ vl -> stackVars vl 5 ++ vl) []
    in
    (flip mapM_) vars $ \vl ->
    assertEqual "StackVar intersection non-nill!" [] $
    intersect vl $ stackVars vl 5

stackConditionTest1 = TestLabel "Stack Condition Test 1" $
    TestCase $
    let 
        (ovl, nvl) = splitAt 1 $stackVars [] 2 
        otl = varIds ovl :: [TermExpr]
        ntl = varIds nvl :: [TermExpr]
    in
    assertEqual ""
    (eAnd [stackTop otl,
        eOr [eAnd [nextP (otl !! 0) (ntl !! 0)]]])
    (stackCondition otl ntl :: GoalExpr)

stackConditionTest2 = TestLabel "Stack Condition Test 2" $
    TestCase $
    let 
        (ovl, nvl) = splitAt 2 $ stackVars [] 4 
        otl = varIds ovl :: [TermExpr]
        ntl = varIds nvl :: [TermExpr]
    in
    assertEqual ""
    (eAnd [stackTop otl, eOr [
        eAnd [
            nextP (otl !! 0) (ntl !! 0),
            sameP (otl !! 1) (ntl !! 1)],
        eAnd [
            endP (otl !! 0),
            beginP (ntl !! 0),
            nextP (otl !! 1) (ntl !! 1)]
        ]])
    (stackCondition otl ntl :: GoalExpr)

stackConditionTest3 = TestLabel "Stack Condition Test 3" $
    TestCase $
    let 
        (ovl, nvl) = splitAt 3 $ stackVars [] 6 
        otl = varIds ovl :: [TermExpr]
        ntl = varIds nvl :: [TermExpr]
    in
    assertEqual ""
    (eAnd [stackTop otl, eOr [
        eAnd [
            nextP (otl !! 0) (ntl !! 0),
            sameP (otl !! 1) (ntl !! 1),
            sameP (otl !! 2) (ntl !! 2)],
        eAnd [
            endP (otl !! 0),
            beginP (ntl !! 0),
            nextP (otl !! 1) (ntl !! 1),
            sameP (otl !! 2) (ntl !! 2)],
        eAnd [
            endP (otl !! 0),
            beginP (ntl !! 0),
            endP (otl !! 1),
            beginP (ntl !! 1),
            nextP (otl !! 2) (ntl !! 2)]
        ]])
    (stackCondition otl ntl :: GoalExpr)



stackTests = TestLabel "Stack Tests" $
    TestList [
        stackVarsTest,
        stackConditionTest1,
        stackConditionTest2,
        stackConditionTest3
        ]

    
