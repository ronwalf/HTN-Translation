{-# OPTIONS
 -fcontext-stack=40
#-}
{-# LANGUAGE FlexibleContexts, ParallelListComp #-}

module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO
import Text.Printf

import Planning.PDDL.PDDL3_0
--import HTNTranslation.Translation (translateProblem)

genTowers :: Int -> PDDLProblem
genTowers n =
    let
        ec :: ConstTermExpr
        ec = undefined
        et :: TermExpr
        et = undefined
        towers :: (:<:) Const e => Expr e -> [Expr e]
        towers e = [(eConst $ 't' : show t) `asTypeOf` e | t <- [1..3]]
        rings :: (:<:) Const e => Expr e -> [Expr e]
        rings e = [(eConst $ 'r' : show r) `asTypeOf` e | r <- [1..n]]
        stack :: [InitLiteralExpr]
        stack = [ eAtomic "on" [ r1, r2 ] | r1 <- rings ec | r2 <- tail $ rings ec ]
    in
    setName ("tower_problem_" ++ show n) $
    setDomainName "towers" $
    setConstants (
        [ eTyped t ["TOWER"]| t <- towers (undefined :: Expr Const)] ++
        [ eTyped r ["RING"] | r <- rings (undefined :: Expr Const)]) $
    setInitial ( 
        [ eAtomic "smallerThan" [r,t] | r <- rings ec, t <- towers ec] ++
        [ eAtomic "smallerThan" [head rl, r2] 
            | rl <- init $ tails $ rings ec, r2 <- tail rl] ++
        stack ++
        [eAtomic "on" [last $ rings ec, towers ec !! 0],
        eAtomic "towerTop" [head $ rings ec, towers ec !! 0],
        eAtomic "towerTop" [towers ec !! 1, towers ec !! 1],
        eAtomic "towerTop" [towers ec !! 2, towers ec !! 2]]) $
    setGoal (Just $ eAnd $
        [eAtomic "on" [r1, o2]
            | r1 <- rings et
            | o2 <- tail (rings et ++ [towers et !! 2])])
    emptyProblem

makeProblemFiles (basename, size) = do
    let prob = genTowers size
    writeFile (basename ++ ".pddl") (show $ pddlDoc prob)

main = do
    fname : sizeStrs <- getArgs
    let maxDigits = maximum $ map length sizeStrs
    let sizes = map read sizeStrs :: [Int]
    let formatStr = printf "%%s%%0%dd" maxDigits
    let basenames = map (printf formatStr fname) sizes
    mapM_ makeProblemFiles $ zip basenames sizes

