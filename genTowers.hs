{-# OPTIONS
 -fglasgow-exts
#-}

module Main where

import Control.Monad
import Data.List
import System.Environment
import System.IO

import Planning.PDDL.Representation
import HTNTranslation.Translation (translateProblem)

genTowers :: Int -> StandardProblem
genTowers n =
    let
        ec :: Expr Const
        ec = undefined
        et :: TermExpr
        et = undefined
        towers :: (:<:) Const e => Expr e -> [Expr e]
        towers e = [(eConst $ 't' : show t) `asTypeOf` e | t <- [1..3]]
        rings :: (:<:) Const e => Expr e -> [Expr e]
        rings e = [(eConst $ 'r' : show r) `asTypeOf` e | r <- [1..n]]
        stack :: [Expr (Atomic (Expr Const))]
        stack = [ eAtomic "on" [ r1, r2 ] | r1 <- rings ec | r2 <- tail $ rings ec ]
    in
    Problem {
        problemName = "tower_problem_" ++ show n,
        problemDomain = "towers",
        problemRequirements = [],
        objects = 
            [ eTyped t (eConst "TOWER") | t <- towers ec] ++
            [ eTyped r (eConst "RING") | r <- rings ec],
        initial =
            [ eAtomic "smallerThan" [r,t] | r <- rings ec, t <- towers ec] ++
            [ eAtomic "smallerThan" [head rl, r2] 
                | rl <- init $ tails $ rings ec, r2 <- tail rl] ++
            stack ++
            [eAtomic "on" [last $ rings ec, towers ec !! 0],
             eAtomic "towerTop" [head $ rings ec, towers ec !! 0],
             eAtomic "towerTop" [towers ec !! 1, towers ec !! 1],
             eAtomic "towerTop" [towers ec !! 2, towers ec !! 2]],
        goal = Just $ eAnd $
            [eAtomic "on" [r1, o2]
             | r1 <- rings et
             | o2 <- tail (rings et ++ [towers et !! 2])],
        constraints = Nothing
        }

addInitialTask prob = prob { initial = initial prob ++ [eAtomic "start_shiftTower" [eConst "t1", eConst "t2", eConst "t3" :: Expr Const]] }

main = do
    fname : sizeStr : _ <- getArgs
    let basename = fname ++ sizeStr
    let prob = genTowers $ read sizeStr
    writeFile (basename ++ ".pddl") (show prob)
    writeFile (basename ++ ".hpddl") (show $ translateProblem 3 1 $ addInitialTask prob)

