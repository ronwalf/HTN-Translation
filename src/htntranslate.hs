{-# OPTIONS_GHC
    -fcontext-stack=30
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
  #-}
module Main where

{-
import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

import HTNTranslation.HTNPDDL
import HTNTranslation.Translation
import Planning.PDDL.Parser

class Functor f => AtomicFinder t f where
    --atomicFinder :: f [Expr (Atomic t)]-> [Expr (Atomic t)]
    atomicFinder :: f [t] -> [t]
instance (AtomicFinder t f, AtomicFinder t g) 
    => AtomicFinder t (f :+: g) where
    atomicFinder (Inl x) = atomicFinder x
    atomicFinder (Inr y) = atomicFinder y

--instance AtomicFinder t (Atomic t) where
--    atomicFinder (Atomic p tl) = [eAtomic p tl]
instance (:<:) (Atomic t) f => AtomicFinder (Expr f) (Atomic t) where
    atomicFinder (Atomic p tl) = [eAtomic p tl]
instance AtomicFinder t And where
    atomicFinder (And el) = concat el
instance AtomicFinder t Or where
    atomicFinder (Or el) = concat el
instance AtomicFinder t Not where
    atomicFinder (Not e) = e
instance AtomicFinder t Imply where
    atomicFinder (Imply e1 e2) = e1 ++ e2
instance AtomicFinder t (Exists v) where
    atomicFinder (Exists _ e) = e
instance AtomicFinder t (ForAll v) where
    atomicFinder (ForAll _ e) = e
instance AtomicFinder t Preference where
    atomicFinder (Preference _ e) = e


findAtomics :: AtomicFinder (Expr h) g => Expr g -> [Expr h]
findAtomics = foldExpr atomicFinder


class AtomicRenamer g f where
    atomicRenamer :: (String -> String) -> f (Expr g) -> Expr g
instance (AtomicRenamer h f, AtomicRenamer h g) 
    => AtomicRenamer h (f :+: g) where
    atomicRenamer t (Inl x) = atomicRenamer t x
    atomicRenamer t (Inr y) = atomicRenamer t y

instance (:<:) (Atomic t) f => AtomicRenamer f (Atomic t) where
    atomicRenamer t (Atomic p tl) = eAtomic (t p) tl
instance (:<:) And f => AtomicRenamer f And where
    atomicRenamer _ (And el) = eAnd el
instance (:<:) Or f => AtomicRenamer f Or where
    atomicRenamer _ (Or el) = eOr el
instance (:<:) Not f => AtomicRenamer f Not where
    atomicRenamer _ (Not e) = eNot e
instance (:<:) Imply f => AtomicRenamer f Imply where
    atomicRenamer _ (Imply e1 e2) = eImply e1 e2
instance (:<:) (Exists v) f => AtomicRenamer f (Exists v) where
    atomicRenamer _ (Exists vl e) = eExists vl e
instance (:<:) (ForAll v) f => AtomicRenamer f (ForAll v) where
    atomicRenamer _ (ForAll vl e) = eForAll vl e
instance (:<:) Preference f => AtomicRenamer f Preference where
    atomicRenamer _ (Preference n e) = ePreference n e


renameAtomics :: (Functor g, AtomicRenamer g g) => (String -> String) -> Expr g -> Expr g
renameAtomics h = foldExpr (atomicRenamer h)


class ConstFinder g f where
    constFinder :: f (Maybe (Expr g)) -> Maybe (Expr g)
instance (ConstFinder h f, ConstFinder h g) => ConstFinder h (f :+: g) where
    constFinder (Inl x) = constFinder x
    constFinder (Inr y) = constFinder y

instance (:<:) Const f => ConstFinder f Const where
    constFinder (Const c) = Just $ eConst c
instance ConstFinder f Var where
    constFinder _ = Nothing
instance ConstFinder f Function where
    constFinder _ = Nothing

findConst :: (Functor f, Functor g, ConstFinder g f) => Expr f -> Maybe (Expr g)
findConst = foldExpr constFinder

constAtomic :: (Functor f, Functor g, Functor h, ConstFinder g f, 
    Atomic (Expr g) :<: h) =>
    Expr g -> Expr (Atomic (Expr f)) -> Maybe (Expr h)
constAtomic constTemplate (In (Atomic p tl)) =
    let consts = mapMaybe findConst tl `asTypeOf` [constTemplate] in
    if (length tl == length consts) then
        Just $ eAtomic p consts
    else
        Nothing

errCheck :: (Show t) => Either t b -> IO b
errCheck (Left err) = do
    hPrint stderr err
    exitFailure 
errCheck (Right prob) = return prob


processProblem :: 
    Expr (Atomic ConstTermExpr) 
    -> [CallNode]
    -> Int -> Int 
    -> FilePath -> FilePath 
    -> IO [String]
processProblem task callNodes arity items pfile pfile' = do
    let countNums = replicate arity items
    contents <- readFile pfile
    prob <- errCheck $ runParser pddlProblemParser emptyProblem pfile contents
    let (counters :: [[ConstTermExpr]], prob') = renderCounters countNums prob
    let prob'' = renderInitTask task callNodes counters prob'
    let atomicGoals =
            maybe [] (findAtomics . renameAtomics ("goal-"++)) (getGoal prob)
            :: [Expr PDDLAtom]
    let goalPreds =
            nub $
            sort $
            map taskName $
            maybe [] (findAtomics :: PreferenceGDExpr -> [Expr PDDLAtom]) $
            getGoal prob

    let initGoals =
            getInitial prob''
            ++ mapMaybe (constAtomic (undefined :: ConstTermExpr)) atomicGoals
    writeFile pfile' (show $ setInitial initGoals prob'')
    return goalPreds

processDomain ::
    FilePath -> IO (PDDLDomain, [CallNode])
processDomain dfile = do
    contents <- readFile dfile
    dom <- errCheck $ parseHTNPDDL dfile contents
    let (dom', callNodes) = deconstructHDomain dom
    let dom'' = renderCalls dom' callNodes
    return (dom'', callNodes)

stripPostfix :: String -> String
stripPostfix =
    -- reverse . tail . (dropWhile (/= '.')) . reverse
    takeWhile (/= '.')

taskParser :: CharParser a (Expr (Atomic ConstTermExpr))
taskParser = parens pddlLexer $
    atomicParser pddlLexer $ constTermParser pddlLexer

main :: IO ()
main = do
    arityS:itemsS:domFile:domFile':taskstr:problems <- getArgs
    let arity = read arityS :: Int
    let items = read itemsS :: Int
    task <- errCheck $ runParser taskParser () taskstr taskstr
    (dom, callNodes) <- processDomain domFile
    goalPredNames <- liftM (sort . nub . concat) $ sequence [
        processProblem
            task
            callNodes
            arity
            items
            pfile
            (stripPostfix pfile ++ ".htn1.pddl")
        | pfile <- problems]
    let cPredNames = map taskName $
            getPredicates dom
    let preds :: [Expr (Atomic TypedVarExpr)] = (++) (getPredicates dom) $
            filter (not . flip elem cPredNames . taskName) $
            map (renameAtomics ("goal-"++)) $
            filter (flip elem goalPredNames . taskName) $
            getPredicates dom
    writeFile domFile' $ show $ setPredicates preds dom
    return ()
-}

main :: IO ()
main = return ()