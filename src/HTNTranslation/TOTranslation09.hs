{-# OPTIONS_GHC
    -freduction-depth=30
    -Wall
  #-}
{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  FunctionalDependencies,
  IncoherentInstances,
  MultiParamTypeClasses,
  OverloadedStrings,
  ParallelListComp,
  RankNTypes,
  ScopedTypeVariables,
  TypeOperators
  #-}
module HTNTranslation.TOTranslation09
where

import Control.Monad
import Control.Monad.State
import Data.List
-- import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, append, pack, unpack)
-- import Text.Printf

import Planning.Records
import Planning.Expressions
import Planning.Util
import HTNTranslation.HTNPDDL
import HTNTranslation.TOTranslation hiding (translateMethod)

translateMethod :: forall m dom sdom template action pre eff.
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Maybe Text, Expr pre) action,
     HasPrecondition (Maybe Text, Expr pre) template,
     Atomic TermExpr :<: pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre, ForAll TypedVarExpr :<: pre,
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) action,
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) template,
     Atomic TermExpr:<: eff, Not :<: eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskList TermExpr action,
     HasTaskOrdering action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> m ()
translateMethod m = do
    guard $ isJust $ getTaskHead m
    guard $ not $ null $ getTaskList m
    template <- getTemplate
    sdom <- getSDomain
    let task = fromJust $ getTaskHead m
    when (isNothing $ findFirstTask m) $ error $ "Method " ++ (show $ getName m) ++ " has no first task (can't use totally-ordered translation)"
    let firstTask = fromJust $ findFirstTask m
    let hid = htnIdV 1
    let hidn = htnIdV 2
    let params = getParameters m ++ [htnIdP 1, htnIdP 2]
    let precond =
            (Nothing, taskP (taskName task) (taskArgs task) hid)
            : (Nothing, topP hid)
            : (Nothing, nextIdP hid hidn)
            : getPrecondition m
    tasklist <- mkTaskList firstTask
    nextTask <- controlOps tasklist
    let effect =
            [ ([], Nothing,
                [ eNot $ taskP (taskName task) (taskArgs task) hid
                , taskP (taskName $ snd firstTask) (taskArgs $ snd firstTask) hidn
                , eNot (topP hid)
                , topP hidn
                , nextTask hid
                ])]
            ++ getEffect m
    let action =
            setName (append "htn_" $ getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect $
            template
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) (htnIdP 1))
    addAction action
    return ()
    where
    mkTaskList :: (Int, Expr (Atomic TermExpr)) -> m [(Int, Expr (Atomic TermExpr))]
    mkTaskList (n,_) = do
        case findNextTasks m n of
            [] -> return []
            [t] -> liftM (t : ) $ mkTaskList t
            _ -> error $ "Method " ++ (show $ getName m) ++ " is not totally ordered!"
    controlOps :: [(Int, Expr (Atomic TermExpr))] -> m (TermExpr -> Expr eff)
    controlOps [] = error $ "Error! Empty list for controlOps"
    controlOps tl@[(n,t)] = do
        template <- getTemplate
        let hid = htnIdV 1
        let params = remainingParams tl
        let pterms :: [TermExpr] = map liftE $ concatMap findFreeVars params
        let cpred = controlP (getName m) n pterms
        let precond =
                [ (Nothing, controlP (getName m) n pterms hid)
                , (Nothing, topP hid) ]
        let effect = [( [], Nothing,
                [ eNot $ cpred hid
                , taskP (taskName t) (taskArgs t) hid
                ])]
        let action =
                setName (pack $ "htn_control_" ++ (unpack $ getName m) ++ "_" ++ show n) $
                setParameters (params ++ [htnIdP 1]) $
                setPrecondition precond $
                setEffect effect $
                template
        ensurePred (controlP (getName m) n params (htnIdP 1))
        addAction action
        return cpred
    controlOps tl@((n,t):_) = do
        template <- getTemplate
        let hid = htnIdV 1
        let hidn = htnIdV 2
        let params = remainingParams tl
        let pterms :: [TermExpr] = map liftE $ concatMap findFreeVars params
        let cpred = controlP (getName m) n pterms
        npred <- controlOps $ tail tl
        let precond =
                [ (Nothing, controlP (getName m) n pterms hid)
                , (Nothing, topP hid)
                , (Nothing, nextIdP hid hidn)
                ]
        let effect = [( [], Nothing,
                [ eNot $ cpred hid
                , eNot $ topP hid
                , topP hidn
                , taskP (taskName t) (taskArgs t) hidn
                , npred hid
                ])]
        let action =
                setName (pack $ "htn_control_" ++ (unpack $ getName m) ++ "_" ++ show n) $
                setParameters (params ++ [htnIdP 1, htnIdP 2]) $
                setPrecondition precond $
                setEffect effect $
                template
        ensurePred (controlP (getName m) n params (htnIdP 1))
        addAction action
        return cpred
    remainingParams :: [(Int, Expr (Atomic TermExpr))] -> [TypedVarExpr]
    remainingParams tl =
        let fvars = nub $ concatMap (findFreeVars . snd) tl in
        filter (not . null . flip intersect fvars . findFreeVars) $
        getParameters m
