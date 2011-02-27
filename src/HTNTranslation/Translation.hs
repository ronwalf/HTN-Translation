{-# OPTIONS_GHC
    -fcontext-stack=30
    -Wall
  #-}
{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances,
  FunctionalDependencies,
  IncoherentInstances,
  MultiParamTypeClasses,
  ScopedTypeVariables
  #-}
module HTNTranslation.Translation
where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Planning.Records
import Planning.Expressions
import Planning.Util
import HTNTranslation.HTNPDDL
import HTNTranslation.Typing

-- Types
htnIdT :: Expr Const
htnIdT = eConst "HTN_ID"

-- Predicates
startingP :: (AtomicExpression t f) => Expr f
startingP = eAtomic "htn_starting" []
taskP :: (AtomicExpression t f) => String -> [t] -> t -> Expr f
taskP name terms hId =
    eAtomic ("start_" ++ name) (terms ++ [hId]) 

topIdP :: forall a f. (AtomicExpression a f) => a -> Expr f
topIdP topId = eAtomic "htn_top_id" [topId]
nextIdP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
nextIdP id1 id2 = eAtomic "htn_next_id" [id1, id2]
runningIdP :: forall a f. (AtomicExpression a f) => a -> Expr f
runningIdP runId = eAtomic "htn_running_id" [runId]

---------------
-- Utilities --
---------------

-- Add action to state
addAction :: (MonadState dom m, HasActions a dom) =>
    a -> m ()
addAction a = do
    dom <- get
    put $ setActions (a : getActions dom) dom
    return ()
    
-- Ensure predicate exists in domain
ensurePred :: (MonadState dom m, HasPredicates (Expr (Atomic p)) dom) =>
    Expr (Atomic p) -> m ()
ensurePred p = do
    dom <- get
    let preds = getPredicates dom
    let found = find ((== taskName p) . taskName) $ getPredicates dom
    when (isNothing found) $ put $ setPredicates (preds ++ [p]) dom
    return ()

-- Copy basic domain info
copyDomainInfo ::
    (HasName a, HasName b,
    HasRequirements a, HasRequirements b,
    HasTypes t a, HasTypes t b,
    HasConstants c a, HasConstants c b,
    HasPredicates p a, HasPredicates p b,
    HasFunctions f a, HasFunctions f b,
    HasConstraints d a, HasConstraints d b)
    => b -> a -> b
copyDomainInfo template domain =
    setName (getName domain) $
    setRequirements (getRequirements domain) $
    setTypes (getTypes domain) $
    setConstants (getConstants domain) $
    setPredicates (getPredicates domain) $
    setFunctions (getFunctions domain) $
    setConstraints (getConstraints domain) $
    template


---------------------
-- Translate domain
---------------------

translateDomain ::     (HasName a, HasName b,
    HasRequirements a, HasRequirements b,
    HasTypes t a, HasTypes t b,
    HasConstants c a, HasConstants c b,
    HasPredicates p a, HasPredicates p b,
    HasFunctions f a, HasFunctions f b,
    HasConstraints d a, HasConstraints d b,
    HasActions action a) =>
    b -> a -> [action -> StateT b Maybe ()] -> b
translateDomain template dom transl =
    let
        copy = copyDomainInfo template dom
        translated =
            --fromJust $
            --foldM (\dom trans -> execStateT trans dom) copy $
            fromJust $
            flip execStateT copy $
            msum $
            map (\a -> msum $ map (\trans -> trans a) transl) $
            getActions dom
    in
    translated
    
    
------------------
-- Task ID use
------------------
type TaskIdUseFunc = String -> Bool

useTaskId :: TaskIdUseFunc
useTaskId = const True

dropAtomicId :: TypeMap -> TaskIdUseFunc
dropAtomicId tm =
    flip isType (addType primitive notLast) .
    flip (Map.findWithDefault baseType) tm

--------------
-- Translators
--------------


translateUncontrolled :: forall m dom vt template action param pre eff t .
    (MonadState dom m, MonadPlus m,
     HasPredicates (Expr (Atomic (Expr vt))) dom,
     Typed (Expr Var) :<: vt,
     HasActions template dom,
     HasName action, HasName template,
     HasParameters param action, HasParameters param template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic (Expr t) :<: pre, AtomicExpression (Expr t) pre, Not :<: pre,
     And :<:  pre, Conjuncts pre pre,
     HasEffect eff action, HasEffect eff template,
     HasTaskHead (Maybe (Expr (Atomic (Expr t)))) action,
     HasTaskLists action)
    => template -> action -> m ()
translateUncontrolled template m = do
    guard $ isNothing $ getTaskHead m 
    guard $ null $ getTaskLists m 
    let precond = conjunct $
            --(eNot startingP) :
            (maybe [] conjuncts $ getPrecondition m)
    let action =
            setName (getName m) $
            setParameters (getParameters m) $
            setPrecondition (getPrecondition m) $
            setEffect (getEffect m) $
            template
    addAction action
    return ()
    

translateAction :: forall m dom template action param pre eff t .
    (MonadState dom m, MonadPlus m,
     HasName action, HasName template,
     HasParameters (Expr param) action, HasParameters (Expr param) template,
     Var :<: param, Var :<: t,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic (Expr t) :<: pre, AtomicExpression (Expr t) pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre,
     HasEffect (Expr eff) action, HasEffect (Expr eff) template,
     Atomic (Expr t) :<: eff, AtomicExpression (Expr t) eff, Not :<: eff,
     And :<: eff, Conjuncts eff eff,
     HasTaskHead (Maybe (Expr (Atomic (Expr t)))) action,
     HasTaskLists action,
     HasActions template dom)
    => template -> action -> m ()

translateAction template m = do
    guard $ isJust $ getTaskHead m
    guard $ null $ getTaskLists m
    let task = fromJust $ getTaskHead m
    let hId = "hId"
    let precond = conjunct $
            (startingP) :
            taskP (taskName task) (taskArgs task) (eVar hId) :
            (maybe [] conjuncts $ getPrecondition m)
    let effect = conjunct $
            eNot (startingP) :
            eNot (runningIdP $ eVar hId) :
            (maybe [] conjuncts $ getEffect m)
    let action = 
            setName (getName m) $
            setParameters (getParameters m) $
            setPrecondition (getPrecondition m) $
            setEffect (getEffect m) $
            template
    addAction action
    return ()
