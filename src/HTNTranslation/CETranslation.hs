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
  ParallelListComp,
  ScopedTypeVariables,
  TypeOperators
  #-}
module HTNTranslation.CETranslation
where

import Control.Monad
import Control.Monad.State
import Data.List
-- import qualified Data.Map as Map
import Data.Maybe
-- import Text.Printf

import Planning.Records
import Planning.Expressions
import Planning.Util
import HTNTranslation.HTNPDDL

-- Types
htnIdT :: String
htnIdT = "HTN_ID"
htnIdV :: (Var :<: f) => Int -> Expr f
htnIdV n = eVar $ "htn_id" ++ show n
htnIdP :: Int -> TypedVarExpr
htnIdP n = eTyped (htnIdV n :: Expr Var) [htnIdT]
htnIdC :: (Const :<: f) => Int -> Expr f
htnIdC n = eConst $ "htn_id" ++ show n

-- Predicates
taskP :: (AtomicExpression t f) => String -> [t] -> t -> Expr f
taskP name terms tid =
    eAtomic ("htn_task_" ++ name) (terms ++ [tid])
constrainsP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
constrainsP id1 id2 = eAtomic "htn_constrains" [id1, id2]

topIdP :: forall a f. (AtomicExpression a f) => a -> Expr f
topIdP topId = eAtomic "htn_top_id" [topId]
nextIdP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
nextIdP id1 id2 = eAtomic "htn_next_id" [id1, id2]
usedIdP :: forall a f. (AtomicExpression a f) => a -> Expr f
usedIdP id1 = eAtomic "htn_used_id" [id1]
usedBlockP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
usedBlockP id1 id2 = eAtomic "htn_used_block" [id1, id2]
firstFreeP :: forall a f. (AtomicExpression a f) => a -> Expr f
firstFreeP id1 = eAtomic "htn_first_free" [id1]
nextFreeP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
nextFreeP id1 id2 = eAtomic "htn_next_free" [id1, id2]
finishedP :: forall a f. (Atomic a :<: f, AtomicExpression a f) => a -> Expr f
finishedP _ = eAtomic "htn_finished" []


---------------
-- Utilities --
---------------

taskDef :: (HasTaskHead [StdTaskDef] sdom) => sdom -> Expr (Atomic t) -> StdTaskDef
taskDef sdom t =
    case find ((== taskName t) . taskName) (getTaskHead sdom) of
        Just td -> td
        Nothing -> error ("Cannot find definition for task " ++ taskName t)

-- Translation state
data TranslationData sdom template = TranslationData
    { tdSDom :: sdom
    , tdTemplate :: template
    }


getDDomain :: (MonadState (ddom, a) m) => m ddom
getDDomain = liftM fst get
putDDomain :: (MonadState (ddom, a) m) => ddom -> m ()
putDDomain dom = do
    (_, td) <- get
    put (dom, td)
getSDomain :: (MonadState (a, TranslationData sdom b) m) => m sdom
getSDomain = liftM (tdSDom . snd) get
getTemplate :: (MonadState (a, TranslationData b template) m) => m template
getTemplate = liftM (tdTemplate . snd) get

-- Add action to state
addAction :: (MonadState (dom, b) m, HasActions a dom) =>
    a -> m ()
addAction a = do
    dom <- getDDomain
    putDDomain $ setActions (a : getActions dom) dom
    return ()
 
addPreds :: (MonadState (dom, td) m, HasPredicates (Expr (Atomic p)) dom) =>
    [Expr (Atomic p)] -> m ()
addPreds p = do
    dom <- getDDomain
    let preds = getPredicates dom
    putDDomain $ setPredicates (preds ++ p) dom

ensurePred :: (MonadState (dom, td) m, HasPredicates (Expr (Atomic p)) dom) =>
    Expr (Atomic p) -> m ()
ensurePred p = do
    dom <- getDDomain
    let preds = getPredicates dom
    when (isNothing $ find ((== taskName p) . taskName) preds) $ do
        putDDomain $ setPredicates (preds ++ [p]) dom

-- Copy basic domain info and create task start/control predicates
domainSetup ::
    ( HasName dom, HasName template
    , HasRequirements dom, HasRequirements template
    , HasTypes TypedTypeExpr dom
    , HasTypes TypedTypeExpr template
    , HasPredicates (Expr (Atomic TypedVarExpr)) dom
    , HasPredicates (Expr (Atomic TypedVarExpr)) template
    , HasConstants c dom, HasConstants c template
    , HasTaskHead [StdTaskDef] dom
    , HasFunctions f dom, HasFunctions f template
    , HasConstraints d dom, HasConstraints d template
    , HasDerived (TypedPredicateExpr, Expr g) dom
    , HasDerived (TypedPredicateExpr, Expr g) template
    , Atomic TermExpr :<: g, Not :<: g, And :<: g
    , Exists TypedVarExpr :<: g, ForAll TypedVarExpr :<: g
    ) => template -> dom -> template 
domainSetup template domain =
    let
        htnIdPs =
            [ topIdP $ htnIdP 1
            , nextIdP (htnIdP 1) (htnIdP 2)
            , usedIdP (htnIdP 1)
            , usedBlockP (htnIdP 1) (htnIdP 2)
            , firstFreeP (htnIdP 1)
            , nextFreeP (htnIdP 1) (htnIdP 2)
            , constrainsP (htnIdP 1) (htnIdP 2)
            , finishedP (htnIdP 1)
            ]
        preds = getPredicates domain 
            ++ htnIdPs
            ++ [taskP (taskName t) (taskArgs t) (htnIdP 1) | t <- getTaskHead domain]
        types = getTypes domain ++ [eTyped htnIdT []]
        [id1, id2, id3] = [htnIdV 1, htnIdV 2, htnIdV 3 :: TermExpr]
        derived = getDerived domain ++
            [ (usedBlockP (htnIdP 1) (htnIdP 2),
                nextIdP id1 id2)
            , (usedBlockP (htnIdP 1) (htnIdP 2),
                eExists [htnIdP 3] $ eAnd
                [ nextIdP id3 id2
                , usedIdP id3
                , usedBlockP id1 id3 ])
            , (nextFreeP (htnIdP 1) (htnIdP 2),
                eAnd $
                [ eNot $ usedIdP id2
                , usedBlockP id1 id2 ])
            , (firstFreeP (htnIdP 1),
                eAnd $
                [ topIdP id1
                , eNot $ usedIdP id1 ])
            , (firstFreeP (htnIdP 1),
                eExists [htnIdP 2] $
                eAnd $
                [ topIdP id2
                , usedIdP id2
                , nextFreeP id2 id1 ])
            , (finishedP (htnIdP 1),
                eForAll [htnIdP 1] $
                eNot $ usedIdP id1)
            ]
    in
    setName (getName domain) $
    setRequirements (getRequirements domain) $
    setTypes types $
    setPredicates preds $
    setConstants (getConstants domain) $
    setFunctions (getFunctions domain) $
    setConstraints (getConstraints domain) $
    setDerived derived $
    template

---------------------
-- Translate problem
---------------------
translateProblem :: forall template problem g c f .
    (HasName template, HasName problem,
    HasDomainName template, HasDomainName problem,
    HasRequirements template, HasRequirements problem,
    HasConstants TypedConstExpr template, HasConstants TypedConstExpr problem,
    HasGoal (Expr g) template, HasGoal (Expr g) problem, 
    PDDLAtom :<: g, And :<: g, Not :<: g, Conjuncts g g,
    HasConstraints c template, HasConstraints c problem,
    HasTaskHead (Maybe (Expr (Atomic ConstTermExpr))) problem,
    HasInitial (Expr f) template, HasInitial (Expr f) problem,
    Atomic ConstTermExpr :<: f)
    => template -> Int -> problem -> template
translateProblem template numIds problem =
    setName (getName problem) $
    setDomainName (getDomainName problem) $
    setRequirements (getRequirements problem) $
    setGoal (Just goal) $
    setConstraints (getConstraints problem) $
    idInits (getTaskHead problem) $
    setConstants (getConstants problem) $
    setInitial (getInitial problem) $
    template
    where
    goal :: Expr g
    goal = maybe htnStopped (\g -> conjunct [htnStopped, g]) $ 
        getGoal problem
    htnStopped :: Expr g
    htnStopped = finishedP (htnIdC 1 :: TermExpr)
    idInits :: Maybe (Expr (Atomic ConstTermExpr)) -> template -> template
    idInits Nothing p =
        setConstants (getConstants p ++ constants numIds) $
        setInitial (getInitial p ++ idList [1..numIds]) $
        p
    idInits (Just t) p =
        setConstants (getConstants p ++ constants (max 1 numIds)) $
        setInitial (getInitial p 
            ++ [ taskP (taskName t) (taskArgs t) (htnIdC 1) 
               , usedIdP (htnIdC 1 :: ConstTermExpr)]
            ++ idList [1..numIds]) $
        p
    constants :: Int -> [TypedConstExpr]
    constants n = [eTyped (htnIdC c :: Expr Const) [htnIdT] | c <- [1..n]]
    idList :: [Int] -> [Expr f]
    idList [] = []
    idList nl@(h:_) =
        topIdP (htnIdC h :: ConstTermExpr) :
        [ nextIdP (htnIdC c1 :: ConstTermExpr) (htnIdC c2) | c1 <- nl | c2 <- tail nl ]


---------------------
-- Translate domain
---------------------

translateDomain :: 
    ( HasName a, HasName b
    , HasRequirements a, HasRequirements b
    , HasTypes (TypedTypeExpr) a, HasTypes (TypedTypeExpr) b
    , HasConstants c a, HasConstants c b
    , HasPredicates (Expr (Atomic TypedVarExpr)) a, HasPredicates (Expr (Atomic TypedVarExpr)) b
    , HasTaskHead [StdTaskDef] a
    , HasFunctions f a, HasFunctions f b
    , HasConstraints d a, HasConstraints d b
    , HasActions action a, HasActions template b
    , HasDerived (TypedPredicateExpr, Expr g) a
    , HasDerived (TypedPredicateExpr, Expr g) b
    , Atomic TermExpr :<: g, Not :<: g, And :<: g
    , Exists TypedVarExpr :<: g, ForAll TypedVarExpr :<: g
    ) => b -> template -> a -> [action -> StateT (b, TranslationData a template) Maybe ()] -> b
translateDomain domTemplate actionTemplate dom transl =
    let
        copy = 
            domainSetup domTemplate dom
        tstate = (copy, TranslationData dom actionTemplate)
        translated =
            fst $
            fromJust $
            flip execStateT tstate$
            mapM_ (\a -> msum $ map (\trans -> trans a) transl) $
            getActions dom
    in
    translated
    
--------------
-- Translators
--------------

translateDummy ::
    ( MonadState (dom, TranslationData sdom template) m, MonadPlus m
    , HasActions template dom
    , HasName action, HasName template )
    => action -> m ()
translateDummy m = do
    template <- getTemplate
    addAction $ setName (getName m) template
    

translateUncontrolled :: forall m dom sdom template action param pre eff.
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasActions template dom,
     HasName action, HasName template,
     HasParameters param action, HasParameters param template,
     HasPrecondition (Maybe String, Expr pre) action, 
     HasPrecondition (Maybe String, Expr pre) template,
     HasEffect eff action, HasEffect eff template,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action)
    => action -> m ()
translateUncontrolled m = do
    guard $ isNothing $ getTaskHead m 
    guard $ null $ getTaskLists m 
    template <- getTemplate
    let action =
            setName (getName m) $
            setParameters (getParameters m) $
            setPrecondition (getPrecondition m) $
            setEffect (getEffect m) $
            template
    addAction action
    return ()



translateAction :: forall m dom sdom template action pre eff.
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Maybe String, Expr pre) action, 
     HasPrecondition (Maybe String, Expr pre) template,
     Atomic TermExpr :<: pre, AtomicExpression TermExpr pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre, ForAll TypedVarExpr :<: pre,
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) action, 
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) template,
     Atomic TermExpr:<: eff, AtomicExpression TermExpr eff, Not :<: eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> m ()
translateAction m = do
    guard $ isJust $ getTaskHead m
    guard $ null $ getTaskLists m
    template <- getTemplate
    let task = fromJust $ getTaskHead m 
    let hid = htnIdV 1
    let params = getParameters m ++ [htnIdP 1]
    let precond = 
             [ (Nothing, taskP (taskName task) (taskArgs task) hid)
             , (Nothing, eForAll [htnIdP 2] $ eNot $ constrainsP (htnIdV 2) hid)
             ]
             ++ getPrecondition m
    let effect = 
            [ ([], Nothing, 
                [ eNot $ taskP (taskName task) (taskArgs task) hid
                , eNot $ usedIdP hid ])
            , ([htnIdP 2], Nothing, [eNot $ constrainsP hid (htnIdV 2)])
            ]
            ++ getEffect m
    let action = 
            setName (getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect $
            template
    addAction action
    return ()

translateMethod :: forall m dom sdom template action pre eff.
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Maybe String, Expr pre) action, 
     HasPrecondition (Maybe String, Expr pre) template,
     Atomic TermExpr :<: pre, AtomicExpression TermExpr pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre, ForAll TypedVarExpr :<: pre,
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) action, 
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) template,
     Atomic TermExpr:<: eff, AtomicExpression TermExpr eff, Not :<: eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action,
     HasTaskConstraints action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> m ()
translateMethod m = do
    guard $ isJust $ getTaskHead m
    guard $ not $ null $ getTaskLists m
    template <- getTemplate
    sdom <- getSDomain
    let task = fromJust $ getTaskHead m 
    let lastTask = findLastTask m 
    let tasks = taskNums lastTask $ enumerateTasks m
    let hid = htnIdV 1
    let alloc = zip (repeat Nothing) $
            take (length tasks - 1) $
            (firstFreeP (htnIdV 2 :: TermExpr))
            : [nextFreeP (htnIdV n :: TermExpr) (htnIdV $ n + 1) | n <- [2..]]
    let params = getParameters m ++ map htnIdP [1 .. length tasks]
    let precond = 
             [ (Nothing, taskP (taskName task) (taskArgs task) hid)
             , (Nothing, eForAll [htnIdP 0] $ eNot $ constrainsP (htnIdV 0) hid)
             ]
             ++ alloc
             ++ getPrecondition m
    let effect = 
            [ ([], Nothing, 
                [ eNot $ taskP (taskName task) (taskArgs task) hid ])]
            ++ [([], Nothing, [taskP (taskName t) (taskArgs t) (htnIdV i)]) | (i, _, t) <- tasks]
            ++ [([], Nothing, [usedIdP (htnIdV i :: TermExpr) | (i, _, _) <- tasks, i /= 1])]
            ++ [([], Nothing, constrains tasks t) | t <- tasks]
            ++ (if (isJust lastTask) then [] else (cleanup tasks))
            ++ getEffect m
    let action = 
            setName (getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect $
            template
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) (htnIdP 1))
    addAction action
    return ()
    where
    taskNums :: (Eq a) => Maybe (Int, a) -> [(Int, a)] -> [(Int, Int, a)]
    taskNums Nothing tasks = zipWith (\i (n,t) -> (i, n, t)) [1..] tasks
    taskNums (Just lt) tasks = zipWith (\i (n, t) -> (i, n, t)) [1..] $
        maybeToList (find (== lt) tasks)
        ++ delete lt tasks
    constrains tasks (i, n, _) = 
        [ constrainsP (htnIdV i :: TermExpr) (htnIdV i')
        | (i', _, _) <- nextTasks ]
        where
        nextTasks = filter (\(_, n', _) -> n' `elem` nextNs) tasks
        nextNs = map fst $ findNextTasks m n
        

    -- Effects needed when there is no last task
    cleanup tasks =
        [( [htnIdP 0]
        , Just $ constrainsP (htnIdV 1 :: TermExpr) (htnIdV 0)
        , (if 1 `elem` (map (\(i,_,_) -> i) ltasks) then id else (:) (eNot $ constrainsP (htnIdV 1 :: TermExpr) (htnIdV 0)))
          [ constrainsP (htnIdV i :: TermExpr) (htnIdV 0) | (i, _, _) <- ltasks ])]
        where
        ltasks = let lnums = map fst (findLastTasks m) in filter (\(_,n,_) -> n `elem` lnums) tasks



