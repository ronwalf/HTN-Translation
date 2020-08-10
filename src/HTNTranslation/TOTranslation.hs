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
module HTNTranslation.TOTranslation
where

import Debug.Trace

import Control.Monad
import Control.Monad.State
import Data.List
-- import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, append, pack)
-- import Text.Printf

import Planning.Records
import Planning.Expressions
import Planning.Util
import HTNTranslation.HTNPDDL

-- Types and constants
htnIdT :: Text 
htnIdT = "HTN_ID"
htnIdV :: (Var :<: f) => Int -> Expr f
htnIdV n = eVar $ append "htn_id" $ pack $ show n
htnIdP :: Int -> TypedVarExpr
htnIdP n = eTyped (htnIdV n :: Expr Var) [htnIdT]
htnIdC :: (Const :<: f) => Int -> Expr f
htnIdC n = eConst $ append "htn_id" $ pack $ show n

-- Predicates
taskP :: (Atomic t :<: f) => Text -> [t] -> t -> Expr f
taskP name terms tid =
    eAtomic (append "htn_task_" name) (terms ++ [tid])
controlP :: (Atomic t :<: f) => Text -> Int -> [t] -> t -> Expr f
controlP name spot terms tid =
    eAtomic (append (pack $ "htn_control_" ++ show spot ++ "_") name) (terms ++ [tid])
-- Predicate for order of free ID constants
nextIdP :: forall a f. (Atomic a :<: f) => a -> a -> Expr f
nextIdP id1 id2 = eAtomic "htn_next_id" [id1, id2]
-- Predicate for stating id1 is not used
topP :: forall a f. (Atomic a :<: f) => a -> Expr f
topP id1 = eAtomic "htn_level" [id1]

---------------
-- Utilities --
---------------

taskDef :: (HasTaskHead [StdTaskDef] sdom) => sdom -> Expr (Atomic t) -> StdTaskDef
taskDef sdom t =
    case find ((== taskName t) . taskName) (getTaskHead sdom) of
        Just td -> td
        Nothing -> error ("Cannot find definition for task " ++ show (taskName t))

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
domainSetup :: forall dom template d f g .
    ( HasName dom, HasName template
    , HasRequirements dom, HasRequirements template
    , HasTypes TypedTypeExpr dom
    , HasTypes TypedTypeExpr template
    , HasPredicates (Expr (Atomic TypedVarExpr)) dom
    , HasPredicates (Expr (Atomic TypedVarExpr)) template
    , HasConstants TypedConstExpr dom, HasConstants TypedConstExpr template
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
            [ topP (htnIdP 1)
            , nextIdP (htnIdP 1) (htnIdP 2)
            ]
        preds :: [Expr (Atomic TypedVarExpr)]
        preds = getPredicates domain
            ++ htnIdPs
            ++ [taskP (taskName t) (taskArgs t) (htnIdP 1) | t <- getTaskHead domain]
        types = getTypes domain ++ [eTyped htnIdT []]
        requirements = nub $ getRequirements domain ++ ["typing"]
    in
    setName (getName domain) $
    setRequirements (nub requirements) $
    setTypes types $
    setPredicates preds $
    setConstants (getConstants domain) $
    setFunctions (getFunctions domain) $
    setConstraints (getConstraints domain) $
    setDerived (getDerived domain) template

---------------------
-- Translate problem
---------------------
translateProblem :: forall template problem g f .
    (HasName template, HasName problem,
    HasDomainName template, HasDomainName problem,
    HasRequirements template, HasRequirements problem,
    HasConstants TypedConstExpr template, HasConstants TypedConstExpr problem,
    HasGoal (Expr g) template, HasGoal (Expr g) problem,
    PDDLAtom :<: g, And :<: g, Not :<: g, Conjuncts g g,
    HasConstraints EqualityConstraintExpr problem, -- TODO: separate HTN constraints from problem constraints
    HasTaskList TermExpr problem,
    HasTaskOrdering problem,
    HasInitial (Expr f) template, HasInitial (Expr f) problem,
    Atomic ConstTermExpr :<: f)
    => template -> Int -> Expr (Atomic ConstTermExpr) -> problem -> template
translateProblem template numIds initialTask problem =
    trace "translateProblem" $
    setName (getName problem) $
    setDomainName (getDomainName problem) $
    setRequirements (getRequirements problem) $
    setGoal (Just goal) $
    -- setConstraints (getConstraints problem) $
    idInits $
    setConstants (getConstants problem) $
    setInitial (getInitial problem) template
    where
    goal :: Expr g
    goal = maybe htnStopped (\g -> conjunct [htnStopped, g]) $
        getGoal problem
    useableIds :: forall e . (Const :<: e) => [Expr e]
    useableIds = map (htnIdC :: Int -> Expr e) [1 .. numIds]
    allIds :: forall e . (Const :<: e) => [Expr e]
    allIds = (htnIdC 0 :: Expr e) : useableIds
    htnStopped :: Expr g
    htnStopped = topP (htnIdC 0 :: TermExpr)
    idInits :: template -> template
    idInits p =
        setConstants (getConstants p ++ constants (max 1 numIds)) $
        setInitial (getInitial p
            ++ [ taskP (taskName initialTask) (taskArgs initialTask) (head useableIds :: ConstTermExpr),
                 topP (head useableIds :: ConstTermExpr)]
            ++ zipWith nextIdP (allIds :: [ConstTermExpr]) useableIds) p
    constants :: Int -> [TypedConstExpr]
    constants n = [eTyped (htnIdC c :: Expr Const) [htnIdT] | c <- [0..n]]

---------------------
-- Translate domain
---------------------

translateDomain :: forall action template a b d f g m .
    ( MonadPlus m
    , HasName a, HasName b
    , HasRequirements a, HasRequirements b
    , HasTypes (TypedTypeExpr) a, HasTypes (TypedTypeExpr) b
    , HasConstants TypedConstExpr a, HasConstants TypedConstExpr b
    , HasPredicates (Expr (Atomic TypedVarExpr)) a, HasPredicates (Expr (Atomic TypedVarExpr)) b
    , HasTaskHead [StdTaskDef] a
    , HasFunctions f a, HasFunctions f b
    , HasConstraints d a, HasConstraints d b
    , HasActions action a, HasActions template b
    , HasDerived (TypedPredicateExpr, Expr g) a
    , HasDerived (TypedPredicateExpr, Expr g) b
    , Atomic TermExpr :<: g, Not :<: g, And :<: g
    , Exists TypedVarExpr :<: g, ForAll TypedVarExpr :<: g
    ) => b -> template -> a -> [action -> StateT (b, TranslationData a template) m ()] -> m b
translateDomain domTemplate actionTemplate dom transl =
    let
        copy :: b
        copy =
            domainSetup domTemplate dom
        tstate = (copy, TranslationData dom actionTemplate)
    in
    liftM fst $
    flip execStateT tstate$
    mapM_ (\a -> msum $ map (\trans -> trans a) transl) $
    getActions dom

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
     HasPrecondition (Maybe Text, Expr pre) action,
     HasPrecondition (Maybe Text, Expr pre) template,
     HasEffect eff action, HasEffect eff template,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskList TermExpr action)
    => action -> m ()
translateUncontrolled m = do
    guard $ isNothing $ getTaskHead m
    guard $ null $ getTaskList m
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
     HasPrecondition (Maybe Text, Expr pre) action,
     HasPrecondition (Maybe Text, Expr pre) template,
     Atomic TermExpr :<: pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre, ForAll TypedVarExpr :<: pre,
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) action,
     HasEffect ([TypedVarExpr], Maybe GDExpr, [Expr eff]) template,
     Atomic TermExpr:<: eff, Not :<: eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskList TermExpr action,
     HasConstraints EqualityConstraintExpr action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> m ()
translateAction m = do
    guard $ isJust $ getTaskHead m
    guard $ null $ getTaskList m
    template <- getTemplate
    let name = if null $ getEffect m
            then append "htn_" $ getName m -- Probably was an empty method
            else getName m
    let task = fromJust $ getTaskHead m
    let hid = htnIdV 1
    let hidp = htnIdV 2
    let params = getParameters m ++ [htnIdP 1, htnIdP 2]
    let precond =
             (Nothing, taskP (taskName task) (taskArgs task) hid)
             : (Nothing, topP hid)
             : (Nothing, nextIdP hidp hid)
             : map (\e -> (Nothing, liftE e :: Expr pre)) (getConstraints m)
             ++ getPrecondition m
    let effect =
            ([], Nothing,
                [ eNot $ taskP (taskName task) (taskArgs task) hid
                , eNot $ topP hid
                , topP hidp
                ])
            : getEffect m
    let action =
            setName name $
            setParameters params $
            setPrecondition precond $
            setEffect effect template
    addAction action
    return ()


translateMethod1 :: forall m dom sdom template action pre eff.
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
     HasConstraints EqualityConstraintExpr action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> m ()
translateMethod1 m = do
    guard $ isJust $ getTaskHead m
    guard $ 1 == length (enumerateTasks m)
    template <- getTemplate
    sdom <- getSDomain
    let task = fromJust $ getTaskHead m
    let lastTask = snd $ fromJust $ findLastTask m
    let hid = htnIdV 1
    let params = getParameters m ++ [htnIdP 1]
    let precond =
             (Nothing, taskP (taskName task) (taskArgs task) hid)
             : (Nothing, topP hid)
             : map (\e -> (Nothing, liftE e :: Expr pre)) (getConstraints m)
             ++ getPrecondition m
    let effect =
            ([], Nothing,
                [ eNot $ taskP (taskName task) (taskArgs task) hid
                , taskP (taskName lastTask) (taskArgs lastTask) hid ])
            : getEffect m
    let action =
            setName (append "htn_" $ getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect template
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) (htnIdP 1))
    addAction action
    return ()


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
     HasConstraints EqualityConstraintExpr action,
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
    when (isNothing $ findFirstTask m) $ error $ "Method " ++ show (getName m) ++ " has no first task (can't use totally-ordered translation)"
    let firstTask = fromJust $ findFirstTask m
    tasklist <- (:) firstTask <$> mkTaskList firstTask
    let hidl@(hid:_) = [htnIdV i | i <- [1 .. length tasklist]]
    let params = getParameters m ++ [htnIdP i | i <- [1 .. length tasklist]]
    let precond =
            (Nothing, taskP (taskName task) (taskArgs task) hid)
            : (Nothing, topP hid)
            : [(Nothing, nextIdP (htnIdV (i - 1) :: TermExpr) (htnIdV i) )
              | i <- [2 .. length tasklist]]
            ++ map (\e -> (Nothing, liftE e :: Expr pre)) (getConstraints m)
            ++ getPrecondition m
    let effect =
            ([], Nothing,
                [ eNot $ taskP (taskName task) (taskArgs task) hid
                , eNot (topP hid)
                , topP $ last hidl
                ] ++
                [ taskP (taskName t) (taskArgs t) i
                | ((_, t), i) <- zip tasklist $ reverse hidl
                ])
            : getEffect m
    let action =
            setName (append "htn_" $ getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect template
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) (htnIdP 1))
    addAction action
    return ()
    where
    mkTaskList :: (Int, Expr (Atomic TermExpr)) -> m [(Int, Expr (Atomic TermExpr))]
    mkTaskList (n,_) =
        case findNextTasks m n of
            [] -> return []
            [t] -> (t : ) <$> mkTaskList t
            _ -> error $ "Method " ++ show (getName m) ++ " is not totally ordered!"
