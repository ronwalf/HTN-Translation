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
module HTNTranslation.Translation
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

-- Types and constants
htnIdT :: String
htnIdT = "HTN_ID"
htnIdV :: (Var :<: f) => Int -> Expr f
htnIdV n = eVar $ "htn_id" ++ show n
htnIdP :: Int -> TypedVarExpr
htnIdP n = eTyped (htnIdV n :: Expr Var) [htnIdT]
htnIdC :: (Const :<: f) => Int -> Expr f
htnIdC n = eConst $ "htn_id" ++ show n

-- Predicates
taskP :: (AtomicExpression t f) => Int -> String -> [t] -> t -> [Expr f]
taskP arity name terms tid =
    let tarity = arity - 1 in
    [ eAtomic ("htn_task_" ++ name ++ "_" ++ show i) (iterms ++ [tid])
    | i <- [0 .. (length terms) `div` tarity ]
    , iterms <- [take tarity $ drop (tarity * i) terms]
    ]

-- Predicate for order of free ID constants
lessThanP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
lessThanP id1 id2 = eAtomic "htn_less_than" [id1, id2]
-- Predicate for order of free ID constants
nextIdP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
nextIdP id1 id2 = eAtomic "htn_next_id" [id1, id2]
-- Predicate for stating id1 is not used
{-
freeP :: forall a f. (AtomicExpression a f) => a -> Expr f
freeP id1 = eAtomic "htn_is_free" [id1]
-}
-- Predicate for stating id1 doesn't constraint id2
permitsP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
permitsP id1 id2 = eAtomic "htn_permits" [id1, id2]
-- Set of predicates for stating id1 doesn't constrain any id
allPermitP :: forall a f. (AtomicExpression a f) => [a] -> a -> [Expr f]
allPermitP allIds id1 = map (flip permitsP id1) allIds
permitsAllP :: forall a f. (AtomicExpression a f) => [a] -> a -> [Expr f]
permitsAllP allIds id1 = map (permitsP id1) allIds

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
    , HasConstants TypedConstExpr dom, HasConstants TypedConstExpr template
    , HasTaskHead [StdTaskDef] dom
    , HasFunctions f dom, HasFunctions f template
    , HasConstraints d dom, HasConstraints d template
    , HasDerived (TypedPredicateExpr, Expr g) dom
    , HasDerived (TypedPredicateExpr, Expr g) template
    , Atomic TermExpr :<: g, Not :<: g, And :<: g
    , Exists TypedVarExpr :<: g, ForAll TypedVarExpr :<: g
    ) => template -> Int -> Int -> dom -> template
domainSetup template maxArity numIds domain =
    let
        htnIdPs =
            [ nextIdP (htnIdP 1) (htnIdP 2)
            , permitsP (htnIdP 1) (htnIdP 2)
            , lessThanP (htnIdP 1) (htnIdP 2)
            ]
        preds = getPredicates domain 
            ++ htnIdPs
            ++ concat [taskP maxArity (taskName t) (taskArgs t) (htnIdP 1) | t <- getTaskHead domain]
        types = getTypes domain ++ [eTyped htnIdT []]
        requirements = nub $ getRequirements domain ++ ["typing"]
        constants = getConstants domain ++ 
            map (flip eTyped [htnIdT] . (htnIdC :: Int -> Expr Const)) [0 .. numIds + 1]
    in
    setName (getName domain) $
    setRequirements (nub requirements) $
    setTypes types $
    setPredicates preds $
    setConstants (getConstants domain ++ constants) $
    setFunctions (getFunctions domain) $
    setConstraints (getConstraints domain) $
    setDerived (getDerived domain) $
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
    HasTaskLists ConstTermExpr problem,
    HasTaskConstraints problem,
    HasInitial (Expr f) template, HasInitial (Expr f) problem,
    Atomic ConstTermExpr :<: f)
    => template -> Int -> Int -> problem -> template
translateProblem template maxArity numIds problem =
    setName (getName problem) $
    setDomainName (getDomainName problem) $
    setRequirements (getRequirements problem) $
    setGoal (Just goal) $
    setConstraints (getConstraints problem) $
    idInits (enumerateTasks problem) $
    setConstants (getConstants problem) $
    setInitial (getInitial problem) $
    template
    where
    goal :: Expr g
    goal = maybe htnStopped (\g -> conjunct [htnStopped, g]) $ 
        getGoal problem
    allIds :: forall e . (Const :<: e) => [Expr e]
    allIds = map (htnIdC :: Int -> Expr e) [0 .. numIds + 1]
    useableIds :: forall e . (Const :<: e) => [Expr e]
    useableIds = take numIds $ (tail allIds)
    htnStopped :: Expr g
    htnStopped = conjunct $ zipWith nextIdP (allIds :: [TermExpr]) (tail allIds)
    idInits :: [(Int, Expr (Atomic ConstTermExpr))] -> template -> template
    idInits [] p =
        setInitial (getInitial p ++ freeList allIds ++ idOrder ++ 
            [permitsP (c1 :: ConstTermExpr) c2 | c1 <- useableIds, c2 <- useableIds]) p
    idInits tl p =
        let 
            idtl = zip useableIds tl
            constrainedIds = [ (c1, c2) 
                | (c1, t1) <- idtl
                , (c2, t2) <- idtl
                , t2 `elem` findNextTasks problem (fst t1) ]
        in
        setInitial (getInitial p 
            ++ concat [ taskP maxArity (taskName t) (taskArgs t) hid 
               | (hid, (_, t)) <- idtl ]
            -- ++ map freeP (drop (length tl) useableIds :: [ConstTermExpr])
            ++ freeList (head allIds : drop (length tl + 1) allIds)
            ++ [permitsP (c1 :: ConstTermExpr) c2 | c1 <- useableIds, c2 <- useableIds, 
                (c1, c2) `notElem` constrainedIds]
            ++ idOrder) $
        p
    freeList :: [ConstTermExpr] -> [Expr f]
    freeList nl =
        [ nextIdP (c1 :: ConstTermExpr) c2
        | c1 <- nl
        | c2 <- tail nl ]
    idOrder :: [Expr f]
    idOrder =
        [ lessThanP (c1 :: ConstTermExpr) (c2) 
        | cl <- filter (not . null) $ tails allIds,
          c1 <- [head cl],
          c2 <- tail cl ]


---------------------
-- Translate domain
---------------------

translateDomain :: 
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
    ) => b -> template -> a -> Int -> Int
      -> [action -> Int -> Int -> StateT (b, TranslationData a template) m ()] -> m b
translateDomain domTemplate actionTemplate dom maxArity numIds transl =
    let
        copy = domainSetup domTemplate maxArity numIds dom
        tstate = (copy, TranslationData dom actionTemplate)
    in
    liftM fst $
    flip execStateT tstate $
    mapM_ (\a -> msum $ map (\trans -> trans a maxArity numIds) transl) $
    getActions dom

-- |Identify which tasks have successors (and thus need to be tail recursive)
tasksWithSuccessors :: forall action domain .
    ( HasName action
    , HasTaskHead (Maybe (Expr (Atomic TermExpr))) action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> [String]
tasksWithSuccessors domain =
    nub $ sort $ 
    map (taskName . snd) $
    concatMap tsucc $ getActions domain
    where
    tsucc m = flip filter (enumerateTasks m) $ \(n, _) -> not (null $ findNextTasks m n)

-- |Returns true if task has a method without a last task
taskHasLooseEnds :: forall action domain .
    ( HasName action
    , HasTaskHead (Maybe (Expr (Atomic TermExpr))) action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> String -> Bool
taskHasLooseEnds domain task =
    or $ map (isNothing . findLastTask) $
    filter (not . null . enumerateTasks) $
    filter ((== Just task) . liftM taskName . getTaskHead) $
    getActions domain


-- |Inserts a dummy task and returns its task atom
insertDummy :: forall action domain .
    ( HasName action
    , HasTaskHead StdTaskHead action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    , HasTaskHead [StdTaskDef] domain
    ) => action -> domain -> (StdTask, domain)
insertDummy template domain =
    (eAtomic dname ([] :: [TermExpr]), dom')
    where
    dname = "htn_noop"
    dom' = setTaskHead (getTaskHead domain ++ [eAtomic dname ([] :: [TypedVarExpr])]) $
        setActions (getActions domain ++
            [ setName "htn_noop_action" $
            setTaskHead (Just $ eAtomic dname ([] :: [TermExpr])) $
            template ]) $
        domain


ensureLastTask :: forall action domain .
    ( HasName action
    , HasTaskHead (Maybe (Expr (Atomic TermExpr))) action
    , HasTaskLists TermExpr action
    , HasTaskConstraints action
    , HasActions action domain
    ) => domain -> Expr PDDLAtom -> String -> domain
ensureLastTask domain dtask taskname =
    setActions (map ensure $ getActions domain) domain
    where
    ensure m
        | ( Just taskname == liftM taskName (getTaskHead m )
          && not (null $ enumerateTasks m)
          && isNothing (findLastTask m) )
          = addDummy m
        | otherwise = m
    tlast = "htn_last"
    namedTaskLists m = zipWith addName [1..] (getTaskLists m)
    addName :: Int -> TaskList TermExpr -> TaskList TermExpr
    addName _ tl@(Just _, _) = tl
    addName i (Nothing, tl) = (Just $ "htn_" ++ show i, tl)
    addDummy m =
        let named = namedTaskLists m in
        setTaskLists ((Just tlast, [dtask]) : named) $
        setTaskConstraints (getTaskConstraints m 
            ++ zip (map (fromJust . fst) named) (repeat tlast)) $
        m
--------------
-- Translators
--------------

translateDummy ::
    ( MonadState (dom, TranslationData sdom template) m, MonadPlus m
    , HasActions template dom
    , HasName action, HasName template )
    => action -> Int -> m ()
translateDummy m _ = do
    _ <- fail "Shouldn't get here!"
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
     HasTaskLists TermExpr action)
    => action -> Int -> Int -> m ()
translateUncontrolled m _ _ = do
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
     HasTaskLists TermExpr action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> Int -> Int -> m ()
translateAction m maxArity numIds = do
    guard $ isJust $ getTaskHead m
    guard $ null $ getTaskLists m
    template <- getTemplate
    let name = if null $ getEffect m
            then "htn_" ++ getName m -- Probably was an empty method
            else getName m
    let task = fromJust $ getTaskHead m 
    let hid = htnIdV 1
    let hidp = htnIdV 2
    let hidn = htnIdV 3
    let params = getParameters m ++ [htnIdP 1, htnIdP 2, htnIdP 3]
    let allIds = map htnIdC [1..numIds] :: [TermExpr]
    let precond = 
             [ (Nothing, conjunct $ 
                taskP maxArity (taskName task) (taskArgs task) hid
                ++ [ lessThanP hidp hid
                , lessThanP hid hidn
                , nextIdP hidp hidn
                ] 
                ++ allPermitP allIds hid)
             ]
             ++ getPrecondition m
    let effect = 
            [ ([], Nothing, 
                map eNot (taskP maxArity (taskName task) (taskArgs task) hid) ++
                [ eNot $ nextIdP hidp hidn
                , nextIdP hidp hid
                , nextIdP hid hidn
                ] ++ permitsAllP allIds hid)
            ]
            ++ getEffect m
    let action = 
            setName name $
            setParameters params $
            setPrecondition precond $
            setEffect effect $
            template
    addAction action
    return ()


translateMethod1 :: forall m dom sdom template action pre eff.
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
     HasTaskLists TermExpr action,
     HasTaskConstraints action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> Int -> Int -> m ()
translateMethod1 m maxArity numIds = do
    guard $ isJust $ getTaskHead m
    guard $ 1 == length (enumerateTasks m)
    template <- getTemplate
    sdom <- getSDomain
    let task = fromJust $ getTaskHead m 
    let lastTask = snd $ fromJust $ findLastTask m 
    let hid = htnIdV 1
    let params = getParameters m ++ [htnIdP 1]
    let precond = 
             (Nothing, conjunct $ 
                taskP maxArity (taskName task) (taskArgs task) hid
                ++ allPermitP (map htnIdC [1..numIds]) hid)
             : getPrecondition m
    let effect = 
            ([], Nothing, 
                map eNot (taskP maxArity (taskName task) (taskArgs task) hid)
                ++ taskP maxArity (taskName lastTask) (taskArgs lastTask) hid)
            : getEffect m
    let action = 
            setName ("htn_" ++ getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect $
            template
    mapM_ ensurePred (taskP maxArity (taskName task) (taskArgs $ taskDef sdom task) (htnIdP 1))
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
     HasTaskLists TermExpr action,
     HasTaskConstraints action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> Int -> Int -> m ()
translateMethod m maxArity numIds = do
    guard $ isJust $ getTaskHead m
    guard $ not $ null $ getTaskLists m
    template <- getTemplate
    sdom <- getSDomain
    let task = fromJust $ getTaskHead m 
    let lastTask = findLastTask m 
    -- when (isNothing lastTask) $ fail $ "Method " ++ getName m ++ " has no last task (can't use STRIPS translation)"
    let tasks = taskNums lastTask $ reverse $ enumerateTasks m
    let hid = htnIdV 1
    let alloc = 
            (nextIdP (htnIdC 0) (htnIdV 2 :: TermExpr))
            : concat [
                [ nextIdP (htnIdV n :: TermExpr) (htnIdV $ n + 1) 
                ] | n <- [2 .. length tasks]]
    let params = getParameters m ++ map htnIdP [1 .. length tasks + 1]
    let precond = 
             (Nothing, conjunct $ 
                taskP maxArity (taskName task) (taskArgs task) hid
                ++ allPermitP (map htnIdC [1..numIds]) hid
                ++ alloc)
             : getPrecondition m
    let effect = 
            [ ([], Nothing, 
                map eNot (taskP maxArity (taskName task) (taskArgs task) hid)
                ++ concat [taskP maxArity (taskName t) (taskArgs t) (htnIdV i) | (i, _, t) <- tasks]
                ++ take (length tasks) (
                    ( eNot $ nextIdP (htnIdC 0) (htnIdV 2 :: TermExpr) )
                    : [ eNot $ nextIdP (htnIdV n :: TermExpr) (htnIdV $ n + 1) 
                      | n <- [2..]] )
                ++ [nextIdP (htnIdC 0 :: TermExpr) (htnIdV $ 1 + length tasks)]
                ++ concatMap (constrains tasks) tasks
            )]
            ++ getEffect m
    let action = 
            setName ("htn_" ++ getName m) $
            setParameters params $
            setPrecondition precond $
            setEffect effect $
            template
    mapM_ ensurePred (taskP maxArity (taskName task) (taskArgs $ taskDef sdom task) (htnIdP 1))
    addAction action
    return ()
    where
    taskNums :: (Eq a) => Maybe (Int, a) -> [(Int, a)] -> [(Int, Int, a)]
    taskNums Nothing tasks = zipWith (\i (n,t) -> (i, n, t)) [1..] tasks
    taskNums (Just lt) tasks = zipWith (\i (n, t) -> (i, n, t)) [1..] $
        maybeToList (find (== lt) tasks)
        ++ delete lt tasks
    constrains tasks (i, n, _) = 
        [ eNot $ permitsP (htnIdV i :: TermExpr) (htnIdV i')
        | (i', _, _) <- nextTasks ]
        where
        nextTasks = filter (\(_, n', _) -> n' `elem` nextNs) tasks
        nextNs = map fst $ findNextTasks m n


