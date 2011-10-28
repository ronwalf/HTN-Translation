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
  ScopedTypeVariables
  #-}
module HTNTranslation.Translation
where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Text.Printf

import Planning.Records
import Planning.Expressions
import Planning.Util
import HTNTranslation.HTNPDDL
import HTNTranslation.Typing

-- Types
htnIdT :: (Const :<: f) => Expr f
htnIdT = eConst "HTN_ID"
htnIdV :: (Var :<: f) => Int -> Expr f
htnIdV n = eVar $ "htn_id" ++ show n
htnIdP :: Int -> TypedVarExpr
htnIdP n = eTyped (htnIdV n :: Expr Var) htnIdT
htnIdC :: (Const :<: f) => Int -> Expr f
htnIdC n = eConst $ "htn_id" ++ show n

-- Predicates
startingP :: (AtomicExpression t f) => t -> Expr f
startingP _ = eAtomic "htn_starting" []
taskP :: (AtomicExpression t f) => String -> [t] -> Maybe t -> Expr f
taskP name terms tid =
    eAtomic ("start_" ++ name) (terms ++ maybeToList tid)
controlP :: (AtomicExpression t f) => String -> Int -> [t] -> Maybe t -> Expr f
controlP name n terms mid =
    eAtomic ("constrained_" ++ name ++ "_" ++ show n) $ terms ++ maybeToList mid

releaseP :: (AtomicExpression t f) => String -> Int -> Maybe t -> Int -> Maybe t -> Expr f
releaseP name n1 mid n2 tid =
    eAtomic (printf "release_%s_%d_%d" name n1 n2) $ catMaybes [mid, tid]

topIdP :: forall a f. (AtomicExpression a f) => a -> Expr f
topIdP topId = eAtomic "htn_top_id" [topId]
nextIdP :: forall a f. (AtomicExpression a f) => a -> a -> Expr f
nextIdP id1 id2 = eAtomic "htn_next_id" [id1, id2]
runningIdP :: forall a f. (AtomicExpression a f) => a -> Expr f
runningIdP runId = eAtomic "htn_running_id" [runId]

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
    , tdTypeMap :: TypeMap
    , tdTaskIdUse :: TaskIdUse
    }

{-
class 
    {-(
    HasName sdom, HasName ddom,
    HasRequirements sdom, HasRequirements ddom,
    HasTypes t sdom, HasTypes t ddom,
    HasConstants c sdom, HasConstants t ddom,
    HasPredicates (Expr (Atomic TypedVarExpr)) sdom, HasPredicates (Expr (Atomic TypedVarExpr)) ddom, 
    HasFunctions f sdom, HasFunctions f ddom,
    HasConstraints d sdom, HasConstraints d ddom
    ) => -}
    TranslationState m sdom ddom template where
    getDDomain :: m ddom
    getSDomain :: m sdom
    getTemplate :: m template
    getTypeMap :: m TypeMap
    getIdUse :: m TaskIdUse
-}

{-
instance (MonadState (ddom, TranslationData sdom template) m, HasActions template ddom) 
    => TranslationState m sdom ddom template where
-}

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
getTypeMap :: (MonadState (a, TranslationData b c) m) => m TypeMap
getTypeMap = liftM (tdTypeMap . snd) get
getIdUse :: (MonadState (a, TranslationData b c) m) => m TaskIdUse
getIdUse = liftM (tdTaskIdUse . snd) get

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

-- Create task start/control predicates
translateTaskP ::
    ( HasPredicates (Expr (Atomic TypedVarExpr)) dom
    , HasTypes TypedConstExpr dom)
    => TaskIdUse -> [Expr (Atomic TypedVarExpr)] -> dom -> dom
translateTaskP useId tasks domain =
    let
        useIds = or $ map (useId . taskName) tasks
        htnIdPs =
            [ topIdP $ htnIdP 1
            , nextIdP (htnIdP 1) (htnIdP 2)
            , runningIdP $ htnIdP 1
            ]
        preds = getPredicates domain
            ++ [startingP (undefined :: TypedVarExpr)]
            ++ (if useIds then htnIdPs else [])
            -- ++ map transTask tasks
        types = getTypes domain ++
            if useIds then [htnIdT] else []
    in
    setTypes types $
    setPredicates preds $
    domain

---------------------
-- Translate problem
---------------------
translateProblem :: forall template problem g c f .
    (HasName template, HasName problem,
    HasDomainName template, HasDomainName problem,
    HasRequirements template, HasRequirements problem,
    HasConstants TypedConstExpr template, HasConstants TypedConstExpr problem,
    HasGoal (Expr g) template, HasGoal (Expr g) problem, 
    PDDLAtom :<: g, And :<: g, Not :<: g,
    HasConstraints c template, HasConstraints c problem,
    HasTaskHead (Maybe (Expr (Atomic ConstTermExpr))) problem,
    HasInitial (Expr f) template, HasInitial (Expr f) problem,
    Atomic ConstTermExpr :<: f)
    => template -> TaskIdUse -> Int -> problem -> template
translateProblem template useId numIds problem =
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
    -- BUGBUG This only works if the initial task uses an htn id (or we use a goal)
    goal :: Expr g
    goal = fromMaybe 
        htnStopped $
        getGoal problem
    htnStopped :: Expr g
    htnStopped = eAnd $
        (eNot $ startingP (undefined :: TermExpr))
        : (if (numIds > 0 || maybe False (useId . taskName) (getTaskHead problem))
            then [eNot $ runningIdP (htnIdC 1 :: TermExpr)]
            else [error "Using goal to enforce termination of an initial task that doesn't use HTN IDs not yet implemented.  Use a more basic optimization"])
    idInits :: Maybe (Expr (Atomic ConstTermExpr)) -> template -> template
    idInits Nothing p =
        setConstants (getConstants p ++ constants numIds) $
        setInitial (getInitial p ++ idList [1..numIds]) $
        p
    idInits (Just t) p
        | useId (taskName t) =
            setConstants (getConstants p ++ constants (max 1 numIds)) $
            setInitial (getInitial p 
                ++ [startingP (undefined :: ConstTermExpr)
                   , taskP (taskName t) (taskArgs t) (Just $ htnIdC 1) 
                   , runningIdP (htnIdC 1)]
                ++ idList [2..numIds]) $
            p
        | otherwise =
            setConstants (getConstants p ++ constants numIds) $
            setInitial (getInitial p 
                ++ [startingP (undefined :: ConstTermExpr)
                   , taskP (taskName t) (taskArgs t) Nothing]
                ++ idList [1..numIds]) $
            p
    constants :: Int -> [TypedConstExpr]
    constants n = [eTyped (htnIdC c :: Expr Const) htnIdT | c <- [1..n]]
    idList :: [Int] -> [Expr f]
    idList [] = []
    idList nl@(h:_) =
        topIdP (htnIdC h) :
        [ nextIdP (htnIdC c1) (htnIdC c2) | c1 <- nl | c2 <- tail nl ]


---------------------
-- Translate domain
---------------------

translateDomain :: (HasName a, HasName b,
    HasRequirements a, HasRequirements b,
    HasTypes (TypedConstExpr) a, HasTypes (TypedConstExpr) b,
    HasConstants c a, HasConstants c b,
    HasPredicates (Expr (Atomic TypedVarExpr)) a, HasPredicates (Expr (Atomic TypedVarExpr)) b,
    HasTaskHead [StdTaskDef] a,
    HasFunctions f a, HasFunctions f b,
    HasConstraints d a, HasConstraints d b,
    HasActions action a, HasActions template b) =>
    b -> template -> a -> TypeMap -> TaskIdUse -> [action -> StateT (b, TranslationData a template) Maybe ()] -> b
translateDomain domTemplate actionTemplate dom typeMap useId transl =
    let
        tasks = getTaskHead dom
        copy = 
            translateTaskP useId tasks $
            copyDomainInfo domTemplate dom
        tstate = (copy, TranslationData dom actionTemplate typeMap useId)
        translated =
            fst $
            fromJust $
            flip execStateT tstate$
            mapM_ (\a -> msum $ map (\trans -> trans a) transl) $
            getActions dom
    in
    translated
    
    
------------------
-- Task ID use
------------------
type TaskIdUseFunc m = TypeMap -> String -> m Bool
type TaskIdUse = String -> Bool
taskIdUse :: [TaskIdUseFunc Maybe] -> TypeMap -> String -> Bool
taskIdUse tfuncs tm =
    fromMaybe True .
    msum .
    flip map tfuncs .
    (\task tfunc -> tfunc tm task)


useAtomicId :: (MonadPlus m) => TaskIdUseFunc m
useAtomicId tm =
    flip (>>) (return False) .
    guard . not .
    (\t -> t `extendsType` nonPrimitive || t `extendsType` isLast) .
    flip (Map.findWithDefault baseType) tm

usePLastId :: (MonadPlus m) => TaskIdUseFunc m
usePLastId tm =
    flip (>>) (return False) .
    guard . not .
    --(\t -> t `extendsType` pNotLast || callCounts t == 0) .
    flip extendsType pNotLast .
    flip (Map.findWithDefault baseType) tm

   
-------------------
-- Collapsable tests
-------------------
{-
canHCMethod :: (
    --MonadState (dom, TranslationData sdom template) m
    --, HasActions action sdom
    HasPrecondition pre action
    , HasEffect eff action
    , HasTaskLists action
    , HasTaskConstraints action
    ) => action -> Bool
canHCMethod m =
    isNothing (getPrecondition m)
    && isNothing (getEffect m)
    && isJust (findFirstTask m)

canHCTask :: forall m dom sdom template action pre eff t .
    ( MonadState (dom, TranslationData sdom template) m
    , HasActions action sdom
    , HasName action
    , HasPrecondition pre action
    , HasEffect eff action
    , HasTaskLists action
    , HasTaskConstraints action
    ) => Expr (Atomic t) -> m Bool
canHCTask task = do
    tm <- getTypeMap
    sdom <- getSDomain
    let ttype = flip (Map.findWithDefault baseType) tm $ taskName task
    let methods = catMaybes $ map (findMethod (getActions sdom)) $ callSpots ttype
    return $ or $ map canHCMethod methods
    where
    findMethod :: [action] -> (String, Int) -> Maybe action
    findMethod actions (mname, n) = 
        (find ((== mname) . getName) actions) >>= \m ->
        if (liftM fst (findFirstTask m) == Just n) then Just m else Nothing
-}
canCollapse :: (MonadState (dom, TranslationData sdom template) m
    , HasActions action sdom
    , HasName action
    , HasPrecondition pre action
    , HasEffect eff action
    , HasTaskLists action
    , HasTaskConstraints action
    ) => Expr (Atomic t) -> m Bool
canCollapse task = do
    --canHC <- canHCTask task
    tm <- getTypeMap
    let ttype = flip (Map.findWithDefault baseType) tm $ taskName task
    return $
        True -- not canHC
        && not (ttype `extendsType` nonPrimitive)
        && callCounts ttype == 1

       
            

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
    

translateUncontrolled :: forall m dom sdom vt template action param pre eff.
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic (Expr vt))) dom,
     Typed (Expr Var) :<: vt,
     HasActions template dom,
     HasName action, HasName template,
     HasParameters param action, HasParameters param template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic TermExpr :<: pre, AtomicExpression TermExpr pre, Not :<: pre,
     And :<:  pre, Conjuncts pre pre,
     HasEffect eff action, HasEffect eff template,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action)
    => action -> m ()
translateUncontrolled m = do
    guard $ isNothing $ getTaskHead m 
    guard $ null $ getTaskLists m 
    let precond = conjunct $
            (eNot $ startingP (undefined :: TermExpr)) :
            (maybe [] conjuncts $ getPrecondition m)
    template <- getTemplate
    let action =
            setName (getName m) $
            setParameters (getParameters m) $
            setPrecondition (Just $ precond) $
            setEffect (getEffect m) $
            template
    addAction action
    return ()



translateAction :: forall m dom sdom template action pre eff.
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic TermExpr :<: pre, AtomicExpression TermExpr pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre,
     HasEffect (Expr eff) action, HasEffect (Expr eff) template,
     Atomic TermExpr:<: eff, AtomicExpression TermExpr eff, Not :<: eff,
     And :<: eff, Conjuncts eff eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom
     )
    => action -> m ()

translateAction m = do
    guard $ isJust $ getTaskHead m
    guard $ null $ getTaskLists m
    useId <- getIdUse
    template <- getTemplate
    sdom <- getSDomain
    let task = fromJust $ getTaskHead m 
    let hId = "hId"
    let mId = if (useId $ taskName task) then Just (eVar hId) else Nothing
    let pId = mId >> return (eTyped (eVar hId :: Expr Var) htnIdT)
    let params = getParameters m
            ++ if (useId $ taskName task) then [eTyped (eVar hId :: Expr Var) htnIdT] else []
    let precond = conjunct $
            (startingP (undefined :: TermExpr)) :
            taskP (taskName task) (taskArgs task) mId :
            (maybe [] conjuncts $ getPrecondition m)
    let effect = conjunct $
            eNot (startingP (undefined :: TermExpr)) :
            eNot (taskP (taskName task) (taskArgs task) mId) :
            (if (useId $ taskName task) then (eNot (runningIdP $ eVar hId) :) else id)
            (maybe [] conjuncts $ getEffect m)
    let action = 
            setName (getName m) $
            setParameters params $
            setPrecondition (Just precond) $
            setEffect (Just effect) $
            template
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) pId)
    addAction action
    return ()


translateCollapsed:: forall m dom sdom template action pre eff .
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasActions template dom,
     HasActions action sdom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic TermExpr :<: pre, AtomicExpression TermExpr pre, Not :<: pre,
     And :<:  pre, Conjuncts pre pre,
     HasEffect (Expr eff) action, HasEffect (Expr eff) template,
     Conjuncts eff eff, And :<: eff, Not :<: eff, Atomic TermExpr :<: eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action, HasTaskConstraints action)
    => action -> m ()
translateCollapsed m = do
    guard $ isJust $ getTaskHead m
    let task = fromJust $ getTaskHead m
    typeMap <- getTypeMap
    canCollapse task >>= guard
    template <- getTemplate
    let [(mname, n)] = callSpots $ Map.findWithDefault baseType (taskName task) typeMap
    sdom <- getSDomain
    let method = fromJust $ find ((== mname) . getName) $ getActions sdom  
    useId <- getIdUse
    let (mid, Nothing, action) =
            constrainAction useId method n $
            setName (getName m) $
            setParameters (getParameters m) $
            setPrecondition (getPrecondition m) $
            setEffect (getEffect m :: Maybe (Expr eff)) $
            template
    let precond = conjunct $
            controlP (getName method) n (taskArgs task) mid
            : (eNot $ startingP (undefined :: TermExpr))
            : (maybe [] conjuncts $ getPrecondition action)
    let effect = conjunct $
            eNot (controlP (getName method) n (taskArgs task) mid)
            : (maybe [] conjuncts $ getEffect action)
    addAction $
        setPrecondition (Just precond) $
        setEffect (Just effect) $
        action


lastN :: (HasTaskLists m, HasTaskConstraints m) => m -> Int
lastN m =
    let maxN = maximum $ map fst $ enumerateTasks m in
    maybe (maxN + 1) (\(n,_) -> n) $
    findLastTask m

hangingId :: (HasTaskLists m, HasTaskConstraints m) => TaskIdUse -> m -> Int -> Bool
hangingId useId m n =
    flip (maybe False) (lookup n $ enumerateTasks m) $ \t ->
    useId (taskName t)
    && n /= lastN m
    && length (findNextTasks m n) /= 1


prevTaskNs :: (HasTaskLists m, HasTaskConstraints m) => TaskIdUse -> m -> Int -> [(Int, Bool)]
prevTaskNs useId m n =
    let
        tasks = enumerateTasks m
        lastTasks :: [(Int, Expr PDDLAtom)]
        lastTasks = filter ((/= lastN m) . fst) $ findLastTasks m -- Non-empty if >1 last task
        hangingIds :: [(Int, Expr PDDLAtom)]
        hangingIds = filter (hangingId useId m . fst) tasks
    in
    nub $
    map (\(n', t') -> (n', useId $ taskName t')) $
    (++) (findPrevTasks m n) $
    if (n == lastN m) then hangingIds ++ lastTasks else []

nextTaskNs :: (HasTaskLists m, HasTaskConstraints m) => TaskIdUse -> m -> Int -> [Int]
nextTaskNs useId m n =
    let
        nextTasks = findNextTasks m n
        ln = lastN m
    in
    if (n == ln) then []
    else if null nextTasks then [ln]
    else
    nub $ 
    (if (hangingId useId m n) then (ln :) else id) $
    map fst nextTasks

{-
  Use cases
    - Normal control action translation
    - Inlining  
    - last action
  Task constraint aspects:
    - Method id/no
    - Task id/no
    - incoming constraints with ids
      - extra constraints for last task

  Return:
    Maybe task-id of task to call
-}
constrainAction :: forall m a pre eff.
    (HasName m, HasTaskLists m, HasTaskConstraints m,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) m,
     HasName a,
     HasParameters TypedVarExpr a,
     HasPrecondition (Expr pre) a,
     HasEffect (Expr eff) a,
     Atomic TermExpr :<: pre, Atomic TermExpr :<: eff,
     Conjuncts pre pre, Conjuncts eff eff,
     And :<: pre, And :<: eff,
     Not :<: pre, Not :<: eff)
    => TaskIdUse -> m -> Int -> a -> (Maybe TermExpr, Maybe TermExpr, a)
constrainAction useId m n a =
    let
        name = getName m
        useMId = maybe False (useId . taskName) $ getTaskHead m 
        useTId = maybe False (useId . taskName) $ lookup n $ enumerateTasks m
        mid :: forall f . (Var :<: f) => Maybe (Expr f)
        mid = if useMId then Just (eVar "htnIdM") else Nothing
        prevts :: forall f . (Var :<: f) => [(Int, Maybe (Expr f))]
        prevts = map (\(n', u') -> if u' then (n', Just (htnIdV n')) else (n', Nothing)) $
            prevTaskNs useId m n
        incomingIds :: forall f . (Var :<: f) => [Expr f]
        incomingIds = mapMaybe snd prevts
        tid_returnIds :: forall f . (Var :<: f) => (Maybe (Expr f), [Expr f])
        tid_returnIds = fixIds mid useTId prevts
        returnIds :: forall f . (Var :<: f) => [Expr f]
        returnIds = snd tid_returnIds
        -- If we reused an incoming id, we don't need to allocate one for the task
        nextid :: forall f . (Var :<: f) => Maybe (Expr f)
        nextid = if (not useTId || (fst tid_returnIds :: Maybe (Expr Var)) `elem` mid : map Just incomingIds) then Nothing else Just (eVar "htnNextId")
        -- If nextid is set, we're allocating tid (so make it top)
        -- If we only have on incoming id, no id management is needed
        topid :: forall f . (Var :<: f) => Maybe (Expr f)
        topid = if isNothing (nextid :: Maybe (Expr Var)) && null (returnIds :: [Expr Var])
                then Nothing
                else Just (eVar "htnTopId")
        -- 
        tid :: forall f . (Var :<: f) => Maybe (Expr f)
        tid = if useTId && isJust (nextid :: Maybe (Expr Var)) then topid else fst tid_returnIds
        nextts = nextTaskNs useId m n
        params = nub $
            (++) (getParameters a) $
            map (flip eTyped htnIdT :: Expr Var -> TypedVarExpr) $
            catMaybes [topid, nextid, mid, tid] ++ incomingIds
        precond = conjunct $
            [releaseP name n' mid n pid | (n', pid) <- prevts]
            ++ [eNot $ runningIdP pid | pid <- incomingIds]
            ++ maybeToList (liftM topIdP (topid :: Maybe TermExpr))
            ++ maybeToList (liftM nextIdP (topid :: Maybe TermExpr) `ap` nextid)
            ++ maybe [] conjuncts (getPrecondition a)
        effects = conjunct $ 
            [eNot $ releaseP name n' mid n t' | (n', t') <- prevts]
            ++ idEffects topid nextid returnIds
            ++ [releaseP name n mid n' tid | n' <- nextts]
            ++ maybe [] conjuncts (getEffect a)
    in
    (mid, tid,
    setParameters params $
    setPrecondition (Just precond) $
    setEffect (Just effects) $
    a)
    where
    fixIds :: forall f . (Var :<: f) => Maybe (Expr f) -> Bool -> [(Int, Maybe (Expr f))] -> (Maybe (Expr f), [Expr f])
    fixIds mid useTId prevts
        | n == lastN m = (mid, mapMaybe snd prevts)
        | otherwise = fixIds' useTId $ mapMaybe snd $ filter (not . hangingId useId m . fst) prevts
    fixIds' :: forall f . (Var :<: f) => Bool -> [Expr f] -> (Maybe (Expr f), [Expr f])
    fixIds' False prevts = (Nothing, prevts)
    fixIds' True [] = (Just $ eVar "htnIdT", [])
    fixIds' True (h : tl) = (Just h, tl)
    idEffects :: forall f . (AtomicExpression TermExpr f, Not :<: f) =>
        Maybe (TermExpr) -> Maybe (TermExpr) -> [TermExpr] -> [Expr f]
    -- Allocating.  returns should be null.
    idEffects (Just topid) (Just nextid) [] =
        [eNot $ topIdP topid, topIdP nextid, eNot $ nextIdP topid nextid]
    -- Allocating (nextid), but the return list wasn't empty
    idEffects _ (Just _) _ = error $ "Should have no return ids when allocating. Method: " ++ getName m
    -- Nothing returned, nothing to allocate
    idEffects Nothing Nothing [] = []
    idEffects Nothing Nothing _ = error $ "No topid!  Can't return or allocate ids. Method: " ++ getName m
    -- Return a list of ids
    idEffects (Just topid) Nothing tl =
        eNot (topIdP topid) 
        : topIdP (last tl)
        : [nextIdP id1 id2 | id2 <- topid : tl | id1 <- tl]


-- Standard task translation
translateTask :: forall m dom sdom template action pre eff .
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic TermExpr :<: pre, AtomicExpression TermExpr pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre,
     HasEffect (Expr eff) action, HasEffect (Expr eff) template,
     Atomic TermExpr :<: eff, AtomicExpression TermExpr eff, Not :<: eff,
     And :<: eff, Conjuncts eff eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action, HasTaskConstraints action,
     HasActions template dom)
    => action -> Int -> StdTaskDef -> m ()

translateTask m n task = do
    useId <- getIdUse
    template <- getTemplate
    let
        name = getName m ++ "_" ++ show n
        (mid, tid, a) = constrainAction useId m n $
            setName name $
            setParameters (taskArgs task) $
            template
        precond :: (Expr pre)
        precond = conjunct $
                controlP (getName m) n (map removeType (taskArgs task)) mid
                : eNot (startingP (undefined :: TermExpr))
                : maybe [] conjuncts (getPrecondition a)
        effect :: (Expr eff)
        effect = conjunct $
                (eNot $ controlP (getName m) n (map removeType (taskArgs task)) mid)
                : (startingP (undefined :: TermExpr))
                : taskP (taskName task) (map removeType (taskArgs task)) tid
                : (if tid == mid then [] else maybeToList (liftM runningIdP $ tid))
                ++ maybe [] conjuncts (getEffect a)
    addAction $
        setPrecondition (Just precond) $
        setEffect (Just effect) $
        a

ignoreCollapsedTasks :: (MonadState (dom, TranslationData sdom template) m
    , MonadPlus m
    , HasActions action sdom
    , HasName action
    , HasPrecondition pre action
    , HasEffect eff action
    , HasTaskLists action
    , HasTaskConstraints action
    ) => action -> Int -> StdTaskDef -> m () 
ignoreCollapsedTasks _ _ task = do
    canCollapse task >>= guard
    return ()

translateMethod :: forall m dom sdom template action pre eff .
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic TermExpr :<: pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre,
     HasEffect (Expr eff) action, HasEffect (Expr eff) template,
     Atomic TermExpr :<: eff, Not :<: eff,
     And :<: eff, Conjuncts eff eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action, HasTaskConstraints action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom)
    => [action -> Int -> StdTaskDef -> m ()] 
    -> action -> m ()
translateMethod taskTransl m = do
    flip (maybe mzero) (getTaskHead m) $ \task -> do
    guard $ not $ null $ getTaskLists m
    useId <- getIdUse
    template <- getTemplate
    let useMId = useId $ taskName task
    let
        hId :: forall e . (Var :<: e) => Maybe (Expr e)
        hId = if useMId then Just (eVar "hId") else Nothing
    let pId = hId >>= \v -> return $ eTyped (v :: Expr Var) htnIdT
    let params = getParameters m
            ++ maybeToList (liftM (flip eTyped htnIdT) (hId :: Maybe (Expr Var)))
    let tasks = enumerateTasks m
    let taskPs = [ controlP (getName m) n (taskArgs t) hId
            | (n, t) <- enumerateTasks m ]
    sdom <- getSDomain
    let controlPreds = [ controlP (getName m) n
            (taskArgs $ taskDef sdom t)
            (liftM (flip eTyped htnIdT) hId)
            | (n, t) <- tasks]
    let precond = conjunct $
            (startingP (undefined :: TermExpr)) :
            taskP (taskName task) (taskArgs task) hId :
            (maybe [] conjuncts $ getPrecondition m)
    let effect = conjunct $
            eNot (startingP (undefined :: TermExpr)) :
            eNot (taskP (taskName task) (taskArgs task) hId) :
            taskPs ++
            (maybe [] conjuncts $ getEffect m)
    let action = 
            setName (getName m) $
            setParameters params $
            setPrecondition (Just precond) $
            setEffect (Just effect) $
            template
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) pId)
    addPreds (
       controlPreds 
        ++ [ releaseP (getName m) 
            n1 (if useMId then Just (htnIdP 0) else Nothing)
            n2 (if (useId $ taskName t) then Just (htnIdP 1) else Nothing)
           | (n1, t) <- tasks, n2 <- nextTaskNs useId m n1])
    addAction action
    mapM_ (\(n, t) -> msum $ flip map taskTransl $ \trans ->
        trans m n $ taskDef sdom t) tasks
    when (useMId && isNothing (findLastTask m)) $ do
        let n = lastN m
        let (mid, _, a) = constrainAction useId m n $
                setName (getName m ++ '_' : show n) template
        let effect' = conjunct $
                eNot (runningIdP $ fromJust mid) 
                : maybe [] conjuncts (getEffect a)
        addAction $ setEffect (Just effect') a
    return ()

{-
translateHCMethod :: forall m dom sdom template action pre eff .
    (MonadState (dom, TranslationData sdom template) m, MonadPlus m,
     HasPredicates (Expr (Atomic TypedVarExpr)) dom,
     HasName action, HasName template,
     HasParameters TypedVarExpr action, HasParameters TypedVarExpr template,
     HasPrecondition (Expr pre) action, HasPrecondition (Expr pre) template,
     Atomic TermExpr :<: pre, Not :<: pre,
     And :<: pre, Conjuncts pre pre,
     HasEffect (Expr eff) action, HasEffect (Expr eff) template,
     Atomic TermExpr :<: eff, Not :<: eff,
     And :<: eff, Conjuncts eff eff,
     HasTaskHead (Maybe (Expr (Atomic TermExpr))) action,
     HasTaskLists action, HasTaskConstraints action,
     HasActions template dom,
     HasTaskHead [StdTaskDef] sdom)
    => [action -> Int -> StdTaskDef -> m ()] 
    -> action -> m ()
translateHCMethod taskTransl m = do
    flip (maybe mzero) (getTaskHead m) $ \task -> do
    guard $ isNothing $ getPrecondition m
    guard $ isNothing $ getEffect m
    guard $ not $ null $ getTaskLists m
    flip (maybe mzero) (findFirstTask m) $ \(fn, ft) -> do
    typemap <- getTypeMap
    guard $ canHCMethod m
    useId <- getIdUse
    template <- getTemplate
    let (mid, tid, a) = constrainAction useId m fn $
            setName (getName m) $
            setParameters (getParameters m) $
            setPrecondition (getPrecondition m) $
            setEffect (getEffect m) $
            template
    let midP :: Maybe TypedVarExpr = mid >> return (eTyped (eVar "htnId" :: Expr Var) htnIdT)
    let tasks = enumerateTasks m
    let taskPs = [ controlP (getName m) n (taskArgs t) mid
            | (n, t) <- enumerateTasks m, n /= fn ]
    sdom <- getSDomain
    let controlPreds = [ controlP (getName m) n
            (taskArgs $ taskDef sdom t)
            midP --(liftM (flip eTyped htnIdT) mid)
            | (n, t) <- tasks, n /= fn]
    let precond = conjunct $
            (startingP (undefined :: TermExpr)) :
            taskP (taskName task) (taskArgs task) mid :
            (maybe [] conjuncts $ getPrecondition a)
    let effect = conjunct $
            eNot (taskP (taskName task) (taskArgs task) mid) :
            taskP (taskName ft) (taskArgs ft) tid
            : (if tid == mid then [] else maybeToList (liftM runningIdP $ tid))
            ++ taskPs
            ++ (maybe [] conjuncts $ getEffect a)
    let action = 
            setPrecondition (Just precond) $
            setEffect (Just effect) $
            a
    ensurePred (taskP (taskName task) (taskArgs $ taskDef sdom task) midP)
    addPreds (
       controlPreds 
        ++ [ releaseP (getName m) 
            n1 (mid >> return (htnIdP 0))
            n2 (if (useId $ taskName t) then Just (htnIdP 1) else Nothing)
           | (n1, t) <- tasks, (n2, _) <- findNextTasks m n1])
    addAction action
    flip mapM_ (filter ((/= fn) . fst) tasks) $
        (\(n, t) -> msum $ flip map taskTransl $ \trans ->
            trans m n $ taskDef sdom t )
    when (isJust mid && isNothing (findLastTask m)) $ do
        let n = lastN m
        let (mid', _, a') = constrainAction useId m n $
                setName (getName m ++ '_' : show n) template
        let effect' = conjunct $
                eNot (runningIdP $ fromJust mid') 
                : maybe [] conjuncts (getEffect a')
        addAction $ setEffect (Just effect') a'
    return ()
-}
