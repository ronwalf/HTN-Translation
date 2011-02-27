{-# OPTIONS_GHC
  -fcontext-stack=30
  -Wall
  #-}
{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    ParallelListComp,
    RankNTypes,
    ScopedTypeVariables,
    TypeOperators
  #-}
module HTNTranslation.TranslationOpt (
    CallDesc(..), getCDTask, getCDSVars,
    CallNode, 

    deconstructHDomain,
    renderCalls,
    renderCounters,
    renderInitTask,

    optimizeCallNet,
    CallNetOptimizer,
    paramOpt
) where

--import Data.Generics (Data, Typeable, Typeable1, Typeable2)
--import Data.Maybe
--import Data.Set (Set)
--import qualified Data.Set as Set

import Control.Monad
import Data.List
import Data.Maybe

import HTNTranslation.HTNPDDL
import Planning.Util

{-
data CallTypes t = CallTypes (Set t) deriving (Data, Eq, Typeable)
unCallTypes :: CallTypes t -> Set t
unCallTypes (CallTypes t) = t
class (Data a, Data t) => HasTaskHead t a where
    getCallTypes :: a -> t
    getCallTypes = unCallTypes . fromJust . gfind
    setCallTypes :: Set t -> a -> a
    setCallTypes ts r = fromJust $ greplace r (CallTypes ts)
-}


type CallNode = (CallDesc, PDDLAction, [CallDesc])

data CallDesc =
    StackCall StdTask Int -- Stack call with number of needed stack vars
    deriving Eq

-- Utils
varId :: (:<:) Var f => Expr Var -> Expr f
varId (In (Var v)) = eVar v

varIds :: (:<:) Var f => [Expr Var] -> [Expr f]
varIds vars = map varId vars

addItems :: (HasItems (Expr di) d, DomainItem i :<: di)
    => [i] -> d -> d
addItems il d =
    setItems
    (getItems d ++ map domainItem il)
    d

addParameters :: (HasParameters t a)
    => [t] -> a -> a
addParameters tl a =
    setParameters
    (getParameters a ++ tl)
    a

addConjuncts :: (And :<: f, Conjuncts f f)
    => (a -> Maybe (Expr f)) 
    -> (Maybe (Expr f) -> a -> a) 
    -> [Expr f]
    -> a
    -> a
addConjuncts reader setter cl a =
    setter
    (Just $
    conjunct $
    cl ++
    (maybe [] conjuncts $ reader a))
    a

addPreconditions :: 
    (HasPrecondition (Expr f) a,
    And :<: f, Conjuncts f f)
    => [Expr f] -> a -> a
addPreconditions = addConjuncts getPrecondition setPrecondition

addEffects :: 
    (HasEffect (Expr f) a,
    And :<: f, Conjuncts f f)
    => [Expr f] -> a -> a
addEffects = addConjuncts getEffect setEffect



-- Fixed counter functions
counterType :: forall f. (:<:) Const f => Expr f
counterType = eConst "COUNTER"
counterTop :: (Atomic (Expr f) :<: g) => Expr f -> Expr g
counterTop var = eAtomic "counterTop" [var]
counterDifferent :: (Atomic (Expr f) :<: g) => Expr f -> Expr f -> Expr g
counterDifferent var1 var2 = eAtomic "counterDifferent" [var1, var2]

counters :: (Const :<: g) => String -> Int -> [Expr g]
counters s num =
    [eConst $ "c_" ++ s ++ '_' : show n | n <- [0..num]]

-- Counter relations
counterBegin :: ((:<:) (Atomic (Expr f)) g) => Expr f -> Expr g
counterBegin var = eAtomic "counterBegin" [var]
counterNext :: ((:<:) (Atomic (Expr f)) g) => Expr f -> Expr f -> Expr g
counterNext ovar nvar = eAtomic "counterNext" [ovar, nvar]

-- Add a counter to an action
addParamTs :: (HasParameters (Expr f) a, TypedVar :<: f) => [Expr Var] -> a -> a
addParamTs tl = addParameters (map (flip eTyped counterType) tl)

-- A counter variable for current count.
currentT :: (Var :<: f) => Int -> Expr f
currentT n = eVar $ "currentT" ++ show n

addCurrentTs :: 
    (HasParameters (Expr t) a, TypedVar :<: t, -- addparamT constraints
    HasPrecondition (Expr f) a, Atomic TermExpr :<: f, And :<: f, Conjuncts f f)
    => Int -> a -> ([Expr Var], a)
addCurrentTs n a =
    let vars = map currentT [1..n] in
    (vars, 
    addParamTs vars $
    addPreconditions [ counterTop (varId v  :: TermExpr) | v <- vars ]
    a)

-- A counter variable for the next count.
nextT :: (Var :<: f) => Int -> Expr f
nextT n = eVar $ "nextT" ++ show n

-- Adds nextT* and necessary precondition to action and effects
addNextTs ::
    (HasParameters (Expr t) a, TypedVar :<: t, -- addParamT constraints
    HasPrecondition (Expr f) a, Atomic TermExpr :<: f, And :<: f, Conjuncts f f, -- addPrecond
    HasEffect (Expr g) a, Atomic TermExpr :<: g, And :<: g, Not :<: g, Conjuncts g g) -- addEffect
    => [Expr Var] -> a -> (a, [Expr Var])
addNextTs vars a =
    let vars' = map nextT [1.. length vars] in
    (addParamTs vars' $
    addPreconditions [counterNext (varId ct) (varId nt :: TermExpr)
        | ct <- vars
        | nt <- vars' ] $
    addEffects (concat 
        [[counterTop (varId nt :: TermExpr), eNot $ counterTop (varId ct:: TermExpr)]
        | ct <- vars
        | nt <- vars' ]) $
    a, 
    vars')

-- Adds nextT* reversed and necessary precondition to action and effects
addPrevTs ::
    (HasParameters (Expr t) a, TypedVar :<: t, -- addParamT constraints
    HasPrecondition (Expr f) a, Atomic TermExpr :<: f, And :<: f, Conjuncts f f, -- addPrecond
    HasEffect (Expr g) a, Atomic TermExpr :<: g, And :<: g, Not :<: g, Conjuncts g g) -- addEffect
    => [Expr Var] -> a -> (a, [Expr Var])
addPrevTs vars a =
    let vars' = map nextT [1.. length vars] in
    (addParamTs vars' $
    addPreconditions [counterNext (varId nt) (varId ct :: TermExpr)
        | ct <- vars
        | nt <- vars' ] $
    addEffects (concat 
        [[counterTop (varId nt :: TermExpr), eNot $ counterTop (varId nt:: TermExpr)]
        | nt <- vars' ]) $
    a, 
    vars')

-- Start a new set of stacks
addNewTs ::
    (HasParameters (Expr t) a, TypedVar :<: t, -- addParamT constraints
    HasPrecondition (Expr f) a, Atomic TermExpr :<: f, And :<: f, Conjuncts f f, -- addPrecond
    HasEffect (Expr g) a, Atomic TermExpr :<: g, And :<: g, Not :<: g, Conjuncts g g) -- addEffect
    => Int -> a -> ([Expr Var], a)
addNewTs n a =
    let
        vars :: [Expr Var]
        vars = [ eVar $ "freshT" ++ show i | i <- [1..n]]
        preconds = [ counterBegin (varId v :: TermExpr) | v <- vars ]
                ++ [ counterDifferent (varId v1) (varId v2 :: TermExpr) 
                    | v1 <- vars, v2 <- vars, v1 /= v2 ]
        effs = map (eNot . counterTop . (varId :: Expr Var -> TermExpr) ) vars
    in
    (vars,
        addParamTs vars $
        addPreconditions preconds $
        addEffects effs $
        a)

-- Predicate for a task with a stack variable
stackCall ::
    (Atomic TermExpr :<: f)
    => StdTask
    -> [Expr Var] -- counter vars
    -> Expr f
stackCall (In (Atomic p args)) svars =
    eAtomic ("start_" ++ p) (args ++ varIds svars)

cStackCall ::
    (Atomic ConstTermExpr :<: f)
    => Expr (Atomic ConstTermExpr)
    -> [ConstTermExpr]
    -> Expr f
cStackCall task counts =
    eAtomic ("start_" ++ taskName task) (taskArgs task ++ counts)

-- Predicate for a task without a stack variable
atomicCall ::
    (Atomic TermExpr :<: f)
    => StdTask
    -> Expr f
atomicCall (In (Atomic p args)) =
    eAtomic ("start_" ++ p) args

-- Methods for calling tasks
getCDTask :: CallDesc -> StdTask
getCDTask (StackCall task _) = task

getCDSVars :: CallDesc -> Int
getCDSVars (StackCall _ sn) = sn

callEffect :: (Atomic TermExpr :<: f)
    => CallDesc -> [Expr Var] -> ([Expr Var], Expr f)
callEffect (StackCall stdTask svn) vars =
    let (used, lovers) = splitAt svn vars in
    (lovers, stackCall stdTask used)

-- Add calls
addCalls ::
    (HasParameters (Expr t) a, TypedVar :<: t, -- addParamT constraints
    HasPrecondition (Expr f) a, Atomic TermExpr :<: f, And :<: f, Conjuncts f f, -- addPrecond
    HasEffect (Expr g) a, Atomic TermExpr :<: g, And :<: g, Not :<: g, Conjuncts g g) -- addEffect
    => (CallDesc, a, [CallDesc])
    -> a
-- addCalls FreeCall a [] = a
-- addCalls FreeCall _ _ = error "FreeCall actions with subtasks not yet handled"
addCalls ((StackCall task n), a, []) =
    let 
        (currTs, a') = addCurrentTs n a
        (a'', _) = addPrevTs currTs a'
    in
    addPreconditions [stackCall task currTs] $
    addEffects [eNot $ stackCall task currTs] $
    a''
addCalls ((StackCall task tn), a, [lastt]) =
    let
        (currTs, act) = addCurrentTs tn a
        (newTs, acnt) = addNewTs (max 0 $ getCDSVars lastt - tn) act
    in
    addPreconditions [stackCall task currTs] $
    addEffects [eNot $ stackCall task currTs,
        snd $ callEffect lastt (currTs ++ newTs)] $
    acnt
addCalls ((StackCall task tn), a, cdl) =
    let
        (lastt : cdl') = reverse cdl
        sumcn = sum . map getCDSVars $ cdl'
        (currTs, act) = addCurrentTs tn a
        (newTs, acnt) = addNewTs (max 0 $ sumcn - tn) act
        (acnnt, nextTs) = addNextTs (currTs ++ newTs) acnt
        starts = snd $ mapAccumR (flip callEffect) nextTs cdl'
    in
    addPreconditions [stackCall task currTs] $
    addEffects ([eNot $ stackCall task currTs,
        snd $ callEffect lastt (currTs ++ newTs)]
        ++ starts) $
    acnnt

renderInitTask ::
    (HasInitial (Expr f) p,
     FuncEq f,
     Atomic ConstTermExpr :<: f)
    => Expr (Atomic ConstTermExpr)
    -> [CallNode]
    -> [[ConstTermExpr]]
    -> p
    -> p
renderInitTask task callNodes counts prob =
    let
        callDef =
            listToMaybe $
            filter ((==) (taskName task) . taskName . getCDTask) $
            map (\(cd, _, _) -> cd) callNodes
        sn = maybe 0 getCDSVars callDef
        otops = take sn $ map head counts
        ntops = take sn $ map (head . tail) counts
        inits' = 
            (getInitial prob \\ [counterTop c | c <- otops])
            ++ cStackCall task ntops
            : [counterTop c | c <- ntops]
    in
    setInitial inits' $
    prob

renderCounters ::
    (HasConstants TypedConstExpr p,
     HasInitial (Expr f) p,
     Atomic ConstTermExpr :<: f,
     Const :<: g)
    => [Int]
    -> p
    -> ([[Expr g]], p)
renderCounters countSizes prob =
    let
        countSizes' = filter (>0) countSizes
        counts :: forall h . (Const :<: h) => [[Expr h]]
        counts = map (uncurry counters) $ zip (map show [1 :: Int ..]) countSizes'
        countTops :: forall h. (Const :<: h) => [Expr h]
        countTops = map head counts
        consts' :: [TypedConstExpr]
        consts' = getConstants prob
            ++ (map (flip eTyped counterType) $ 
                (concat counts :: [Expr Const]))
        inits' = getInitial prob
            ++ [counterDifferent (c1 :: ConstTermExpr) c2
                | c1 <- countTops, c2 <- countTops, c1 /= c2]
            ++ concatMap oneCounter counts
    in
    (\p -> (counts, p)) $
    setConstants consts' $
    setInitial inits' $
    prob
    where
        oneCounter :: (Atomic ConstTermExpr :<: f) => [ConstTermExpr] -> [Expr f]
        oneCounter cl@(h:cr) =
            counterTop h :
            counterBegin h :
            zipWith counterNext cl cr
        oneCounter _ = []

renderCalls ::
    (HasPredicates (Expr p) d,
     FuncOrd p,
     Atomic TypedVarExpr :<: p,
     HasItems (Expr di) d,
     DomainItem a :<: di,
     HasParameters (Expr t) a, TypedVar :<: t, -- addParamT constraints
     HasPrecondition (Expr f) a, Atomic TermExpr :<: f, And :<: f, Conjuncts f f, -- addPrecond
     HasEffect (Expr g) a, Atomic TermExpr :<: g, And :<: g, Not :<: g, Conjuncts g g) -- addEffect
    => d
    -> [(CallDesc, a, [CallDesc])]
    -> d
renderCalls dom callstructs =
    let
        defcalls = [ call | (call, _, _) <- callstructs]
        vars :: [TypedVarExpr]
        vars = [eVar $ 'v' : show n | n <- [1 :: Int ..]]
        sVars :: [Expr Var]
        sVars = repeat $ eVar "s" 
        dtasks = sort $ nub $
            [let 
                (_, callAtom :: Expr (Atomic TermExpr)) = callEffect call sVars
             in
             eAtomic (taskName callAtom) (take (length $ taskArgs callAtom) vars)
             | call <- defcalls ]
        preds =
            getPredicates dom
            ++ dtasks
                
    in
    setPredicates preds $
    addItems (map addCalls callstructs) $
    dom



deconstructHDomain ::
    StandardHTNDomain
    -> (PDDLDomain, [CallNode])
deconstructHDomain hdom =
    let
        
        -- (freeActions, callstructs) :: [(CallDesc, a, [CallDesc])]
        (freeActions, callstructs) = splitEithers $
            concatMap deconItem $ getItems hdom
        -- mytasks = nub $ map (\(cd, _, _) -> cd) callstructs
        --svars :: [TypedVarExpr]
        (svar1 :: TypedVarExpr) : svar2 : _ = 
                map (flip eTyped counterType . (currentT :: Int -> Expr Var)) [1..]
        {-
        tpreds =
            [let 
                name = taskName t
                ovars = taskArgs t :: [TypedVarExpr]
                sn =
                    head $
                    flip (++) [0] $
                    map getCDSVars $
                    filter ((==) name . taskName . getCDTask) $
                    mytasks
             in
             eAtomic name (ovars ++ take sn svars)
             | t <- getTaskHead hdom]
        -}     
        preds =
            [counterTop svar1,
             counterBegin svar1,
             counterNext svar1 svar2]
        --    ++ tpreds
    in
    (
    setName (getName hdom) $
    setRequirements (getRequirements hdom) $
    setTypes (getTypes hdom ++ [counterType]) $
    setConstants (getConstants hdom) $
    setPredicates (getPredicates hdom ++ preds) $
    setFunctions (getFunctions hdom) $
    setConstraints (getConstraints hdom) $
    setItems (map domainItem freeActions) $
    emptyDomain, callstructs)

type DeconItem = Either PDDLAction CallNode
class Functor f => ItemDeconstructor f where
    deconItem' :: f [DeconItem] -> [DeconItem]
deconItem :: (ItemDeconstructor f) =>
    Expr f -> [DeconItem]
deconItem = foldExpr deconItem'

instance (ItemDeconstructor f, ItemDeconstructor g) => ItemDeconstructor (f :+: g) where
    deconItem' (Inl x) = deconItem' x
    deconItem' (Inr y) = deconItem' y

instance ItemDeconstructor (DomainItem StandardHAction) where
    deconItem' (DomainItem ha) =
        (: []) $
        maybe (Left acopy) addDesc $ getTaskHead ha
        where
            acopy =
                setName (getName ha) $
                setParameters (getParameters ha) $
                setPrecondition (getPrecondition ha) $
                setEffect (getEffect ha) $
                defaultAction 
            addDesc thead =
                Right (StackCall thead 1, acopy, [])


instance ItemDeconstructor (DomainItem StandardMethod) where
    deconItem' (DomainItem m) =
        let 
            prefix = "htn_" ++ getName m ++ "_"
            mbranches = case (getBranches m) of
                [] -> [emptyBranch]
                bl -> bl
            preconds =
                map (\ pl -> catMaybes $ getPrecondition m : pl) $
                (\pls -> [ bprecondition b : pl | b <- mbranches | pl <- pls]) $
                inits $
                map (\b -> case (bprecondition b) of
                    Just pre -> Just $ eForAll (bparameters b) $ nnf $ eNot $ pre
                    Nothing -> Nothing) $
                mbranches
        in
        concat $ 
        [deconBranch prefix (fromJust $ getTaskHead m) (getParameters m) precond branch 
            | precond <- preconds 
            | branch <- mbranches]

deconBranch ::
    String
    -> Expr PDDLAtom
    -> [TypedVarExpr]
    -> [PreferenceGDExpr] 
    -> Branch PreferenceGDExpr
    -> [DeconItem]
deconBranch prefix task mparams preconds branch =
    let
        bprefix = prefix ++ branchName branch ++ "_"
        params = mparams ++ bparameters branch
        precond = conjunct preconds
            -- preconds 
            -- ++ maybe [] conjuncts (bprecondition branch)
        ntasks = 
            foldr (taskCall bprefix params) [] $
            zip [1..] $ tasks branch
        
        tname = bprefix ++ "start"
        tcall = StackCall task 1
        tact = setName tname $ 
            setParameters params $
            setPrecondition (Just precond) $
            defaultAction
        tcalls = case ntasks of
            [] -> []
            (ntask, _, _) : _ -> [ntask]
    in
    map Right $
    (tcall, tact, tcalls) :
    ntasks
    where
        taskCall :: String -> [TypedVarExpr] 
            -> (Int, Either (Expr PDDLAtom) [Expr PDDLAtom])
            -> [CallNode]
            -> [CallNode]
        taskCall _ _ (_, Right []) l = l
        taskCall bprefix params (n, Right [t]) l = taskCall bprefix params (n, Left t) l
        taskCall bprefix params (n, Right unordered) [] =
            let
                sn = length unordered
                
                lname = bprefix ++ show (n+1)
                ltask = eAtomic lname (map removeType params :: [TermExpr])
                lcall = StackCall ltask sn
                lact = setName lname $ setParameters params $ defaultAction 
                
                tname = bprefix ++ show n
                ttask = eAtomic tname (map removeType params :: [TermExpr])
                tcall = StackCall ttask 1
                tact = setName tname $ setParameters params $ defaultAction 
                tcalls = map (flip StackCall 1) unordered
            in
            [(tcall, tact, tcalls ++ [lcall]),
            (lcall, lact, [])]
        taskCall bprefix params (n, Right unordered) ((nextcall,nextact, nextcalls):l) =
            let
                sn = length unordered
                nextcall' = StackCall (getCDTask nextcall) (max sn $ getCDSVars nextcall)

                tname = bprefix ++ show n
                ttask = eAtomic tname (map removeType params :: [TermExpr])
                tcall = StackCall ttask 1
                tact = setName tname $ setParameters params $ defaultAction
                tcalls = map (flip StackCall 1) unordered
            in
            (tcall, tact, tcalls ++ [nextcall'])
            : (nextcall', nextact, nextcalls)
            : l
        taskCall bprefix params (n, Left ntask) l =
            let
                tname = bprefix ++ show n
                ttask = eAtomic tname (map removeType params :: [TermExpr])
                tcall = StackCall ttask 1
                tact = setName tname $ setParameters params $ defaultAction
                tcalls = StackCall ntask 1 :
                    case l of
                        [] -> []
                        (lcd, _, _) : _ -> [lcd]
            in
            (tcall, tact, tcalls)
            : l

splitEithers :: [Either a b] -> ([a], [b])
splitEithers = foldr select ([], [])
    where
        select (Left l) (ls, rs) = (l : ls, rs)
        select (Right r) (ls, rs) = (ls, r : rs)


findTaskNodes :: [CallNode] -> StdTask -> [CallNode]
findTaskNodes callNet task =
    filter isTask callNet
    where
        isTask (cd, _, _) = 
            taskName task == taskName (getCDTask cd)

findTaskCallers :: StdTask -> [CallNode] -> [(CallNode, [Int])]
findTaskCallers task =
    filter (\ (_, pl) -> not $ null pl) .
    map taskPositions
    where
        taskPositions cn@(_, _, cdl) =
            (cn, map fst $ filter (isTask . snd) $ zip [0..] cdl)
        isTask cdesc = taskName task == taskName (getCDTask cdesc)

type CallNetOptimizer = [CallNode] -> Maybe [CallNode]

optimizeCallNet :: [CallNetOptimizer] -> [CallNode] -> [CallNode]
optimizeCallNet opts callnet =
    maybe callnet (optimizeCallNet opts) $
    findOpt opts
    where
        findOpt :: [CallNetOptimizer] -> Maybe [CallNode]
        findOpt [] = Nothing
        findOpt (opt : l) =
            maybe (findOpt l) Just $
            opt callnet




{-
Parameter optimization:
Eliminate extraneous parameters from task calls and actions.
We can eliminate a parameter from a call node iff both:
 - It's not used in a call
 - It's not used in a precondition or effect
-}

unusedTerm :: 
    CallNode -> TermExpr -> Bool
unusedTerm (_, action, calls) term =
    let 
        precondAtoms = maybe [] findAtoms $ getPrecondition action
        effectAtoms  = maybe [] findAtoms $ getEffect action
        inAction = term `elem` concatMap taskArgs (precondAtoms ++ effectAtoms)
        inCall = term `elem` concatMap (taskArgs . getCDTask) calls
    in
    not $ inAction || inCall

unusedParams ::
    CallNode -> [Int]
unusedParams cn@(call, _, _) =
    map fst $
    filter (unusedTerm cn . snd) $
    zip [0..] $
    taskArgs $
    getCDTask call


removeParams :: CallDesc -> [Int] -> CallDesc
removeParams (StackCall task sn) remList =
    flip StackCall sn $
    eAtomic (taskName task) $
    map snd $
    filter (flip elem remList . fst) $
    zip [0..] $
    taskArgs task

shortenCall :: String -> [Int] -> [CallNode] -> [CallNode]
shortenCall target remList callnet =
    map shorten callnet
    where
        shorten (tdesc, action, cdescs) =
            (matchShorten tdesc, action, map matchShorten cdescs)
        matchShorten desc
            | target == taskName (getCDTask desc) = removeParams desc remList
            | otherwise = desc

paramOpt :: CallNetOptimizer 
paramOpt calls =
    let
        toOpt = map (\cn -> (cn, unusedParams cn)) calls
    in
    if null (filter (not . null . snd) toOpt) then
        Nothing
    else
        Just $
        foldl 
        (\calls' ((cd, _, _), remList) -> shortenCall (taskName $ getCDTask cd) remList calls')
        calls
        toOpt


{-
Head to Tail Node Collapsing:

A calling node can be collapsed into the 
 - Either Calling's effects and preconds are empty or Called's effects and preconds are
 - No other nodes call Called

-}


{-
Inlining

-}
buildInlineChain :: (Monad m) => [CallNode] -> CallNode -> m CallNode
buildInlineChain callNet node@(cd, action, cdl) = do
    when (not isEndPoint) $ fail "Not endpoint."
    return node
    where
        isEndPoint
            | not (null cdl) = False
            | length (findTaskNodes callNet $ getCDTask cd) > 1 = False
            | otherwise = True 
            
