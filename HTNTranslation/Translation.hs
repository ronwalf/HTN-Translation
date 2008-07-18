{-# OPTIONS
 -fglasgow-exts
 -fallow-overlapping-instances
 -fallow-undecidable-instances
 #-}
module HTNTranslation.Translation

where

import Data.Generics (Data) 
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Text.PrettyPrint

import Planning.PDDL.Representation
import HTNTranslation.HTNPDDL

-- Utils
varId :: (:<:) Var f => Expr Var -> Expr f
varId (In (Var v)) = eVar v

varIds :: (:<:) Var f => [Expr Var] -> [Expr f]
varIds vars = map varId vars

action n pl pre eff = Action 
    (Name n) 
    (Parameters pl)
    (Precondition pre)
    (Effect eff)

-- Fixed stack functions
stackType = eConst "STACKITEM"
stackTop :: (:<:) (Atomic (Expr f)) g => [Expr f] -> Expr g
stackTop vars = eAtomic "stackTop" vars
--stackList :: (:<:) (Atomic (Expr f)) g => [Expr f] -> [Expr f] -> Expr g
--stackList oVars nVars = eAtomic "stackList" $ oVars ++ nVars


beginP :: ((:<:) (Atomic (Expr f)) g) => Expr f -> Expr g
beginP var = eAtomic "counterBegin" [var]
endP :: ((:<:) (Atomic (Expr f)) g) => Expr f -> Expr g
endP var = eAtomic "counterEnd" [var]
nextP :: ((:<:) (Atomic (Expr f)) g) => Expr f -> Expr f -> Expr g
nextP ovar nvar = eAtomic "counterNext" [ovar, nvar]
sameP :: ((:<:) (Atomic (Expr f)) g) => Expr f -> Expr f -> Expr g
sameP ovar nvar = eAtomic "counterSame" [ovar, nvar]


--startP :: (:<:) (Atomic (Expr f)) g => [Expr f] -> Bool -> String -> [Expr f] -> Expr g
--startP stackVars True name args = eAtomic ("start_" ++ name) args
--startP stackVars False name args = eAtomic ("start_" ++ name) $ args ++ stackVars
startP name args = eAtomic ("start_" ++ name) args



stackVar :: [Expr Var] -> Int -> Expr Var
stackVar vars n = head $
    filter (Prelude.not . (`elem` vars)) 
    [ eVar ("svar" ++ (show n) ++ postfix) 
        | postfix <- inits $ repeat '_']

stackVars vars n = take n [ stackVar vars i | i <- [1..n] ]

stackCondition :: forall f g.
    ((:<:) And g, 
     (:<:) Or g, 
     (:<:) Not g,
     (:<:) (Atomic (Expr f)) g) =>
    [Expr f] -> [Expr f] -> (Expr g)
stackCondition oldVars newVars = 
    let
        stackP = stackTop oldVars
        varPairs = zip oldVars newVars
        parts = [eAnd $ stackIncr varPairs n | n <- [0 .. (length oldVars - 1)]]
    in
    eAnd [ stackP, eOr parts ]
    where
        stackIncr varPairs pos =
            let 
                (prevars, var:postvars) = splitAt pos varPairs 
            in
            concat [
                [endP ov, beginP nv]
                | (ov, nv) <- prevars] ++
            [nextP (fst var) (snd var)] ++
            [sameP ov nv | (ov, nv) <- postvars]

incrementStack :: ((:<:) And g, (:<:) (Atomic (Expr f)) g, (:<:) Not g) =>
    [Expr f] -> [Expr f] -> Expr g
incrementStack oldVars newVars =
    eAnd [ eNot $ stackTop oldVars, stackTop newVars ]


-- Atomic Action collection
class Functor f => AtomicTester f where
    atomicTest :: f (String, Bool) -> (String, Bool)
instance (AtomicTester f, AtomicTester g) => AtomicTester (f :+: g) where
    atomicTest (Inl x) = atomicTest x
    atomicTest (Inr y) = atomicTest y
instance (Data p, Data e) => AtomicTester (DomainItem (Action p e)) where
    atomicTest (DomainItem a) = (getName a, True)
instance (Data p) => AtomicTester (DomainItem (Method p)) where
    atomicTest (DomainItem m) = (taskName $ getTaskHead m, and $ map (null . tasks) $ getBranches m)

atomicTester :: (AtomicTester f) => [Expr f] -> (String -> Bool)
atomicTester tl =
    let
        atomicList = 
            mapMaybe (\ (n, s) -> case s of
                False -> Just n
                True -> Nothing) $ 
            map (foldExpr atomicTest) tl
        atomicSet = Set.fromList atomicList
    in
    \x -> not $ Set.member x atomicSet

-- Domain Modification.  
-- The first position is for list which actions need to be controlled
-- The second position is the list of generated actions
data DomainMod c = 
    DomainMod { newPredicates :: [Expr (Atomic TypedVarExpr)], controlledActions::[String], modActions::[c] }
    | DomainFailure String
instance (PDDLDoc f) => Show (DomainMod (Expr f)) where
    show (DomainFailure txt) = show $
        parens $ 
        text "DomainTranslationFailure:" <+> text txt
    show (DomainMod pl cl el) = show $ parens $ sep [
        text "DomainModification",
        parens $ sep $ text "NewPredicates" : map (\ (In x) -> pddlDoc x) pl,
        parens $ sep $ text "ControlledActions" : map text cl,
        parens $ sep $ text "Actions" : map (\ (In a) -> pddlDoc a) el]

addDMods dml =
   DomainMod
    (concatMap newPredicates dml)
    (concatMap controlledActions dml) 
    (concatMap modActions dml)


--
-- Task collection
--
class Functor f => TaskCollector f where
    collectTask :: f (Maybe (Expr (Atomic TypedVarExpr))) -> (Maybe (Expr (Atomic TypedVarExpr)))

instance (TaskCollector f, TaskCollector g) => TaskCollector (f :+: g) where
    collectTask (Inl x) = collectTask x
    collectTask (Inr y) = collectTask y

instance TaskCollector (DomainItem (Action p e)) where
    collectTask _ = Nothing

instance Data p => TaskCollector (DomainItem (Method p)) where
    collectTask (DomainItem m) = Just $ startP
        (taskName $ getTaskHead m)
        [ eVar ('v' : show n) :: TypedVarExpr | n <- [1 .. (length $ taskArgs $ getTaskHead m)] ]

-- 
-- Item control transformation
-- 
class Functor f => ItemController f where
    controlIfMatch :: 
        (String -> Bool) -> 
        f (Maybe (Expr (Atomic TypedVarExpr)), Expr f) -> 
        (Maybe (Expr (Atomic TypedVarExpr)), Expr f)

instance 
    (Data (Expr p), Data (Expr e),
    (:<:) StdAtomicType p,
    (:<:) StdAtomicType e,
    (:<:) And p,
    (:<:) And e,
    (:<:) Not e,
    HasName a,
    HasParameters (TypedVarExpr) a,
    HasPrecondition (Expr p) a,
    HasEffect (Expr e) a) => ItemController (DomainItem a) where
    controlIfMatch matcher (DomainItem a)
        | not (matcher $ getName a) = (Nothing, domainItem a)
        | otherwise =
        let
            name = getName a
            vars = (varIds $ map removeType $ getParameters a) :: [TermExpr]
            controlPre = case (getPrecondition a) of
                Just pre -> eAnd [startP name vars, pre]
                Nothing -> startP name vars
            controlEffect = case (getEffect a) of
                Just eff -> eAnd [ eNot $ startP name vars, eff ]
                Nothing -> eNot $ startP name vars
        in
        (Just $ startP name $ getParameters a, 
         domainItem $ 
         setPrecondition (Just controlPre) $
         setEffect (Just controlEffect) a)

class Functor f => ItemTranslator a c f where
    translateItem :: a -> f (DomainMod c) -> DomainMod c
instance (ItemTranslator a c f, ItemTranslator a c g) => ItemTranslator a c (f :+: g) where
    translateItem a (Inl x) = translateItem a x
    translateItem a (Inr y) = translateItem a y

instance (:<:) (DomainItem (Action p e)) c => ItemTranslator a (Expr c) (DomainItem (Action p e)) where
    translateItem _ (DomainItem a) = DomainMod [] [] [domainItem a]

type MethodTransConfig a = (String -> Bool, [TypedVarExpr], [Expr Var], [Expr Var], a)

instance forall p e f .
    ((:<:) And p, 
     (:<:) Or p, 
     (:<:) Not p, 
     (:<:) (ForAll TypedVarExpr) p, 
     (:<:) StdAtomicType p,
     (:<:) And e,
     (:<:) Not e,
     (:<:) StdAtomicType e,
     (:<:) (DomainItem (Action (Expr p) (Expr e))) f,
     Data (Expr p),
     Data (Expr e)
    ) =>
    ItemTranslator 
        (MethodTransConfig (Expr e))
        (Expr f)
        (DomainItem (Method (Expr p)))
    where
    translateItem desc@(isAtomic, stackParams, oVars, nVars, template) (DomainItem m) =
        let
            controlled = 
                (map taskName $ concatMap tasks (getBranches m)) 
            prefix = (taskName $ getTaskHead m) ++ "_" ++ getName m
            branchControlP :: Expr p
            branchControlP = startP (taskName $ getTaskHead m) (taskArgs $ getTaskHead m)
            mbranches :: [Branch (Expr p)]
            mbranches = case (getBranches m) of
                [] -> [emptyBranch]
                bl -> bl
            atomicity = isAtomic $ taskName $ getTaskHead m
            preconds :: [[Expr p]]
            preconds =
                map (\ pl -> catMaybes $ getPrecondition m : pl) $
                inits $
                map (\b -> case (bprecondition b) of
                    Just pre -> Just $ eForAll (bparameters b) $ eNot $ pre
                    Nothing -> Nothing) $
                mbranches -- :: [[Expr c]]
        in
        addDMods $
        map (\ (pre, br) -> translateBranch 
            desc 
            prefix 
            (getTaskHead m) 
            (getParameters m) 
            (pre :: [Expr p])
            br
            :: DomainMod (Expr f)) $
        zip preconds $ mbranches

translateBranch :: forall p e f .
    (
     (:<:) And p, 
     (:<:) StdAtomicType p, 
     (:<:) Not p, 
     (:<:) Or p,
     (:<:) (ForAll TypedVarExpr) p,
     (:<:) And e,
     (:<:) Not e,
     (:<:) StdAtomicType e,
     (:<:) (DomainItem (Action (Expr p) (Expr e))) f) =>
    MethodTransConfig (Expr e) -> -- Method Translation Config
    String -> -- Branch prefix
    Expr (StdAtomicType) -> -- Task
    [TypedVarExpr] -> -- Method parameters, should be disjoint from branch params
    [Expr p] -> -- list of preconditions for branch to execute (not including branch precond)
    Branch (Expr p) -> -- Actual branch
    DomainMod (Expr f) -- resulting domain modifications
translateBranch (isAtomic, stackParams, oVars, nVars, template) prefix task methodParams precondList branch
    | null (tasks branch) && (isAtomic $ taskName task) = DomainMod [] [] [
        domainItem $ action 
            (prefix ++ "_" ++ branchName branch)
            (methodParams ++ bparameters branch)
            (Just $ eAnd $ precondList ++ (maybeToList $ bprecondition branch))
            (Just $ eNot $ eAtomic (taskName task) (taskArgs task) :: Maybe (Expr e))]
    | otherwise =
    let
        -- Number of sub tasks
        numTasks = length $ tasks branch
        -- Branches with empty task lists or atomic final tasks need to pop the stack
        popStack = numTasks == 0 || isAtomic (taskName $ last $ tasks branch)
        -- TermExpr versions of oVars, nVars
        otVars = varIds oVars :: [TermExpr]
        ntVars = varIds nVars :: [TermExpr]
        numActions = numTasks + if popStack then 1 else 0
        bnames = [prefix ++ "_" ++ (branchName branch) ++ show n | n <- [1..numActions]]
        params1 = methodParams ++ bparameters branch ++ take (length oVars) stackParams
        params2 = params1 ++ drop (length oVars) stackParams
        pvars = varIds $ map removeType params1 :: [TermExpr]
        controlPs :: [Expr p]
        controlPs = startP (taskName task) (taskArgs task) :
            tail [startP bname pvars | bname <- bnames ]
        controlEs :: [Expr e]
        controlEs = startP (taskName task) (taskArgs task) : 
            tail [ startP bname pvars | bname <- bnames ]
        controlledNames = [taskName t | t <- tasks branch, isAtomic (taskName t)]
        newPreds = [ startP name params1 | name <- tail bnames ]
        -- Which sub tasks are atomic:
        atomicities = map (isAtomic . taskName) $ tasks branch

        -- basic preconditions for all tasks
        basicPreconds =
            [[ controlP, stackTop otVars] 
                | controlP <- controlPs]
        -- preconditions with waiting for previous task
        waitingPreconds = head basicPreconds : [ pl ++ [eNot prevP]
            | prevP <- controlPs
            | pl <- tail basicPreconds ]
        -- preconditions for non-atomic sub tasks
        stackedPreconds = [ pl ++ if atomicity then [] else [stackCondition otVars ntVars]
            | atomicity <- atomicities
            | pl <- waitingPreconds ] ++ 
            if popStack then [last waitingPreconds ++ [stackCondition ntVars otVars]] else []
        allPreconds = (precondList ++ head stackedPreconds) : tail stackedPreconds
        -- Effects
        -- starting subtask
        startEffects = [ eNot controlE : startEffect t
            | t <- tasks branch 
            | controlE <- controlEs ] ++
            if popStack then 
                [[eNot $ last controlEs, eNot $ stackTop otVars, stackTop ntVars]] else []
        -- plus startNext
        allEffects = [ pl ++ [controlE]
            | pl <- startEffects
            | controlE <- tail controlEs] ++ [last startEffects]

        actions = [ domainItem $ action
            name
            (if needAllStack then params2 else params1)
            (Just $ eAnd preconds)
            (Just $ eAnd effs)
            | name <- bnames
            | needAllStack <- atomicities ++ if popStack then [True] else []
            | preconds <- allPreconds
            | effs <- allEffects ]
    in
    DomainMod newPreds controlledNames actions
    where
        startCondition :: Expr StdAtomicType -> [Expr p]
        startCondition t
            | isAtomic (taskName t) = []
            | otherwise = [stackCondition (varIds oVars :: [TermExpr]) (varIds nVars :: [TermExpr])]
        startEffect :: Expr StdAtomicType -> [Expr e]
        startEffect t
            | isAtomic (taskName t) = [startP (taskName t) (taskArgs t)]
            | otherwise = [ 
                startP (taskName t) (taskArgs t),
                eNot $ stackTop (varIds oVars :: [TermExpr]),
                stackTop (varIds nVars :: [TermExpr])]
        finishCondition :: Expr StdAtomicType -> Expr p
        finishCondition t
            | isAtomic (taskName t) = eNot $ startP (taskName t) (taskArgs t)
            | otherwise = stackTop (varIds oVars :: [TermExpr])


eUnique :: Eq (Expr f) => (String -> Expr f) -> [Expr f] -> String -> Expr f
eUnique f el prefix =
    head $
    filter (\ e -> not $ e `elem` el) $
    map f $
    [ prefix ++ scores | scores <- inits $ repeat '_' ]
    

translateDomain stackArity domain template =
    let
        isAtomic = atomicTester $ items domain
        (oVars, nVars) = 
            splitAt stackArity [eVar ("v" ++ show n) :: Expr Var | n <- [1.. (2*stackArity)]]
        stackArgs = [ eTyped v stackType :: TypedVarExpr
            | v <- oVars ++ nVars ] :: [TypedVarExpr]
        DomainMod newPreds controlled domActions = addDMods $
            map (foldExpr $ translateItem (isAtomic, stackArgs, oVars, nVars, template)) $
            items domain
        (startPreds, cActions) = unzip $
            map (foldExpr (controlIfMatch (\x -> x `elem` controlled))) domActions
        taskPreds = mapMaybe (foldExpr collectTask) $ items domain
    in
    Domain {
        domainName = domainName domain,
        requirements = requirements domain,
        types = types domain ++ [eConst "STACKITEM" :: TypedConstExpr],
        constants = constants domain,
        predicates = predicates domain ++
            [stackTop (take stackArity stackArgs), 
             beginP (stackArgs !! 0),
             endP (stackArgs !! 0),
             nextP (stackArgs !! 0) (stackArgs !! 1),
             sameP (stackArgs !! 0) (stackArgs !! 1)] ++
             taskPreds ++
             catMaybes startPreds ++
             newPreds,
        items = cActions
    }

translateProblem numDigits stackArity problem =
    let
        stackItems = [ eConst $ "stackDigit" ++ show n | n <- [1..numDigits] ] :: [Expr Const]
        stackRel =
            stackTop (replicate (stackArity - 1) (stackItems !! 0) ++ [stackItems !! 1]) :
            beginP (head stackItems) :
            endP (last stackItems) :
            [sameP i i | i <- stackItems] ++
            [nextP i1 i2 | i1 <- stackItems | i2 <- tail stackItems]
    in
    problem {
        objects = objects problem ++ [eTyped i stackType | i <- stackItems],
        initial = initial problem ++ stackRel
    }
        
