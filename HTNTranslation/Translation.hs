{-# LANGUAGE OverlappingInstances, UndecidableInstances#-}
module HTNTranslation.Translation
where

import Data.Generics (Data) 
import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Planning.Util
import HTNTranslation.HTNPDDL

-- Utils
varId :: (:<:) Var f => Expr Var -> Expr f
varId (In (Var v)) = eVar v

varIds :: (:<:) Var f => [Expr Var] -> [Expr f]
varIds vars = map varId vars

action :: String -> [TypedVarExpr] -> Maybe p -> Maybe e -> Action p e
action n pl pre eff = Action 
    (Name n) 
    (Parameters pl)
    (Precondition pre)
    (Effect eff)

-- Fixed stack functions
stackType :: forall f. (:<:) Const f => Expr f
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
startP :: (:<:) (Atomic t) f => String -> [t] -> Expr f
startP name args = eAtomic ("start_" ++ name) args
finishP:: (:<:) (Atomic t) f => String -> [t] -> Expr f
finishP name args = eAtomic ("finished_" ++ name) args

startTaskH :: (Atomic t :<: f) => Maybe (Expr (Atomic t)) -> Maybe (Expr f)
startTaskH Nothing = Nothing
startTaskH (Just th) = Just $ startP (taskName th) (taskArgs th)
endTaskH:: (Atomic t :<: f) => Maybe (Expr (Atomic t)) -> Maybe (Expr f)
endTaskH Nothing = Nothing
endTaskH (Just th) = Just $ finishP (taskName th) (taskArgs th)


stackVar :: [Expr Var] -> Int -> Expr Var
stackVar vars n = head $
    filter (Prelude.not . (`elem` vars)) 
    [ eVar ("svar" ++ (show n) ++ postfix) 
        | postfix <- inits $ repeat '_']

stackVars :: [Expr Var] -> Int -> [Expr Var]
stackVars vars n = take n [ stackVar vars i | i <- [1..n] ]

stackCondition :: forall f g.
    ((:<:) And g, 
     (:<:) Or g, 
     (:<:) Not g,
     (:<:) (Atomic (Expr f)) g,
     Conjuncts g g) =>
    [Expr f] -> [Expr f] -> (Expr g)
stackCondition oldVars newVars = 
    let
        varPairs = zip oldVars newVars
        parts = [conjunct $ stackIncr varPairs n | n <- [0 .. (length oldVars - 1)]]
    in
    eOr parts
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

incrementStack :: ((:<:) And g, (:<:) (Atomic (Expr f)) g, (:<:) Not g, NNF g g, Conjuncts g g) =>
    [Expr f] -> [Expr f] -> Expr g
incrementStack oldVars newVars =
    conjunct [ nnf $ eNot $ stackTop oldVars, stackTop newVars ]


-- Atomic Action collection
class Functor f => AtomicTester f where
    atomicTest :: f (String, Bool) -> (String, Bool)
instance (AtomicTester f, AtomicTester g) => AtomicTester (f :+: g) where
    atomicTest (Inl x) = atomicTest x
    atomicTest (Inr y) = atomicTest y
instance (Data p, Data e) => AtomicTester (DomainItem (HAction p e)) where
    atomicTest (DomainItem a) = 
        maybe (getName a, True) (\th -> (taskName th, True)) $
        getTaskHead a
instance (Data p) => AtomicTester (DomainItem (Method p)) where
    atomicTest (DomainItem m) =
        let t = getTaskHead m in
        maybe ("", False) (\th -> (taskName th, and $ map (null . tasks) $ getBranches m)) t

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
data DomainMod c = 
    DomainMod { newPredicates :: [Expr (Atomic TypedVarExpr)], modActions::[c] }
    | DomainFailure String

addDMods :: [DomainMod c] -> DomainMod c
addDMods dml =
   DomainMod
    (concatMap newPredicates dml)
    (concatMap modActions dml)


--
-- Task collection
--
class Functor f => TaskCollector f where
    collectTask :: f [Expr (Atomic TypedVarExpr)]-> [Expr (Atomic TypedVarExpr)]

instance (TaskCollector f, TaskCollector g) => TaskCollector (f :+: g) where
    collectTask (Inl x) = collectTask x
    collectTask (Inr y) = collectTask y

instance (Data p, Data e) => TaskCollector (DomainItem (HAction p e)) where 
    collectTask (DomainItem a)
        | isNothing (getTaskHead a) = []
        | otherwise = 
        let 
            th = fromJust $ getTaskHead a 
            name = taskName th
            args = [ eVar ('v' : show n) :: TypedVarExpr | n <- [1 .. (length $ taskArgs th)] ]
        in 
        [startP name args, finishP name args]
    

instance Data p => TaskCollector (DomainItem (Method p)) where
    collectTask (DomainItem m)
        | isNothing (getTaskHead m) = []
        | otherwise = 
        let 
            th = fromJust $ getTaskHead m 
            name = taskName th
            args = [ eVar ('v' : show n) :: TypedVarExpr | n <- [1 .. (length $ taskArgs th)] ]
        in
        [startP name args, finishP name args]

--
-- Item Translation
--
class Functor f => ItemTranslator a c f where
    translateItem :: a -> f (DomainMod c) -> DomainMod c
instance (ItemTranslator a c f, ItemTranslator a c g) => ItemTranslator a c (f :+: g) where
    translateItem a (Inl x) = translateItem a x
    translateItem a (Inr y) = translateItem a y

instance 
    ((:<:) (DomainItem (Action (Expr p) (Expr e))) c, 
     (:<:) And p,
     (:<:) And e,
     (:<:) Not e,
     (:<:) PDDLAtom p,
     (:<:) PDDLAtom e,
     NNF p p,
     Conjuncts e e,
     Conjuncts p p,
     Data (Expr p), 
     Data (Expr e)) => 
    ItemTranslator a (Expr c) (DomainItem (HAction (Expr p) (Expr e))) where
    translateItem _ (DomainItem a@(HAction rName rParams _ rPrecond rEffect))
        | isNothing (getTaskHead a) = 
            DomainMod [] [domainItem $ Action rName rParams rPrecond rEffect]
        | otherwise =
        let
            th = fromJust $ getTaskHead a
            pre = Just $ conjunct $ catMaybes [
                Just $ startP (taskName th) (taskArgs th),
                getPrecondition a]
            eff = Just $ conjunct $ catMaybes [
                Just $ eNot $ startP (taskName th) (taskArgs th),
                Just $ finishP (taskName th) (taskArgs th),
                getEffect a]
            newA = action (getName a) (getParameters a) pre eff
        in
        DomainMod [] [domainItem newA]

type MethodTransConfig a = (String -> Bool, [TypedVarExpr], [Expr Var], [Expr Var], a)

instance forall p e f .
    ((:<:) And p, 
     (:<:) Or p, 
     (:<:) Not p, 
     (:<:) (ForAll TypedVarExpr) p, 
     (:<:) PDDLAtom p,
     (:<:) And e,
     (:<:) Not e,
     (:<:) PDDLAtom e,
     (:<:) (DomainItem (Action (Expr p) (Expr e))) f,
     NNF p p,
     Data (Expr p),
     Data (Expr e),
     FreeVarsFindable p,
     Conjuncts p p,
     Conjuncts e e
    ) =>
    ItemTranslator 
        (MethodTransConfig (Expr e))
        (Expr f)
        (DomainItem (Method (Expr p)))
    where
    translateItem desc (DomainItem m) =
        let
            prefix = 
                --maybe "" (\th -> taskName th ++ "_") (getTaskHead m) ++ 
                getName m
            mbranches :: [Branch (Expr p)]
            mbranches = case (getBranches m) of
                [] -> [emptyBranch]
                bl -> bl
            preconds :: [[Expr p]]
            preconds =
                map (\ pl -> catMaybes $ getPrecondition m : pl) $
                (\pls -> [ bprecondition b : pl | b <- mbranches | pl <- pls]) $
                inits $
                map (\b -> case (bprecondition b) of
                    Just pre -> Just $ eForAll (bparameters b) $ nnf $ eNot $ pre
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
     (:<:) PDDLAtom p, 
     (:<:) Not p, 
     (:<:) Or p,
     (:<:) (ForAll TypedVarExpr) p,
     (:<:) And e,
     (:<:) Not e,
     (:<:) PDDLAtom e,
     (:<:) (DomainItem (Action (Expr p) (Expr e))) f,
     FreeVarsFindable p,
     Conjuncts p p,
     Conjuncts e e) =>
    MethodTransConfig (Expr e) -> -- Method Translation Config
    String -> -- Branch prefix
    StdTaskHead -> -- Task
    [TypedVarExpr] -> -- Method parameters, should be disjoint from branch params
    [Expr p] -> -- list of preconditions for branch to execute (not including branch precond)
    Branch (Expr p) -> -- Actual branch
    DomainMod (Expr f) -- resulting domain modifications
translateBranch (isAtomic, stackParams, oVars, nVars, _) prefix task methodParams precondList branch
    | null (tasks branch) && (maybe False (isAtomic . taskName) task) = DomainMod [] [
        domainItem $ action 
            (prefix ++ "_" ++ branchName branch)
            (methodParams ++ bparameters branch)
            (Just $ conjunct $ 
                maybeToList (startTaskH task) ++
                precondList ++ (maybeToList $ bprecondition branch))
            (task >>= (\th -> return $ 
                conjunct [eNot $ startP (taskName th) (taskArgs th), finishP (taskName th) (taskArgs th)]) 
                :: Maybe (Expr e))]
    | otherwise =
    let
        -- Number of sub tasks
        numTasks = length $ tasks branch
        -- Branches with empty task lists or atomic final tasks need to pop the stack
        popStack = numTasks == 0 || isAtomic (taskName $ last $ tasks branch)
        startStats = if null $ tasks branch then [] else
            reverse $ map snd $
            foldl startStatus [((firstTask, undefined), (True, Nothing))] $ 
            tail $ tasks branch
        headPrecond = 
            maybeToList (startTaskH task) 
            ++ precondList
            ++ (if startStats == [] && popStack then [stackTop otVars, stackCondition ntVars otVars] else
                if startStats /= [] && (fst $ head $ startStats) then [] else 
                    [stackTop otVars, stackCondition otVars ntVars])
            -- ++ [eAtomic (show startStats) ([] :: [TermExpr]) :: (Expr p)]
        -- TermExpr versions of oVars, nVars
        otVars = varIds oVars :: [TermExpr]
        ntVars = varIds nVars :: [TermExpr]
        numActions = numTasks + if popStack then 1 else 0
        bnames = [prefix ++ "_" ++ (branchName branch) ++ show n | n <- [1..numActions]]
        usedVars = if null $ tasks branch then [concatMap findFreeVars headPrecond] else
            (concatMap findFreeVars headPrecond ++ findFreeVars (head $ tasks branch)) :
            map findFreeVars (tail $ tasks branch) ++
            [[]]
        waitingVars = [] : [ 
            if isAtomic $ taskName prevTask then
                findFreeVars prevTask
            else
                []
            | prevTask <- tasks branch
            ]
        necessaryVars = map nub [
            concat remaining
            | remaining <- init $ tails $ zipWith (\x y -> nub (x ++ y)) usedVars waitingVars
            ]
        params0 = [
            filter (\v -> varId (removeType v) `elem` necessary) $
                methodParams ++ bparameters branch
            | necessary <- necessaryVars
            ]
        waitParams = take (length oVars) stackParams
        
        firstTask = head $ tasks branch
        pvars :: [[TermExpr]]
        pvars = [
            case status of
                (_, Just False) -> varIds $ map removeType $ params ++ waitParams
                _ -> varIds $ map removeType params
            | status <- startStats ++ if popStack then [(True, Nothing)] else []
            | params <- params0]
        tParams = [
            -- eVar (show startStats) : -- Debug code
            case status of
                (False, _) -> params ++ stackParams
                (_, Just False) -> params ++ waitParams
                _ -> params
            | status <- startStats ++ if popStack then [(False, undefined)] else []
            | params <- params0 ]
        controlEs :: [Expr PDDLAtom]
        controlEs = maybeToList task ++
            tail [ eAtomic bname tvars | bname <- bnames | tvars <- pvars ]
        newPreds = concat [
            let pl' = case status of
                    (_, Just False) -> params ++ waitParams
                    _ -> params
            in
            [startP name  pl', finishP name pl'] 
            | name <- tail bnames 
            | params <- tail params0
            | status <- tail $ startStats ++ if popStack then [(undefined, Just True)] else []]
        -- Which sub tasks are atomic.  Always treat last task as atomic

        restPreconds = 
            [ startP bname tvars: startCondition otVars ntVars prevTask status 
                | prevTask <- tasks branch
                | bname <- tail bnames
                | tvars <- tail pvars
                | status <- tail startStats ] 
        allPreconds = headPrecond : restPreconds ++
            if not popStack then [] else [
                let lastTask = last $ tasks branch in
                [startP (last bnames) (last pvars),
                 finishP (taskName lastTask) (taskArgs lastTask),
                 stackTop otVars,
                 stackCondition ntVars otVars]]
        -- Effects
        -- starting subtask
        startEffects = [ eNot (startP (taskName controlE) (taskArgs controlE)) 
            : finishP (taskName controlE) (taskArgs controlE)
            : startEffect a t
            | t <- tasks branch 
            | (a, _) <- startStats
            | controlE <- controlEs ] ++
            if not popStack then [] else let lastE = last controlEs in
                [[eNot $ startP (taskName lastE) (taskArgs lastE),
                  finishP (taskName lastE) (taskArgs lastE),
                  eNot $ stackTop otVars, stackTop ntVars]]
        -- plus startNext
        allEffects = [ pl ++ [startP (taskName controlE) (taskArgs controlE), eNot $ finishP (taskName controlE) (taskArgs controlE)]
            | pl <- startEffects
            | controlE <- tail controlEs] ++ [last startEffects]

        actions = [ domainItem $ action
            name
            pl
            (Just $ conjunct preconds)
            (Just $ conjunct effs)
            | name <- bnames
            | pl <- tParams
            | preconds <- allPreconds
            | effs <- allEffects ]
    in
    DomainMod newPreds actions
    where
        startStatus :: 
            [((Expr PDDLAtom, Expr PDDLAtom),(Bool, Maybe Bool))] 
            -> Expr PDDLAtom
            -> [((Expr PDDLAtom, Expr PDDLAtom),(Bool, Maybe Bool))]
        startStatus [] _ = []
        startStatus (((prev, pprev), (_, pprevA)) : rest) curr =
            let prevA = isAtomic $ taskName prev in
            ((curr, prev), (True, Just prevA)) : ((prev, pprev), (prevA, pprevA)) : rest
        startCondition otVars ntVars prev atomicity 
            | atomicity == (True, Just True) = [finishP (taskName prev) (taskArgs prev)]
            | atomicity == (True, Just False) = [stackTop otVars]
            | otherwise = [stackTop otVars, stackCondition otVars ntVars]
        startEffect :: Bool -> Expr PDDLAtom -> [Expr e]
        startEffect atomicAction t
            | atomicAction = [startP (taskName t) (taskArgs t), eNot $ finishP (taskName t) (taskArgs t)]
            | otherwise = [
                eNot $ finishP (taskName t) (taskArgs t),
                startP (taskName t) (taskArgs t),
                eNot $ stackTop (varIds oVars :: [TermExpr]),
                stackTop (varIds nVars :: [TermExpr])]
        
eUnique :: Eq (Expr f) => (String -> Expr f) -> [Expr f] -> String -> Expr f
eUnique f el prefix =
    head $
    filter (\ e -> not $ e `elem` el) $
    map f $
    [ prefix ++ scores | scores <- inits $ repeat '_' ]
    
translateDomain :: (TaskCollector f, HasItems (Expr f) a, AtomicTester f,
    Data a, Data b, Data c,
    HasPredicates (Expr (Atomic TypedVarExpr)) a,
    HasConstants TypedConstExpr a,
    HasTypes TypedConstExpr a,
    HasRequirements a,
    HasName a,
    ItemTranslator (String -> Bool, [TypedVarExpr], [Expr Var], [Expr Var], t) b f) =>
    Int -> a -> t -> Domain c b
translateDomain stackArity domain template =
    let
        isAtomic = atomicTester $ getItems domain
        (oVars, nVars) = 
            splitAt stackArity [eVar ("v" ++ show n) :: Expr Var | n <- [1.. (2*stackArity)]]
        stackArgs = [ eTyped v stackType :: TypedVarExpr
            | v <- oVars ++ nVars ] :: [TypedVarExpr]
        DomainMod newPreds domActions = addDMods $
            map (foldExpr $ translateItem (isAtomic, stackArgs, oVars, nVars, template)) $
            getItems domain
        taskPreds = nub $ concatMap (foldExpr collectTask) $ getItems domain
    in
    setName (getName domain) $
    setRequirements (getRequirements domain) $
    setTypes (getTypes domain ++ [eConst "STACKITEM" :: TypedConstExpr]) $
    setConstants (getConstants domain) $
    setPredicates (getPredicates domain ++
        [stackTop (take stackArity stackArgs), 
        beginP (stackArgs !! 0),
        endP (stackArgs !! 0),
        nextP (stackArgs !! 0) (stackArgs !! 1),
        sameP (stackArgs !! 0) (stackArgs !! 1)] ++
        taskPreds ++
        newPreds) $
    setItems domActions emptyDomain 

translateProblem :: (Atomic (Expr ConstTerm) :<: g,
    HasConstants (Expr f) a,
    Typed (Expr Const) :<: f,
    HasInitial (Expr g) a) =>
    Int -> Int -> a -> a
translateProblem numDigits stackArity problem =
    let
        stackStrings = ["stackDigit" ++ show n | n <- [1..numDigits]]
        stackItems :: [ConstTermExpr]
        stackItems = map eConst stackStrings
        stackRel =
            stackTop (replicate (stackArity - 1) (stackItems !! 0) ++ [stackItems !! 1]) :
            beginP (head stackItems) :
            endP (last stackItems) :
            [sameP i i | i <- stackItems] ++
            [nextP i1 i2 | i1 <- stackItems | i2 <- tail stackItems]
    in
    setConstants (getConstants problem ++
        [eTyped (eConst i :: Expr Const) stackType | i <- stackStrings]) $
    setInitial ( getInitial problem ++ stackRel ) problem
        
