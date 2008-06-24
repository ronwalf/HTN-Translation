module HTNTranslation.Translation

where

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

-- Fixed stack functions

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
instance AtomicTester (Action c) where
    atomicTest (Action name _ _ _) = (name, True)
instance AtomicTester (Method c) where
    atomicTest (Method name _ (In (Atomic p _)) _ bl) = (p, and $ map (null . tasks) bl)

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

instance TaskCollector (Action c) where
    collectTask _ = Nothing

instance TaskCollector (Method c) where
    collectTask m = Just $ startP
        (taskName $ taskHead m)
        [ eVar ('v' : show n) :: TypedVarExpr | n <- [1 .. (length $ taskArgs $ taskHead m)] ]

-- 
-- Item control transformation
-- 
class Functor f => ItemController f where
    controlIfMatch :: 
        (String -> Bool) -> 
        f (Maybe (Expr (Atomic TypedVarExpr)), Expr f) -> 
        (Maybe (Expr (Atomic TypedVarExpr)), Expr f)

instance ((:<:) And c, (:<:) Not c, (:<:) StdAtomicType c) =>
    ItemController (Action (Expr c)) 
    where
    controlIfMatch matcher a@(Action name params precond effect)
        | not (matcher name) = (Nothing, action name params precond effect)
        | otherwise =
        let
            vars = (varIds $ map removeType params) :: [TermExpr]
            controlP = startP name vars
            controlPre = case precond of
                Just pre -> eAnd [controlP, pre]
                Nothing -> controlP
            controlEffect = eAnd [ eNot controlP, effect ]
        in
        (Just $ startP name params, 
         action name params (Just controlPre) controlEffect)

class Functor f => ItemTranslator a c f where
    translateItem :: a -> f (DomainMod (Expr (Action c))) -> DomainMod (Expr (Action c))
instance (ItemTranslator a c f, ItemTranslator a c g) => ItemTranslator a c (f :+: g) where
    translateItem a (Inl x) = translateItem a x
    translateItem a (Inr y) = translateItem a y

instance ItemTranslator a c (Action c) where
    translateItem _ (Action n vars precond effect) = DomainMod [] [] [action n vars precond effect]

type MethodTransConfig = (String -> Bool, [TypedVarExpr], [Expr Var], [Expr Var])

instance -- forall c .
    ((:<:) And c, (:<:) Or c, (:<:) Not c, (:<:) (ForAll TypedVarExpr) c, (:<:) StdAtomicType c)=> 
    ItemTranslator 
        MethodTransConfig
        (Expr c) 
        (Method (Expr c)) 
    where
    translateItem desc@(isAtomic, stackParams, oVars, nVars) m =
        let
            controlled = 
                (map taskName $ concatMap tasks (branches m)) 
            prefix = (taskName $ taskHead m) ++ "_" ++ methodName m
            branchControlP = startP (taskName $ taskHead m) (taskArgs $ taskHead m)
            mbranches = case (branches m) of
                [] -> [emptyBranch]
                bl -> bl
            atomicity = isAtomic $ taskName $ taskHead m
            preconds = map ((branchControlP : maybeToList (mprecondition m))++) $ 
                inits $
                mapMaybe (\b -> case (bprecondition b) of
                    Just pre -> Just $ 
                        (if null (bparameters b) then id else eForAll (bparameters b)) $ 
                        eNot $ pre
                    Nothing -> Nothing) $ 
                mbranches -- :: [[Expr c]]
        in
        addDMods $
        map (\ (pre, br) -> translateBranch desc prefix (taskHead m) (mparameters m) pre br) $
        zip preconds $ mbranches

translateBranch :: forall f .
    ((:<:) And f, (:<:) StdAtomicType f, (:<:) Not f, (:<:) Or f) =>
    MethodTransConfig -> -- Method Translation Config
    String -> -- Branch prefix
    Expr (StdAtomicType) -> -- Task
    [TypedVarExpr] -> -- Method parameters, should be disjoint from branch params
    [Expr f] -> -- list of preconditions for branch to execute (not including branch precond)
    Branch (Expr f) -> -- Actual branch
    DomainMod (Expr (Action (Expr f))) -- resulting domain modifications
translateBranch (isAtomic, stackParams, oVars, nVars) prefix task methodParams precondList branch
    | null (tasks branch) && (isAtomic $ taskName task) = DomainMod [] [] [
        action 
            (prefix ++ "_" ++ branchName branch)
            (methodParams ++ bparameters branch)
            (Just $ eAnd $ precondList ++ (maybeToList $ bprecondition branch))
            (eNot $ eAtomic (taskName task) (taskArgs task))]
    | otherwise =
    let
        numActions = (length $ tasks branch) +
            if (isAtomic $ taskName $ last $ tasks branch) then 1 else 0
        bnames = [prefix ++ "_" ++ (branchName branch) ++ show n | n <- [1..numActions]]
        params1 = methodParams ++ bparameters branch ++ take (length oVars) stackParams
        params2 = params1 ++ drop (length oVars) stackParams
        pvars = varIds $ map removeType params1 :: [TermExpr]
        controlPs = [ startP bname pvars
            | bname <- bnames ]
        controlledNames = [taskName t | t <- tasks branch, isAtomic (taskName t)]
        newPreds = [ startP name params1 | name <- tail bnames ]
        headAction = action (head bnames)
            (if (isAtomic $ taskName (head $ tasks branch)) then params1 else params2)
            (Just $ eAnd $ 
                precondList ++
                [stackTop $ (varIds oVars :: [TermExpr])] ++
                startCondition (head $ tasks branch) ++
                (maybeToList $ bprecondition branch))
            (eAnd $ startEffect (head $ tasks branch) ++ [
                controlPs !! 1,
                eNot $ startP (taskName task) (taskArgs task)])
        tailAction = if (not $ isAtomic $ taskName $ last $ tasks branch) then 
            [action (last bnames) params1
                (Just $ eAnd [
                    last controlPs,
                    finishCondition $ last $ init $ tasks branch])
                (eAnd [
                    startP (taskName $ last $ tasks branch) (taskArgs $ last $ tasks branch),
                    eNot $ last $ controlPs])]
            else
            [action (last bnames) params2
                (Just $ eAnd [
                    startP (last bnames) pvars,
                    finishCondition (last $ tasks branch),
                    stackTop (varIds oVars :: [TermExpr]),
                    stackCondition (varIds nVars :: [TermExpr]) (varIds oVars :: [TermExpr])])
                (eAnd [
                    eNot $ startP (last bnames) pvars,
                    eNot $ stackTop (varIds oVars :: [TermExpr]),
                    stackTop (varIds nVars :: [TermExpr])])]
                    
        actions = headAction : [ action
            name
            (if (isAtomic $ taskName currTask) then params1 else params2)
            (Just $ eAnd $ startCondition currTask ++
                [controlP, finishCondition prevTask]) -- BUGBUG)
            (eAnd $ 
                startEffect currTask ++
                if (isAtomic (taskName currTask)) then [controlNext] else [])
            | name <- tail bnames
            | controlP <- tail controlPs
            | prevTask <- tasks branch
            | currTask <- tail $ tasks branch
            | controlNext <- tail $ tail controlPs ] ++
            tailAction
    in
    DomainMod newPreds controlledNames actions
    where
        startCondition :: Expr StdAtomicType -> [Expr f]
        startCondition t
            | isAtomic (taskName t) = []
            | otherwise = [stackCondition (varIds oVars :: [TermExpr]) (varIds nVars :: [TermExpr])]
        startEffect :: Expr StdAtomicType -> [Expr f]
        startEffect t
            | isAtomic (taskName t) = [startP (taskName t) (taskArgs t)]
            | otherwise = [ 
                startP (taskName t) (taskArgs t),
                eNot $ stackTop (varIds oVars :: [TermExpr]),
                stackTop (varIds nVars :: [TermExpr])]
        finishCondition :: Expr StdAtomicType -> Expr f
        finishCondition t
            | isAtomic (taskName t) = eNot $ startP (taskName t) (taskArgs t)
            | otherwise = stackTop (varIds oVars :: [TermExpr])


eUnique :: Eq (Expr f) => (String -> Expr f) -> [Expr f] -> String -> Expr f
eUnique f el prefix =
    head $
    filter (\ e -> not $ e `elem` el) $
    map f $
    [ prefix ++ scores | scores <- inits $ repeat '_' ]
    


translateDomain stackArity domain =
    let
        isAtomic = atomicTester $ items domain
        stackType = "STACKDIGIT"
        (oVars, nVars) = 
            splitAt stackArity [eVar ("v" ++ show n) :: Expr Var | n <- [1.. (2*stackArity)]]
        stackArgs = [ typed v (eConst stackType) :: TypedVarExpr
            | v <- oVars ++ nVars ] :: [TypedVarExpr]
        DomainMod newPreds controlled domActions = addDMods $
            map (foldExpr (translateItem (isAtomic, stackArgs, oVars, nVars))) $
            items domain
        (startPreds, cActions) = unzip $
            map (foldExpr (controlIfMatch (\x -> x `elem` controlled))) domActions
        taskPreds = mapMaybe (foldExpr collectTask) $ items domain
    in
    Domain {
        domainName = domainName domain,
        requirements = requirements domain,
        types = types domain ++ [eConst stackType],
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
