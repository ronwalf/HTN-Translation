{-# OPTIONS_GHC
    -fcontext-stack=30
  #-}
{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    OverlappingInstances, 
    RankNTypes,
    ScopedTypeVariables,
    StandaloneDeriving, 
    TypeOperators,
    TypeSynonymInstances
  #-}
module HTNTranslation.HTNPDDL (
    module Planning.PDDL.PDDL3_0,
    StandardHTNDomain,
    HDomain(..), emptyHDomain,
    StandardHTNProblem,
    HProblem(..), emptyHProblem,
    Method(..), StandardMethod, defaultMethod,
    TaskHead(..), HasTaskHead, getTaskHead, setTaskHead,
    TaskList, TaskLists, TaskConstraint, TaskConstraints, 
    HasTaskLists(..), HasTaskConstraints(..),
    taskName, taskArgs, StdTask, StdTaskHead, StdTaskDef,
    enumerateTasks, numberTasks,
    findFirstTask, findLastTask, findLastTasks, findNextTasks, findPrevTasks,
    parseHTNPDDL, parseHTNProblem
) where

import Control.Monad.State
import Data.List
import Data.Generics (Data, Typeable, Typeable2)
import Data.Maybe
import Text.ParserCombinators.Parsec hiding (space)
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.PrettyPrint

import Planning.PDDL.PDDL3_0
import Planning.PDDL.Parser

data HDomain a b = HDomain
    Name
    Requirements
    (Types TypedConstExpr)
    (Constants TypedConstExpr)
    (Predicates (Expr (Atomic TypedVarExpr)))
    (TaskHead [StdTaskDef])
    (Functions TypedFuncExpr)
    (Constraints a)
    (Actions b)
    deriving (Data, Eq, Typeable)

instance (Data a, Data b) => HasName (HDomain a b)
instance (Data a, Data b) => HasRequirements (HDomain a b)
instance (Data a, Data b) => HasTypes TypedConstExpr (HDomain a b)
instance (Data a, Data b) => HasConstants TypedConstExpr (HDomain a b)
instance (Data a, Data b) => HasPredicates (Expr (Atomic TypedVarExpr)) (HDomain a b)
instance (Data a, Data b) => HasTaskHead [StdTaskDef] (HDomain a b)
instance (Data a, Data b) => HasFunctions TypedFuncExpr (HDomain a b)
instance (Data a, Data b) => HasConstraints a (HDomain a b)
instance (Data a, Data b) => HasActions b (HDomain a b)
instance (Data a, Data b, PDDLDoc a, PDDLDoc b) =>
    PDDLDoc (HDomain a b) where
    pddlDoc domain = parens $ ($$) (text "define") $ vcat $
        parens (text "domain" <+> text (getName domain)) :
         -- Requirement strings are prefixed with ':'
        (if (null $ getRequirements domain) then empty else parens
            (sep $
             map (text . (':':)) $
             "requirements" : getRequirements domain)) :
        parens (sep $ (text ":types") :
            [pddlDoc t | t <- getTypes domain]) :
        parens (sep $ (text ":predicates") :
            [pddlDoc p | p <- getPredicates domain]) :
        parens (sep $ (text ":tasks") :
            [pddlDoc p | p <- getTaskHead domain]) :
        space :
        intersperse space [pddlDoc x | x <- getActions domain]

instance (Data a, Data b, PDDLDoc a, PDDLDoc b) => Show (HDomain a b) where
    show domain = show $ pddlDoc domain
    
emptyHDomain :: forall a b. HDomain a b
emptyHDomain = HDomain
    (Name "empty")
    (Requirements [])
    (Types [])
    (Constants [])
    (Predicates [])
    (TaskHead [])
    (Functions [])
    (Constraints Nothing)
    (Actions [])


type StandardHTNDomain = HDomain ConstraintGDExpr StandardMethod
type StandardHTNProblem = HProblem InitLiteralExpr PreferenceGDExpr ConstraintGDExpr (Expr (Atomic ConstTermExpr))

--deriving instance Data (Expr (DomainItem StandardHAction :+: DomainItem StandardMethod))

data HProblem a b c t = HProblem
    Name
    DomainName
    Requirements
    (Constants TypedConstExpr)
    (Initial a)
    (TaskHead (Maybe t))
    (Goal b)
    (Constraints c)
    deriving (Data, Eq, Typeable)


instance (Data a, Data b, Data c, Data t) => HasName (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasDomainName (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasRequirements (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasConstants TypedConstExpr (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasInitial a (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasTaskHead (Maybe t) (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasGoal b (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasConstraints c (HProblem a b c t)

--instance 
--    (Data (Expr a), Data (Expr b), Data (Expr c),
--     PDDLDocExpr a, PDDLDocExpr b, PDDLDocExpr c) =>
--    Show (HProblem (Expr a) (Expr b) (Expr c)) where
instance (Data a, Data b, Data c, Data t,
        PDDLDoc a, PDDLDoc b, PDDLDoc c, PDDLDoc t) =>
        PDDLDoc (HProblem a b c t) where
    pddlDoc prob = parens $ sep $
        text "define" :
        (parens $ text "problem" <+> (text $ getName prob)) :
        (parens $ text ":domain" <+> (text $ getDomainName prob)) :
        (if null $ getRequirements prob then empty else 
           (parens $ sep $ text ":requirements" : map (text . (':':)) (getRequirements prob))) :
        docNonEmpty ":objects" (getConstants prob) :
        docNonEmpty ":init" (getInitial prob) :
        maybe empty (\x -> parens $ sep [text ":task", pddlDoc x]) (getTaskHead prob):
        maybe empty (\x -> parens $ sep [text ":goal", pddlDoc x]) 
            (getGoal prob) :
        docMaybe ":constraints" (getConstraints prob) : []

       
    
emptyHProblem :: forall a b c t. HProblem a b c t
emptyHProblem = HProblem
    (Name "empty")
    (DomainName "empty")
    (Requirements [])
    (Constants [])
    (Initial [])
    (TaskHead Nothing)
    (Goal Nothing)
    (Constraints Nothing)

data TaskHead f = TaskHead f deriving (Data, Eq, Typeable)
unTaskHead :: TaskHead t -> t
unTaskHead (TaskHead h) = h
class (Data a, Data f) => HasTaskHead f a | a -> f where
    getTaskHead :: a -> f
    getTaskHead = unTaskHead . fromJust . gfind
    setTaskHead :: f -> a -> a
    setTaskHead h r = fromJust $ greplace r (TaskHead h)

type TaskList = (Maybe String, [Expr PDDLAtom])
type NumberedTaskList = (Maybe String, [(Int, Expr PDDLAtom)])
taskName :: Expr (Atomic t) -> String
taskName (In (Atomic p _)) = p
taskArgs :: Expr (Atomic t) -> [t]
taskArgs (In (Atomic _ al)) = al

data TaskLists = TaskLists [TaskList] deriving (Data, Eq, Typeable)
unTaskLists :: TaskLists -> [TaskList]
unTaskLists (TaskLists tl) = tl
class (Data f) => HasTaskLists f where
    getTaskLists :: f -> [TaskList]
    getTaskLists = unTaskLists . fromJust . gfind
    setTaskLists :: [TaskList] -> f -> f
    setTaskLists tl f = fromJust $ greplace f (TaskLists tl)

 
-- Courtesy of Saizan (freenode #haskell)
numberTasks :: HasTaskLists m => m -> [NumberedTaskList]
numberTasks m =
    let next = do 
        n <- get
        put $ n+1
        return n
    in 
    flip evalState 0 $ 
    flip mapM (getTaskLists m) $ \(name, tasks) -> do
        tasks' <- flip mapM tasks $ \x -> do
            n <- next
            return (n, x)
        return (name, tasks')

enumerateTasks :: HasTaskLists m => m -> [(Int, Expr PDDLAtom)]
enumerateTasks = concatMap snd . numberTasks    
    
--findPrecursor :: (HasTaskLists m, HasTaskConstraints m) =>
--    Int -> m -> [ 

type TaskConstraint = (String, String)
data TaskConstraints = TaskConstraints [TaskConstraint] deriving (Data, Eq, Typeable)
unTaskConstraints :: TaskConstraints -> [TaskConstraint]
unTaskConstraints (TaskConstraints tc) = tc
class (Data f) => HasTaskConstraints f where
    getTaskConstraints :: f -> [TaskConstraint]
    getTaskConstraints = unTaskConstraints . fromJust . gfind
    setTaskConstraints :: [TaskConstraint] -> f -> f
    setTaskConstraints tc f = fromJust $ greplace f (TaskConstraints tc)

findFirstTask :: (HasTaskLists m, HasTaskConstraints m) =>
    m -> Maybe (Int, Expr PDDLAtom)
findFirstTask m =
    case winnow (getTaskConstraints m) (numberTasks m) of
        [(_, h : _)] -> Just h
        _ -> Nothing
    where
        winnow [] tl = tl
        winnow ((_, tlname) : cl) tl =
            winnow cl $
            filter (not . (== Just tlname) . fst) tl

findLastTasks :: (HasTaskLists m, HasTaskConstraints m) =>
    m -> [(Int, Expr PDDLAtom)]
findLastTasks m =
    map (last . snd) $
    winnow (getTaskConstraints m) (numberTasks m)
    where
        winnow :: [(String, String)] -> [(Maybe String, [(Int, Expr PDDLAtom)])] -> [(Maybe String, [(Int, Expr PDDLAtom)])]
        winnow [] tl = tl
        winnow ((tlname, _) : cl) tl =
            winnow cl $
            filter (not . (== Just tlname) . fst) tl

findLastTask :: (HasTaskLists m, HasTaskConstraints m) =>
    m -> Maybe (Int, Expr PDDLAtom)
findLastTask m =
    case findLastTasks m of 
        [t] -> Just t
        _ -> Nothing

findPrevTasks :: (HasTaskLists m, HasTaskConstraints m) =>
    m -> Int -> [(Int, Expr PDDLAtom)]
findPrevTasks m n =
    let
        tasks = numberTasks m
    in
    case tcontext tasks of
    Left t -> [t]
    Right Nothing -> []
    Right (Just tlname) -> 
        concatMap (\(prevname, _) -> map (last . snd) $ filter ((== Just prevname) . fst) tasks) $
        filter ((== tlname) . snd) $
        getTaskConstraints m
    where
        tcontext :: [(Maybe String, [(Int, Expr PDDLAtom)])] -> Either (Int, Expr PDDLAtom) (Maybe String)
        tcontext [] = Right Nothing
        tcontext ((_, []) : tl) = tcontext tl
        tcontext ((name, [(tn, _)]) : tl)
            | tn == n = Right name
            | otherwise = tcontext tl
        tcontext ((name, pnt@(pn, _) : tnt@(tn,_) : ttl) : tl)
            | pn == n = Right name
            | tn == n = Left pnt
            | otherwise = tcontext $ (name, tnt : ttl) : tl

findNextTasks :: (HasTaskLists m, HasTaskConstraints m) =>
    m -> Int -> [(Int, Expr PDDLAtom)]
findNextTasks m n =
    let
        tasks = numberTasks m
    in
    case tcontext tasks of
    Left t -> [t]
    Right Nothing -> []
    Right (Just tlname) -> 
        concatMap (\(_, nextname) -> map (head . snd) $ 
            filter (not . null . snd) $
            filter ((== Just nextname) . fst) tasks) $
        filter ((== tlname) . fst) $
        getTaskConstraints m
    where
        tcontext :: [(Maybe String, [(Int, Expr PDDLAtom)])] -> Either (Int, Expr PDDLAtom) (Maybe String)
        tcontext [] = Right Nothing
        tcontext ((_, []) : tl) = tcontext tl
        tcontext ((name, [(tn, _)]) : tl)
            | tn == n = Right name
            | otherwise = tcontext tl
        tcontext ((name, (pn, _) : tnt : ttl) : tl)
            | pn == n = Left tnt 
            | otherwise = tcontext $ (name, tnt : ttl) : tl

type StdTask = Expr (Atomic TermExpr)
type StdTaskHead = Maybe StdTask 
type StdTaskDef = Expr (Atomic TypedVarExpr)


data Method c e = Method
    Name
    (Parameters TypedVarExpr)
    (TaskHead StdTaskHead)
    (Precondition c)
    (Effect e)
    TaskLists
    TaskConstraints
    deriving (Data, Eq)
deriving instance Typeable2 Method


instance (Data c, Data e) => HasName (Method c e)
instance (Data c, Data e) => HasParameters TypedVarExpr (Method c e)
instance (Data c, Data e) => HasTaskHead StdTaskHead (Method c e)
instance (Data c, Data e) => HasPrecondition c (Method c e)
instance (Data c, Data e) => HasEffect e (Method c e)
instance (Data c, Data e) => HasTaskLists (Method c e)
instance (Data c, Data e) => HasTaskConstraints (Method c e)

defaultMethod :: forall c e. Method c e
defaultMethod = Method (Name "")
    (Parameters [])
    (TaskHead Nothing)
    (Precondition Nothing)
    (Effect Nothing)
    (TaskLists [])
    (TaskConstraints [])

instance (Data (Expr c), Data (Expr e), PDDLDocExpr c, PDDLDocExpr e) => PDDLDoc (Method (Expr c) (Expr e)) where
    pddlDoc m = parens $ sep  ([
        (if null (getTaskLists m) then text ":action" else text ":method") <+> text (getName m),
        text ":parameters" <+> parens (sep $ map pddlDoc $ getParameters m),
        docMaybe ":task" $ getTaskHead m,
        docMaybe ":precondition" $ getPrecondition m,
        docMaybe ":effect" $ getEffect m]
        ++ map tasklist (getTaskLists m)
        ++ [text ":constraints" 
            <+> parens (sep $ map tconstraint $ getTaskConstraints m)])
        where
            tasklist :: TaskList -> Doc
            tasklist (Just name, tl) = text ":tasks" <+> parens (sep $
                (text name) : map pddlDoc tl)
            tasklist (Nothing, tl) = text ":tasks" <+> parens (sep $
                map pddlDoc tl)
            tconstraint :: TaskConstraint -> Doc
            tconstraint (t1, t2) = parens $ text t1 <+> text t2
                
        

type StandardMethod = Method PreferenceGDExpr EffectDExpr


htnDescLanguage :: forall st. T.LanguageDef st
htnDescLanguage = pddlDescLanguage {
    T.reservedNames = T.reservedNames pddlDescLanguage ++
        [":method", ":task", ":tasks", ":branch"]
    }

htnDescLexer :: forall st. T.TokenParser st
htnDescLexer = T.makeTokenParser htnDescLanguage

htnParser :: GenParser Char (StandardHTNDomain) (StandardHTNDomain)
htnParser = let 
        condParser = prefGDParser pddlExprLexer 
        effParser = effectDParser pddlExprLexer
        constraintP = constraintGDParser pddlExprLexer
        actions =
            (methodParser htnDescLexer condParser effParser :: CharParser StandardHTNDomain ())
    in
    domainParser htnDescLexer (hDomainInfoParser htnDescLexer constraintP) actions

htnProblemParser :: GenParser Char StandardHTNProblem StandardHTNProblem
htnProblemParser = 
    let
        stateP = T.parens pddlExprLexer $ initLiteralParser pddlExprLexer :: CharParser StandardHTNProblem InitLiteralExpr 
        goalP = prefGDParser pddlExprLexer :: CharParser StandardHTNProblem PreferenceGDExpr
        constraintP = constraintGDParser pddlExprLexer :: CharParser StandardHTNProblem ConstraintGDExpr
        taskP =  do
            try $ T.reserved htnDescLexer ":task"
            task <- T.parens pddlExprLexer (atomicParser pddlExprLexer (constTermParser pddlExprLexer))
            updateState (setTaskHead $ Just task)
        infoP = taskP <|> problemInfoParser htnDescLexer stateP goalP constraintP
    in
    problemParser htnDescLexer infoP

hDomainInfoParser :: (HasRequirements st,
        HasTypes TypedConstExpr st,
        HasConstants TypedConstExpr st,
        HasConstraints a st,
        HasTaskHead [StdTaskDef] st,
        Atomic TypedVarExpr :<: f,
        HasPredicates (Expr f) st) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st ()
hDomainInfoParser dlex condParser =
    (do
        try $ T.reserved dlex ":tasks"
        tasks <- many $ T.parens dlex (atomicParser dlex (parseTypedVar dlex))
        updateState (setTaskHead tasks))
    <|>
    domainInfoParser dlex condParser


methodParser :: forall st p e .
    (HasActions (Method p e) st,
    Data p, Data e) => 
    T.TokenParser st -> 
    CharParser st p -> 
    CharParser st e ->
    CharParser st ()
methodParser mylex condParser effParser = do
    let 
        infoParser :: CharParser st (Method p e -> Method p e)
        infoParser = methodInfoParser mylex condParser effParser
    T.reserved mylex ":method" <|> T.reserved mylex ":action"
    name <- T.identifier mylex
    updates <- many infoParser
    let method = foldl (\a t -> t a) (setName name defaultMethod) updates
    updateState (\d -> setActions (method : getActions d) d)

methodInfoParser :: (HasParameters TypedVarExpr a,
    HasPrecondition b a,
    HasEffect e a,
    HasTaskHead (Maybe (Expr f)) a,
    HasTaskLists a,
    HasTaskConstraints a,
    Atomic TermExpr :<: f) =>
    T.TokenParser st -> CharParser st b -> CharParser st e -> CharParser st (a -> a)
methodInfoParser mylex condParser effParser =
    paramParser mylex
    <|>
    precondParser mylex condParser
    <|>
    effectParser mylex effParser
    <|>
    taskHeadParser mylex
    <|>
    taskListParser mylex
    <|>
    taskConstraintParser mylex
    

taskHeadParser :: (Atomic TermExpr :<: f,
        HasTaskHead (Maybe (Expr f)) a) =>
    T.TokenParser st -> CharParser st (a -> a)

taskHeadParser mylex = do
    try $ T.reserved mylex ":task"
    task <- maybeParser mylex $ taskParser mylex -- atomicParser mylex (termParser mylex)
    return $ setTaskHead task
    
taskListParser :: HasTaskLists a =>
    T.TokenParser st -> CharParser st (a -> a)
taskListParser mylex = do
    try $ T.reserved mylex ":tasks"
    T.parens mylex $ do
        name <- optionMaybe (try $ T.identifier mylex)
        tasks <- many $ T.parens mylex $ taskParser mylex
        return $ \m -> setTaskLists ((name, tasks) : getTaskLists m) m
        
taskConstraintParser :: HasTaskConstraints a =>
    T.TokenParser st -> CharParser st (a -> a)
taskConstraintParser mylex = do
    try $ T.reserved mylex ":constraints"
    T.parens mylex $ do
        orderings <- many $ T.parens mylex $ do
            t1 <- T.identifier mylex
            t2 <- T.identifier mylex
            return (t1, t2)
        return $ \m -> setTaskConstraints 
            (orderings ++ getTaskConstraints m) m


taskParser :: (:<:) (Atomic TermExpr) f => 
    T.TokenParser st -> CharParser st (Expr f)
taskParser mylex = do
    name <- T.identifier mylex
    terms <- many $ termParser mylex
    return $ eAtomic name terms

parseHTNPDDL :: SourceName -> String -> Either ParseError StandardHTNDomain
parseHTNPDDL source input =
    runParser htnParser emptyHDomain source input

parseHTNProblem :: SourceName -> String -> Either ParseError StandardHTNProblem
parseHTNProblem source input =
    runParser htnProblemParser emptyHProblem source input

   
