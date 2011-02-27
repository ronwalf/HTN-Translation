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
    Method(..), StandardMethod,
    TaskHead(..), HasTaskHead, getTaskHead, setTaskHead,
    TaskList, TaskLists, TaskConstraint, TaskConstraints,
    HasTaskLists(..), HasTaskConstraints,
    taskName, taskArgs, StdTask, StdTaskHead, StdTaskDef,
    enumerateTasks, numberTasks,
    findFirstTask, findLastTask,
    parseHTNPDDL
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
--deriving instance Data (Expr (DomainItem StandardHAction :+: DomainItem StandardMethod))


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

findLastTask :: (HasTaskLists m, HasTaskConstraints m) =>
    m -> Maybe (Int, Expr PDDLAtom)
findLastTask m =
    case winnow (getTaskConstraints m) (numberTasks m) of
        [(_, tl@(_ : _))] -> Just $ last tl
        _ -> Nothing
    where
        winnow [] tl = tl
        winnow ((tlname, _) : cl) tl =
            winnow cl $
            filter (not . (== Just tlname) . fst) tl            
            

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
defaultMethod = Method undefined 
    (Parameters [])
    undefined
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


htnLanguage :: forall st. T.LanguageDef st
htnLanguage = pddlLanguage {
    T.reservedNames = T.reservedNames pddlLanguage ++
        [":method", ":task", ":tasks", ":branch"]
    }

htnLexer :: forall st. T.TokenParser st
htnLexer = T.makeTokenParser htnLanguage

htnParser :: GenParser Char (StandardHTNDomain) (StandardHTNDomain)
htnParser = let 
        mylex = htnLexer 
        condParser = prefGDParser mylex 
        effParser = effectDParser mylex 
        constraintP = constraintGDParser mylex
        actions =
            (methodParser mylex condParser effParser :: CharParser StandardHTNDomain ())
    in
    domainParser mylex (hDomainInfoParser mylex constraintP) actions

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
hDomainInfoParser mylex condParser =
    (do
        try $ T.reserved mylex ":tasks"
        tasks <- many $ T.parens mylex (atomicParser mylex (parseTypedVar mylex))
        updateState (setTaskHead tasks))
    <|>
    domainInfoParser mylex condParser

{-
hActionParser :: (Data p, Data e, HasActions (Expr f) st,
    DomainItem (HAction p e) :<: f) =>
    T.TokenParser st -> CharParser st p -> CharParser st e -> CharParser st ()
hActionParser mylex condParser effParser = do
    let infoParser = hActionInfoParser mylex condParser effParser
    try $ T.reserved mylex ":action"
    name <- T.identifier mylex
    updates <- many infoParser
    let action = foldl (\ a t -> t a) (setName name defaultHAction) updates
    updateState (\d -> setActions (domainItem action : getActions d) d)

hActionInfoParser :: (HasParameters TypedVarExpr a,
    HasTaskHead (Maybe (Expr f)) a,
    HasPrecondition b a,
    HasEffect c a,
    Atomic TermExpr :<: f) =>
    T.TokenParser st -> CharParser st b -> CharParser st c -> CharParser st (a -> a)
hActionInfoParser mylex condParser effParser =
    paramParser mylex
    <|>
    taskHeadParser mylex
    <|>
    precondParser mylex condParser
    <|>
    effectParser mylex effParser
-}
    

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
    
--    <|>
--    (do
--        try $ T.reserved mylex ":branches"
--        bl <- T.parens mylex $ many $ branchParser mylex condParser
--        return $ setBranches bl)

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
