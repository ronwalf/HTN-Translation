{-# OPTIONS_GHC
    -freduction-depth=30
  #-}
{-# LANGUAGE
    DeriveDataTypeable,
    FlexibleContexts,
    FlexibleInstances,
    FunctionalDependencies,
    MultiParamTypeClasses,
    OverloadedStrings,
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
    TaskDef, TaskConstraint, TaskOrdering,
    HasTaskList(..), HasTaskOrdering(..),
    EqualityConstraintExpr,
    taskName, taskArgs, StdTask, StdTaskHead, StdTaskDef,
    enumerateTasks, numberTasks, listTaskNames,
    findFirstTask, findLastTask, findLastTasks, findNextTasks, findPrevTasks,
    parseHTNPDDL, parseHTNProblem,
    htnDescLexer, taskOrderingParser, taskListParser, orderedTaskListParser
) where

import Data.List
import Data.Generics (Data, Typeable)
import Data.Maybe
import Data.Text (Text, cons, pack)
import Data.Text.Prettyprint.Doc
import Text.ParserCombinators.Parsec hiding (space)
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL.PDDL3_0
import Planning.PDDL.Parser

prettyT :: Text -> Doc ann
prettyT = pretty

data HDomain a b g = HDomain
    Name
    Requirements
    (Types TypedTypeExpr)
    (Constants TypedConstExpr)
    (Predicates TypedPredicateExpr)
    (TaskHead [StdTaskDef])
    (Functions TypedFuncSkelExpr)
    (Constraints a)
    (Derived (TypedPredicateExpr, g))
    (Actions b)
    deriving (Data, Eq, Typeable)

instance (Data a, Data b, Data g) => HasName (HDomain a b g)
instance (Data a, Data b, Data g) => HasRequirements (HDomain a b g)
instance (Data a, Data b, Data g) => HasTypes TypedTypeExpr (HDomain a b g)
instance (Data a, Data b, Data g) => HasConstants TypedConstExpr (HDomain a b g)
instance (Data a, Data b, Data g) => HasPredicates TypedPredicateExpr (HDomain a b g)
instance (Data a, Data b, Data g) => HasTaskHead [StdTaskDef] (HDomain a b g)
instance (Data a, Data b, Data g) => HasFunctions TypedFuncSkelExpr (HDomain a b g)
instance (Data a, Data b, Data g) => HasConstraints a (HDomain a b g)
instance (Data a, Data b, Data g) => HasDerived (TypedPredicateExpr, g) (HDomain a b g)
instance (Data a, Data b, Data g) => HasActions b (HDomain a b g)
instance (Data a, Data b, Data g, PDDLDoc a, PDDLDoc b, PDDLDoc g) =>
    PDDLDoc (HDomain a b g) where
    pddlDoc domain = parens $ align $ vcat $
        prettyT "define" :
        parens (prettyT "domain" <+> prettyT (getName domain)) :
         -- Requirement strings are prefixed with ':'
        (if null $ getRequirements domain then mempty else
            parens (sep $
                map (prettyT . cons ':') $
                "requirements" : getRequirements domain)) :
        docList (parens . sep . (prettyT ":types" :) . (:[]) . pddlDoc) (getTypes domain) :
        docList (parens . sep . (prettyT ":predicates" :) . map pddlDoc) (getPredicates domain) :
        docList (parens . sep . (prettyT ":tasks" :) . map pddlDoc) (getTaskHead domain) :
        space :
        intersperse space
            (map (\(p,b) ->
                parens $ sep
                  [ prettyT ":derived"
                  , pddlDoc p
                  , pddlDoc b ])
                (getDerived domain)
            ++ map (\t -> 
                parens $ sep [
                    prettyT ":task" <+> pretty (taskName t),
                    prettyT ":parameters" <+> parens (pddlDoc $ taskArgs t)])
                (getTaskHead domain)
            ++ map pddlDoc (getActions domain))

instance (Data a, Data b, Data g, PDDLDoc a, PDDLDoc b, PDDLDoc g) => Show (HDomain a b g) where
    show domain = show $ pddlDoc domain

emptyHDomain :: forall a b g. HDomain a b g
emptyHDomain = HDomain
    (Name "empty")
    (Requirements [])
    (Types [])
    (Constants [])
    (Predicates [])
    (TaskHead [])
    (Functions [])
    (Constraints [])
    (Derived [])
    (Actions [])


data HProblem a b c t = HProblem
    Name
    DomainName
    Requirements
    (Constants TypedConstExpr)
    (Initial a)
    (Parameters TypedVarExpr)
    (TaskList t)
    TaskOrdering
    (Goal b)
    (Constraints [c])
    deriving (Data, Eq, Typeable)


instance (Data a, Data b, Data c, Data t) => HasName (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasDomainName (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasRequirements (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasConstants TypedConstExpr (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasInitial a (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasTaskList t (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasParameters TypedVarExpr (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasTaskOrdering (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasGoal b (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasConstraints c (HProblem a b c t)

--instance
--    (Data (Expr a), Data (Expr b), Data (Expr c),
--     PDDLDocExpr a, PDDLDocExpr b, PDDLDocExpr c) =>
--    Show (HProblem (Expr a) (Expr b) (Expr c)) where
-- instance (Data a, Data b, Data c, Data t,
--         PDDLDoc a, PDDLDoc b, PDDLDoc c, PDDLDocExpr (Atomic t)) =>
--         PDDLDoc (HProblem a b c t) where
--     pddlDoc _ = parens $ sep [
--         prettyT "define",
--         parens (prettyT "problem" <+> prettyT (getName prob)),
--         parens (prettyT ":domain" <+> prettyT (getDomainName prob)),
--         if null $ getRequirements prob then mempty else
--            parens (sep $ prettyT ":requirements" : map (prettyT . cons ':') (getRequirements prob)),
--         docList (parens . sep . (prettyT ":objects" :) . (:[]) . pddlDoc) (getConstants prob),
--         parens ( sep [
--             prettyT ":htn",
--             prettyT ":parameters", -- TODO
--             docMaybe (parens . (prettyT ":constraints" <+>) . pddlDoc) (getConstraints prob),
--             prettyT ":tasks", docAnd $ getTaskList prob,
--             prettyT ":ordering", docAnd taskDoc $ getTaskOrdering prob]),
--         docList (parens . sep . (prettyT ":init" :) . map pddlDoc) (getInitial prob),
--         docMaybe (parens . sep . (prettyT ":goal" :) . (:[]) . pddlDoc) (getGoal prob)
--         ]
        


emptyHProblem :: forall a b c t. HProblem a b c t
emptyHProblem = HProblem
    (Name "empty")
    (DomainName "empty")
    (Requirements [])
    (Constants [])
    (Initial [])
    (Parameters [])
    (TaskList [])
    (TaskOrdering [])
    (Goal Nothing)
    (Constraints [])


type EqualityConstraint = PDDLAtom :+: Not -- More permissive than equality, but should allow for '='
type EqualityConstraintExpr = Expr EqualityConstraint
type StandardHTNDomain = HDomain EqualityConstraintExpr StandardMethod GDExpr
type StandardHTNProblem = HProblem InitLiteralExpr PreferenceGDExpr EqualityConstraintExpr TermExpr


newtype TaskHead f = TaskHead f deriving (Data, Eq, Typeable)
unTaskHead :: TaskHead t -> t
unTaskHead (TaskHead h) = h
class (Data a, Data f) => HasTaskHead f a | a -> f where
    getTaskHead :: a -> f
    getTaskHead = unTaskHead . fromJust . gfind
    setTaskHead :: f -> a -> a
    setTaskHead h r = fromJust $ greplace r (TaskHead h)


type TaskDef a = (Maybe Text, Expr (Atomic a))
type NumberedTaskDef a = (Int, TaskDef a)
taskName :: Expr (Atomic t) -> Text
taskName (In (Atomic p _)) = p
taskArgs :: Expr (Atomic t) -> [t]
taskArgs (In (Atomic _ al)) = al

{-
taskDoc :: PDDLDoc p => (Maybe Text, p) -> Doc ann
taskDoc (Nothing, p) = pddlDoc p
taskDoc (Just l, p) = parens $ sep [prettyT l, pddlDoc p]
-}

newtype TaskList a = TaskList [TaskDef a] deriving (Data, Eq, Typeable)
unTaskList :: TaskList a -> [TaskDef a]
unTaskList (TaskList tl) = tl
class (Data a, Data f) => HasTaskList a f | f -> a where
    getTaskList :: f -> [TaskDef a]
    getTaskList = unTaskList . fromJust . gfind
    setTaskList :: [TaskDef a] -> f -> f
    setTaskList tl f = fromJust $ greplace f (TaskList tl)
--tasklist :: (PDDLDocExpr (Atomic a)) => [TaskDef a] -> Doc ann
--tasklist (Just name, tl) = prettyT ":tasks" <+> parens (sep $
--    (prettyT name) : map pddlDoc tl)
--tasklist (Nothing, tl) = prettyT ":tasks" <+> parens (sep $
--    map pddlDoc tl)

numberTasks :: HasTaskList a m => m -> [NumberedTaskDef a]
numberTasks = zip [0..] . getTaskList

enumerateTasks :: HasTaskList a m => m -> [(Int, Expr (Atomic a))]
enumerateTasks = map (\(n, (_, t)) -> (n, t)) . numberTasks

listTaskNames :: HasTaskList a m => m -> [Text]
listTaskNames = nub . map (taskName . snd) . getTaskList


findTaskList :: HasTaskList a m => m -> Text -> [TaskDef a]
findTaskList m name =
    filter ((== Just (name)) . fst) $
    getTaskList m

--findPrecursor :: (HasTaskList m, HasTaskOrdering m) =>
--    Int -> m -> [

type TaskConstraint = (Text, Text)
newtype TaskOrdering = TaskOrdering [TaskConstraint] deriving (Data, Eq, Typeable)
unTaskOrdering :: TaskOrdering -> [TaskConstraint]
unTaskOrdering (TaskOrdering tc) = tc
class (Data f) => HasTaskOrdering f where
    getTaskOrdering :: f -> [TaskConstraint]
    getTaskOrdering = unTaskOrdering . fromJust . gfind
    setTaskOrdering :: [TaskConstraint] -> f -> f
    setTaskOrdering tc f = fromJust $ greplace f (TaskOrdering tc)

-- tconstraint :: TaskConstraint -> Doc ann
-- tconstraint (t1, t2) = parens $ prettyT "<" <+> pretty t1 <+> pretty t2

deleteAll :: Eq c => c -> [c] -> [c]
deleteAll c = filter (/= c)

normalizedTaskOrdering :: (HasTaskList a m, HasTaskOrdering m) =>
    m -> [TaskConstraint]
normalizedTaskOrdering m =
    let
        constraints :: [TaskConstraint]
        constraints = getTaskOrdering m
        names :: [Text]
        names = nub $
            map fst constraints
            ++ map snd constraints
        used :: [Text]
        used = filter (any (not . null) . findTaskList m) names
        unused :: [Text]
        unused = names \\ used
    in
    expand unused constraints
    where
        expand :: [Text] -> [TaskConstraint] -> [TaskConstraint]
        expand [] tc = tc
        expand (u : ul) tc =
            expand ul $ -- Continue expansion
            replace u $ -- Replace left and right occurances
            deleteAll (u,u) tc -- Remove self constraints (OK since empty)
        replace :: Text -> [TaskConstraint] -> [TaskConstraint]
        replace u tc =
            let
                l = map fst $ filter ((== u) . snd) tc
                r = map snd $ filter ((== u) . fst) tc
            in
            [(c1,c2) | c1 <- l, c2 <- r] -- Cross product of constraints
            ++ filter (\(c1,c2) -> c1 == u || c2 == u) tc



findFirstTask :: (HasTaskList a m, HasTaskOrdering m) =>
    m -> Maybe (Int, Expr (Atomic a))
findFirstTask m =
    case winnow (normalizedTaskOrdering m) (numberTasks m) of
        [(n, (_, t))] -> Just (n, t)
        _ -> Nothing
    where
        winnow [] tl = tl
        winnow ((_, tlname) : cl) tl =
            winnow cl $
            filter ((/= Just tlname) . fst . snd) tl

findLastTasks :: forall a m . (HasTaskList a m, HasTaskOrdering m) =>
    m -> [(Int, Expr (Atomic a))]
findLastTasks m =
    map (\(n, (_, t)) -> (n, t)) $
    winnow (normalizedTaskOrdering m) (numberTasks m)
    where
        winnow :: [(Text, Text)] -> [NumberedTaskDef a] -> [NumberedTaskDef a]
        winnow [] tl = tl
        winnow ((tlname, _) : cl) tl =
            winnow cl $
            filter ((/= Just tlname) . fst . snd) tl

findLastTask :: (HasTaskList a m, HasTaskOrdering m) =>
    m -> Maybe (Int, Expr (Atomic a))
findLastTask m =
    case findLastTasks m of
        [t] -> Just t
        _ -> Nothing

findPrevTasks :: forall a m . (HasTaskList a m, HasTaskOrdering m) =>
    m -> Int -> [(Int, Expr (Atomic a))]
findPrevTasks m n =
    let
        tasks = numberTasks m
        -- Find the task name associated with the task (if any)
        tlname :: Maybe Text
        tlname = fst $ snd $ head $ filter ((== n) . fst) tasks
        prevTaskIds :: [Maybe Text]
        prevTaskIds =
            map (Just . fst) $
            filter ((== tlname) . Just . snd) $
            normalizedTaskOrdering m
        prevTasks = filter (flip elem prevTaskIds . fst . snd) tasks
    in
    map (\(i, (_, t)) -> (i, t)) prevTasks
    
    

findNextTasks :: (HasTaskList a m, HasTaskOrdering m) =>
    m -> Int -> [(Int, Expr (Atomic a))]
findNextTasks m n =
    let
        tasks = numberTasks m
        tlname :: Maybe Text
        tlname = fst $ snd $ head $ filter ((== n) . fst) tasks
        nextTaskIds :: [Maybe Text]
        nextTaskIds =
            map (Just . snd) $
            filter ((== tlname) . Just . fst) $
            normalizedTaskOrdering m
        nextTasks = filter (flip elem nextTaskIds . fst . snd) tasks
    in
    map (\(i, (_, t)) -> (i, t)) nextTasks

type StdTask = Expr PDDLAtom
type StdTaskHead = Maybe StdTask
type StdTaskDef = Expr (Atomic TypedVarExpr)


data Method c t e = Method
    Name
    (Parameters TypedVarExpr)
    (TaskHead StdTaskHead)
    (Precondition c)
    (Effect e)
    (TaskList TermExpr)
    TaskOrdering
    (Constraints t)
    deriving (Data, Eq)
deriving instance Typeable Method


instance (Data c, Data t, Data e) => HasName (Method c t e)
instance (Data c, Data t, Data e) => HasParameters TypedVarExpr (Method c t e)
instance (Data c, Data t, Data e) => HasTaskHead StdTaskHead (Method c t e)
instance (Data c, Data t, Data e) => HasPrecondition c (Method c t e)
instance (Data c, Data t, Data e) => HasEffect e (Method c t e)
instance (Data c, Data t, Data e) => HasTaskList TermExpr (Method c t e)
instance (Data c, Data t, Data e) => HasTaskOrdering (Method c t e)
instance (Data c, Data t, Data e) => HasConstraints t (Method c t e)

defaultMethod :: forall c t e. Method c t e
defaultMethod = Method (Name "")
    (Parameters [])
    (TaskHead Nothing)
    (Precondition [])
    (Effect [])
    (TaskList [])
    (TaskOrdering [])
    (Constraints [])

instance (Data p, Data ct, Data t, Data ep, Data e, PDDLDoc p, PDDLDoc ct, PDDLDoc [t], PDDLDoc ep, PDDLDoc e)
    => PDDLDoc (Method (Maybe Text, p) ct ([t], Maybe ep, [e])) where
    pddlDoc _ = parens $ sep  [
        {-
        (if null (getTaskList m) then prettyT ":action" else prettyT ":method") <+> prettyT (getName m),
        prettyT ":parameters" <+> parens (pddlDoc $ getParameters m),
        docMaybe ((prettyT ":task" <+>) . pddlDoc) (getTaskHead m),
        docList ((prettyT ":precondition" <+>) . docAnd prefDoc) $ getPrecondition m,
        docList ((prettyT ":effect" <+>) . docAnd id . concatMap effectDoc) $ getEffect m,
        docList ((prettyT ":tasks" <+>) . docAnd taskDoc) (getTaskList m),
        docList ((prettyT ":ordering" <+>) . docAnd tconstraint) (getTaskOrdering m),
        docList ((prettyT ":constraints" <+>) . docAnd pddlDoc) (getConstraints m) ]
        where
            prefDoc :: (Maybe Text, p) -> Doc ann
            prefDoc (Nothing, p) = pddlDoc p
            prefDoc (Just n, p) = parens $ sep [
                prettyT "preference",
                prettyT n,
                pddlDoc p ]
            effectDoc :: ([t], Maybe ep, [e]) -> [Doc ann]
            effectDoc ([], ep, el) = condDoc ep el
            effectDoc (tl, ep, el) = [parens $ sep [
                prettyT "forall",
                parens (pddlDoc tl),
                docAnd id $ condDoc ep el]]
            condDoc :: Maybe ep -> [e] -> [Doc ann]
            condDoc Nothing el = map pddlDoc el
            condDoc (Just ep) el = [parens $ sep [
                prettyT "when",
                pddlDoc ep,
                docAnd pddlDoc el ]]
            -}
            ]



type StandardMethod = Method PDDLPrecond EqualityConstraintExpr PDDLEffect

-- Don't need to reserve names that are in syntacticaly distinct locations from identifiers
htnDescLanguage :: forall st. T.LanguageDef st 
htnDescLanguage = pddlDescLanguage {
    T.reservedNames = T.reservedNames pddlDescLanguage 
    }

htnDescLexer :: forall st. T.TokenParser st
htnDescLexer = T.makeTokenParser htnDescLanguage

htnParser :: CharParser StandardHTNDomain StandardHTNDomain
htnParser = let
        constraintP = effectDParser pddlExprLexer :: CharParser StandardHTNDomain EqualityConstraintExpr -- Overly broad (accepts more predicates than '=')
        prefP = prefListParser pddlExprLexer (gdParser pddlExprLexer :: CharParser StandardHTNDomain GDExpr)
        effectP = ucEffectParser pddlExprLexer
            (gdParser pddlExprLexer :: CharParser StandardHTNDomain GDExpr)
            (effectDParser pddlExprLexer :: CharParser StandardHTNDomain EffectDExpr)
    in
    domainParser htnDescLexer $
        hDomainInfoParser htnDescLexer (effectDParser pddlExprLexer)
        <|>
        derivedParser pddlDescLexer
          (atomicTypeParser pddlExprLexer (varParser pddlExprLexer) :: CharParser st TypedPredicateExpr)
          (gdParser pddlExprLexer)
        <|>
        taskDescParser pddlDescLexer
        <|>
        methodParser htnDescLexer prefP constraintP effectP


htnProblemParser :: CharParser StandardHTNProblem StandardHTNProblem
htnProblemParser =
    let
        stateP = T.parens pddlExprLexer $ initLiteralParser pddlExprLexer :: CharParser StandardHTNProblem InitLiteralExpr
        goalP = prefGDParser pddlExprLexer :: CharParser StandardHTNProblem PreferenceGDExpr
        constraintP = effectDParser pddlExprLexer :: CharParser StandardHTNProblem EqualityConstraintExpr -- Technically shouldn't be used
        infoP = htnBlockParser <|> problemInfoParser htnDescLexer stateP goalP constraintP
    in
    problemParser htnDescLexer infoP
    where
        htnBlockParser = do
            try $ T.reserved htnDescLexer ":htn"
            skipMany $ T.parens htnDescLexer htnInfoParser
        htnInfoParser = 
            (paramParser pddlExprLexer >>= updateState)
            <|> (taskListParser htnDescLexer (termParser pddlExprLexer :: CharParser StandardHTNProblem TermExpr) >>= updateState)
            <|> (taskOrderingParser htnDescLexer >>= updateState)
            <|> (orderedTaskListParser htnDescLexer (termParser pddlExprLexer) >>= updateState)
           


hDomainInfoParser :: (HasRequirements st,
        HasTypes TypedTypeExpr st,
        HasConstants TypedConstExpr st,
        HasConstraints a st,
        HasTaskHead [StdTaskDef] st,
        HasPredicates TypedPredicateExpr st) =>
    T.TokenParser st
    -> CharParser st a
    -> CharParser st ()
hDomainInfoParser dlex condParser =
    (do
        try $ T.reserved dlex ":tasks"
        tasks <- many $ T.parens dlex (atomicTypeParser dlex (varParser dlex))
        updateState (\d -> setTaskHead (tasks ++ getTaskHead d) d))
    <|>
    domainInfoParser dlex condParser
    
taskDescParser :: (HasTaskHead [StdTaskDef] st) =>
    T.TokenParser st
    -> CharParser st ()
taskDescParser dlex = do
    T.reserved dlex ":task"
    name <- T.identifier dlex
    T.reserved dlex ":parameters"
    params <- T.parens dlex $ parseTypedList dlex $ varParser dlex
    let task :: StdTaskDef = eAtomic (pack name) (params :: [TypedVarExpr])
    updateState (\d -> setTaskHead (task : getTaskHead d) d)

methodParser :: forall st p c e .
    (HasActions (Method p c e) st,
    Data p, Data c, Data e) =>
    T.TokenParser st ->
    CharParser st [p] ->
    CharParser st c ->
    CharParser st [e] ->
    CharParser st ()
methodParser mylex condParser constraintParser effParser = do
    let
        infoParser :: CharParser st (Method p c e -> Method p c e)
        infoParser = methodInfoParser mylex condParser constraintParser effParser
    T.reserved mylex ":method" <|> T.reserved mylex ":action"
    name <- T.identifier mylex
    updates <- many infoParser
    let method = foldl (\a t -> t a) (setName (pack name) defaultMethod) updates
    updateState (\d -> setActions (method : getActions d) d)

methodInfoParser :: (HasParameters TypedVarExpr a,
    HasPrecondition b a,
    HasConstraints c a,
    HasEffect e a,
    HasTaskHead (Maybe (Expr PDDLAtom)) a,
    HasTaskList TermExpr a,
    HasTaskOrdering a) =>
    T.TokenParser st -> CharParser st [b] -> CharParser st c -> CharParser st [e] -> CharParser st (a -> a)
methodInfoParser mylex condParser constraintParser effParser =
    paramParser mylex
    <|>
    precondParser mylex condParser
    <|>
    effectParser mylex effParser
    <|>
    taskHeadParser mylex
    <|>
    taskListParser mylex (termParser mylex)
    <|>
    taskOrderingParser mylex
    <|>
    orderedTaskListParser mylex (termParser mylex)
    <|>
    taskConstraintParser mylex constraintParser

taskHeadParser :: HasTaskHead (Maybe (Expr PDDLAtom)) a =>
    T.TokenParser st -> CharParser st (a -> a)
taskHeadParser mylex = do
    try $ T.reserved mylex ":task"
    task <- maybeParser mylex $ taskParser mylex (termParser mylex)
    return $ setTaskHead task



taskListParser :: HasTaskList a m =>
    T.TokenParser st -> CharParser st a -> CharParser st (m -> m)
taskListParser mylex argParser = do
    try (T.reserved mylex ":tasks" <|> T.reserved mylex ":subtasks")
    tasks <- T.parens mylex $ emptyAndListParser mylex $ namedItemParser mylex $ taskParser mylex argParser
    return $ \m -> setTaskList (tasks ++ getTaskList m) m

taskOrderingParser :: HasTaskOrdering a =>
    T.TokenParser st -> CharParser st (a -> a)
taskOrderingParser mylex = do
    try (T.reserved mylex ":order" <|> T.reserved mylex ":ordering")
    order <- T.parens mylex $ emptyAndListParser mylex orderParser
    return $ \m -> setTaskOrdering (order ++ getTaskOrdering m) m
    where
        orderParser = do
            T.reserved mylex "<"
            t1 <- T.identifier mylex
            t2 <- T.identifier mylex
            return (pack t1, pack t2)

orderedTaskListParser  :: (HasTaskList a m, HasTaskOrdering m) =>
    T.TokenParser st -> CharParser st a -> CharParser st (m -> m)
orderedTaskListParser mylex argParser = do
    try (T.reserved mylex ":ordered-subtasks" <|> T.reserved mylex ":ordered-tasks")
    tasks <- T.parens mylex $ emptyAndListParser mylex $ namedItemParser mylex $ taskParser mylex argParser
    let namedTasks = [(Just $ fromMaybe (pack $ 't' : show i) n, t) | (i :: Int, (n, t)) <- zip [0..] tasks]
    let names = map (fromJust . fst) namedTasks
    let ordering = zip names (tail names)
    return $ \m -> setTaskList (namedTasks ++ getTaskList m) $ setTaskOrdering (ordering ++ getTaskOrdering m) m



taskConstraintParser :: HasConstraints c a =>
    T.TokenParser st -> CharParser st c -> CharParser st (a -> a)
taskConstraintParser mylex constraintParser = do
    try $ T.reserved mylex ":constraints"
    constraints :: [c] <- T.parens mylex $ emptyAndListParser mylex constraintParser
    return $ \m -> setConstraints (constraints ++ getConstraints m) m

{-|
If 'p' parses expressions of the form <p> and returns a value,
'namedItemParser' parses expressions of the form:

* ID (<p>)
* <p>
-}
namedItemParser :: T.TokenParser st -> CharParser st a -> CharParser st (Maybe Text, a)
namedItemParser mylex itemParser =
    try (namedParser <|> bareParser)
    where
        namedParser = do
            name <- T.identifier mylex
            item <- T.parens mylex itemParser
            return (Just $ pack name, item)
        bareParser = do
            item <- itemParser
            return (Nothing, item)


taskParser :: T.TokenParser st -> CharParser st a -> CharParser st (Expr (Atomic a))
taskParser mylex argParser = do
    name <- T.identifier mylex
    terms <- many argParser
    return $ eAtomic (pack name) terms

parseHTNPDDL :: SourceName -> String -> Either ParseError StandardHTNDomain
parseHTNPDDL = runParser htnParser emptyHDomain

parseHTNProblem :: SourceName -> String -> Either ParseError StandardHTNProblem
parseHTNProblem = runParser htnProblemParser emptyHProblem
