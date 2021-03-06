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
    TaskList, TaskLists, TaskConstraint, TaskConstraints,
    HasTaskLists(..), HasTaskConstraints(..),
    taskName, taskArgs, StdTask, StdTaskHead, StdTaskDef,
    enumerateTasks, numberTasks, listTaskNames,
    findFirstTask, findLastTask, findLastTasks, findNextTasks, findPrevTasks,
    parseHTNPDDL, parseHTNProblem,
    htnDescLexer, taskConstraintParser, taskListParser,
) where

import Control.Monad.State
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
        (if (null $ getRequirements domain) then mempty else parens
            (sep $
             map (prettyT . (cons ':')) $
             "requirements" : getRequirements domain)) :
        docList (parens . sep . (prettyT ":types" :) . (:[]) . pddlDoc) (getTypes domain) :
        docList (parens . sep . (prettyT ":predicates" :) . map pddlDoc) (getPredicates domain) :
        docList (parens . sep . (prettyT ":tasks" :) . map pddlDoc) (getTaskHead domain) :
        space :
        intersperse space (
        (flip map (getDerived domain) (\(p,b) ->
            parens $ sep $
              [ prettyT ":derived"
              , pddlDoc p
              , pddlDoc b ]))
          ++ (map pddlDoc $ getActions domain))

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
    (Constraints Nothing)
    (Derived [])
    (Actions [])


type StandardHTNDomain = HDomain ConstraintGDExpr StandardMethod GDExpr
type StandardHTNProblem = HProblem InitLiteralExpr PreferenceGDExpr ConstraintGDExpr ConstTermExpr

--deriving instance Data (Expr (DomainItem StandardHAction :+: DomainItem StandardMethod))

data HProblem a b c t = HProblem
    Name
    DomainName
    Requirements
    (Constants TypedConstExpr)
    (Initial a)
    (TaskLists t)
    TaskConstraints
    (Goal b)
    (Constraints c)
    deriving (Data, Eq, Typeable)


instance (Data a, Data b, Data c, Data t) => HasName (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasDomainName (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasRequirements (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasConstants TypedConstExpr (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasInitial a (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasTaskLists t (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasTaskConstraints (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasGoal b (HProblem a b c t)
instance (Data a, Data b, Data c, Data t) => HasConstraints c (HProblem a b c t)

--instance
--    (Data (Expr a), Data (Expr b), Data (Expr c),
--     PDDLDocExpr a, PDDLDocExpr b, PDDLDocExpr c) =>
--    Show (HProblem (Expr a) (Expr b) (Expr c)) where
instance (Data a, Data b, Data c, Data t,
        PDDLDoc a, PDDLDoc b, PDDLDoc c, PDDLDocExpr (Atomic t)) =>
        PDDLDoc (HProblem a b c t) where
    pddlDoc prob = parens $ sep $
        prettyT "define" :
        (parens $ prettyT "problem" <+> (prettyT $ getName prob)) :
        (parens $ prettyT ":domain" <+> (prettyT $ getDomainName prob)) :
        (if null $ getRequirements prob then mempty else
           (parens $ sep $ prettyT ":requirements" : map (prettyT . (cons ':')) (getRequirements prob))) :
        docList (parens . sep . (prettyT ":objects" :) . (:[]) . pddlDoc) (getConstants prob) :
        docList (parens . sep . (prettyT ":init" :) . map pddlDoc) (getInitial prob) :
        docMaybe (parens . sep . (prettyT ":goal" :) . (:[]) . pddlDoc) (getGoal prob) :
        docMaybe (parens . sep . (prettyT ":constraints" :) . (:[]) . pddlDoc) (getConstraints prob) :
        map (parens . tasklist) (getTaskLists prob)
        ++ [docList (parens . (prettyT ":ordering" <+>) . parens . sep . map tconstraint) (getTaskConstraints prob)]


emptyHProblem :: forall a b c t. HProblem a b c t
emptyHProblem = HProblem
    (Name "empty")
    (DomainName "empty")
    (Requirements [])
    (Constants [])
    (Initial [])
    (TaskLists [])
    (TaskConstraints [])
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

type TaskList a = (Maybe Text, [Expr (Atomic a)])
type NumberedTaskList a = (Maybe Text, [(Int, Expr (Atomic a))])
taskName :: Expr (Atomic t) -> Text
taskName (In (Atomic p _)) = p
taskArgs :: Expr (Atomic t) -> [t]
taskArgs (In (Atomic _ al)) = al

data TaskLists a = TaskLists [TaskList a] deriving (Data, Eq, Typeable)
unTaskLists :: TaskLists a -> [TaskList a]
unTaskLists (TaskLists tl) = tl
class (Data a, Data f) => HasTaskLists a f | f -> a where
    getTaskLists :: f -> [TaskList a]
    getTaskLists = filter (not . null . snd) . unTaskLists . fromJust . gfind
    setTaskLists :: [TaskList a] -> f -> f
    setTaskLists tl f = fromJust $ greplace f (TaskLists tl)
tasklist :: (PDDLDocExpr (Atomic a)) => TaskList a -> Doc ann
tasklist (Just name, tl) = prettyT ":tasks" <+> parens (sep $
    (prettyT name) : map pddlDoc tl)
tasklist (Nothing, tl) = prettyT ":tasks" <+> parens (sep $
    map pddlDoc tl)


-- Courtesy of Saizan (freenode #haskell)
numberTasks :: HasTaskLists a m => m -> [NumberedTaskList a]
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

enumerateTasks :: HasTaskLists a m => m -> [(Int, Expr (Atomic a))]
enumerateTasks = concatMap snd . numberTasks

listTaskNames :: HasTaskLists a m => m -> [Text]
listTaskNames = nub . map (taskName . snd) . enumerateTasks


findTaskLists :: HasTaskLists a m => m -> Text -> [[(Int, Expr (Atomic a))]]
findTaskLists m name =
    map snd $
    filter ((== Just (name)) . fst) $
    numberTasks m

--findPrecursor :: (HasTaskLists m, HasTaskConstraints m) =>
--    Int -> m -> [

type TaskConstraint = (Text, Text)
data TaskConstraints = TaskConstraints [TaskConstraint] deriving (Data, Eq, Typeable)
unTaskConstraints :: TaskConstraints -> [TaskConstraint]
unTaskConstraints (TaskConstraints tc) = tc
class (Data f) => HasTaskConstraints f where
    getTaskConstraints :: f -> [TaskConstraint]
    getTaskConstraints = unTaskConstraints . fromJust . gfind
    setTaskConstraints :: [TaskConstraint] -> f -> f
    setTaskConstraints tc f = fromJust $ greplace f (TaskConstraints tc)

tconstraint :: TaskConstraint -> Doc ann
tconstraint (t1, t2) = parens $ pretty t1 <+> pretty t2

deleteAll :: Eq c => c -> [c] -> [c]
deleteAll c = filter (/= c)

normalizedTaskConstraints :: (HasTaskLists a m, HasTaskConstraints m) =>
    m -> [TaskConstraint]
normalizedTaskConstraints m =
    let
        constraints :: [TaskConstraint]
        constraints = getTaskConstraints m
        names :: [Text]
        names = nub $
            map fst constraints
            ++ map snd constraints
        used :: [Text]
        used = filter (\n -> or $ map (not . null) $ findTaskLists m n) names
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



findFirstTask :: (HasTaskLists a m, HasTaskConstraints m) =>
    m -> Maybe (Int, Expr (Atomic a))
findFirstTask m =
    case winnow (normalizedTaskConstraints m) (numberTasks m) of
        [(_, h : _)] -> Just h
        _ -> Nothing
    where
        winnow [] tl = tl
        winnow ((_, tlname) : cl) tl =
            winnow cl $
            filter (not . (== Just tlname) . fst) tl

findLastTasks :: forall a m . (HasTaskLists a m, HasTaskConstraints m) =>
    m -> [(Int, Expr (Atomic a))]
findLastTasks m =
    map (last . snd) $
    winnow (normalizedTaskConstraints m) (numberTasks m)
    where
        winnow :: [(Text, Text)] -> [(Maybe Text, [(Int, Expr (Atomic a))])] -> [(Maybe Text, [(Int, Expr (Atomic a))])]
        winnow [] tl = tl
        winnow ((tlname, _) : cl) tl =
            winnow cl $
            filter (not . (== Just tlname) . fst) tl

findLastTask :: (HasTaskLists a m, HasTaskConstraints m) =>
    m -> Maybe (Int, Expr (Atomic a))
findLastTask m =
    case findLastTasks m of
        [t] -> Just t
        _ -> Nothing

findPrevTasks :: forall a m . (HasTaskLists a m, HasTaskConstraints m) =>
    m -> Int -> [(Int, Expr (Atomic a))]
findPrevTasks m n =
    let
        tasks = numberTasks m
    in
    case tcontext tasks of
    Left t -> [t]
    Right Nothing -> []
    Right (Just tlname) ->
        concatMap (\(prevname, _) -> map (last . snd) $
            filter ((== Just prevname) . fst) tasks) $
        filter ((== tlname) . snd) $
        normalizedTaskConstraints m
    where
        tcontext :: [(Maybe Text, [(Int, Expr (Atomic a))])] -> Either (Int, Expr (Atomic a)) (Maybe Text)
        tcontext [] = Right Nothing
        tcontext ((_, []) : tl) = tcontext tl
        tcontext ((name, [(tn, _)]) : tl)
            | tn == n = Right name
            | otherwise = tcontext tl
        tcontext ((name, pnt@(pn, _) : tnt@(tn,_) : ttl) : tl)
            | pn == n = Right name
            | tn == n = Left pnt
            | otherwise = tcontext $ (name, tnt : ttl) : tl

findNextTasks :: (HasTaskLists a m, HasTaskConstraints m) =>
    m -> Int -> [(Int, Expr (Atomic a))]
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
        normalizedTaskConstraints m
    where
        -- Return the next task in the list or the string associated with its list
        tcontext :: [(Maybe Text, [(Int, Expr (Atomic a))])] -> Either (Int, Expr (Atomic a)) (Maybe Text)
        tcontext [] = Right Nothing
        tcontext ((_, []) : tl) = tcontext tl
        tcontext ((name, [(tn, _)]) : tl)
            | tn == n = Right name
            | otherwise = tcontext tl
        tcontext ((name, (pn, _) : tnt : ttl) : tl)
            | pn == n = Left tnt
            | otherwise = tcontext $ (name, tnt : ttl) : tl


type StdTask = Expr PDDLAtom
type StdTaskHead = Maybe StdTask
type StdTaskDef = Expr (Atomic TypedVarExpr)


data Method c e = Method
    Name
    (Parameters TypedVarExpr)
    (TaskHead StdTaskHead)
    (Precondition c)
    (Effect e)
    (TaskLists TermExpr)
    TaskConstraints
    deriving (Data, Eq)
deriving instance Typeable Method


instance (Data c, Data e) => HasName (Method c e)
instance (Data c, Data e) => HasParameters TypedVarExpr (Method c e)
instance (Data c, Data e) => HasTaskHead StdTaskHead (Method c e)
instance (Data c, Data e) => HasPrecondition c (Method c e)
instance (Data c, Data e) => HasEffect e (Method c e)
instance (Data c, Data e) => HasTaskLists TermExpr (Method c e)
instance (Data c, Data e) => HasTaskConstraints (Method c e)

defaultMethod :: forall c e. Method c e
defaultMethod = Method (Name "")
    (Parameters [])
    (TaskHead Nothing)
    (Precondition [])
    (Effect [])
    (TaskLists [])
    (TaskConstraints [])

instance (Data p, Data t, Data ep, Data e, PDDLDoc p, PDDLDoc [t], PDDLDoc ep, PDDLDoc e)
    => PDDLDoc (Method (Maybe Text, p) ([t], Maybe ep, [e])) where
    pddlDoc m = parens $ sep  ([
        (if null (getTaskLists m) then prettyT ":action" else prettyT ":method") <+> prettyT (getName m),
        prettyT ":parameters" <+> parens (pddlDoc $ getParameters m),
        docMaybe ((prettyT ":task" <+>) . pddlDoc) (getTaskHead m),
        docList ((prettyT ":precondition" <+>) . andDoc prefDoc) $ getPrecondition m,
        docList ((prettyT ":effect" <+>) . andDoc id . concatMap effectDoc) $ getEffect m]
        ++ map tasklist (getTaskLists m)
        ++ [docList ((prettyT ":ordering" <+>) . parens . sep . map tconstraint) (getTaskConstraints m)])
        where
            andDoc :: forall a ann. (a -> Doc ann) -> [a] -> Doc ann
            andDoc f [t] = f t
            andDoc f tl = parens $ sep $
                prettyT "and"
                : map f tl
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
                andDoc id $ condDoc ep el]]
            condDoc :: Maybe ep -> [e] -> [Doc ann]
            condDoc Nothing el = map pddlDoc el
            condDoc (Just ep) el = [parens $ sep [
                prettyT "when",
                pddlDoc ep,
                andDoc pddlDoc el ]]



type StandardMethod = Method PDDLPrecond PDDLEffect


htnDescLanguage :: forall st. T.LanguageDef st
htnDescLanguage = pddlDescLanguage {
    T.reservedNames = T.reservedNames pddlDescLanguage ++
        [":method", ":task", ":tasks", ":branch", ":ordering"]
    }

htnDescLexer :: forall st. T.TokenParser st
htnDescLexer = T.makeTokenParser htnDescLanguage

htnParser :: CharParser (StandardHTNDomain) (StandardHTNDomain)
htnParser = let
        constraintP = constraintGDParser pddlExprLexer
        prefP = prefListParser pddlExprLexer (gdParser pddlExprLexer :: CharParser StandardHTNDomain GDExpr)
        effectP = ucEffectParser pddlExprLexer
            (gdParser pddlExprLexer :: CharParser StandardHTNDomain GDExpr)
            (effectDParser pddlExprLexer :: CharParser StandardHTNDomain EffectDExpr)
    in
    domainParser htnDescLexer $
        (hDomainInfoParser htnDescLexer constraintP)
        <|>
        derivedParser pddlDescLexer
          (atomicTypeParser pddlExprLexer (varParser pddlExprLexer) :: CharParser st TypedPredicateExpr)
          (gdParser pddlExprLexer)
        <|>
        (methodParser htnDescLexer prefP effectP)


htnProblemParser :: CharParser StandardHTNProblem StandardHTNProblem
htnProblemParser =
    let
        stateP = T.parens pddlExprLexer $ initLiteralParser pddlExprLexer :: CharParser StandardHTNProblem InitLiteralExpr
        goalP = prefGDParser pddlExprLexer :: CharParser StandardHTNProblem PreferenceGDExpr
        constraintP = constraintGDParser pddlExprLexer :: CharParser StandardHTNProblem ConstraintGDExpr
        infoP =
            (taskListParser htnDescLexer (constTermParser pddlExprLexer) >>= updateState)
            <|> (taskConstraintParser htnDescLexer >>= updateState)
            <|> problemInfoParser htnDescLexer stateP goalP constraintP
    in
    problemParser htnDescLexer infoP

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
        updateState (setTaskHead tasks))
    <|>
    domainInfoParser dlex condParser


methodParser :: forall st p e .
    (HasActions (Method p e) st,
    Data p, Data e) =>
    T.TokenParser st ->
    CharParser st [p] ->
    CharParser st [e] ->
    CharParser st ()
methodParser mylex condParser effParser = do
    let
        infoParser :: CharParser st (Method p e -> Method p e)
        infoParser = methodInfoParser mylex condParser effParser
    T.reserved mylex ":method" <|> T.reserved mylex ":action"
    name <- T.identifier mylex
    updates <- many infoParser
    let method = foldl (\a t -> t a) (setName (pack name) defaultMethod) updates
    updateState (\d -> setActions (method : getActions d) d)

methodInfoParser :: (HasParameters TypedVarExpr a,
    HasPrecondition b a,
    HasEffect e a,
    HasTaskHead (Maybe (Expr PDDLAtom)) a,
    HasTaskLists TermExpr a,
    HasTaskConstraints a) =>
    T.TokenParser st -> CharParser st [b] -> CharParser st [e] -> CharParser st (a -> a)
methodInfoParser mylex condParser effParser =
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
    taskConstraintParser mylex


taskHeadParser :: HasTaskHead (Maybe (Expr PDDLAtom)) a =>
    T.TokenParser st -> CharParser st (a -> a)
taskHeadParser mylex = do
    try $ T.reserved mylex ":task"
    task <- maybeParser mylex $ taskParser mylex (termParser mylex)
    return $ setTaskHead task

taskListParser :: HasTaskLists a m =>
    T.TokenParser st -> CharParser st a -> CharParser st (m -> m)
taskListParser mylex argParser = do
    try $ T.reserved mylex ":tasks"
    T.parens mylex $ do
        name <- optionMaybe (try $ T.identifier mylex)
        tasks <- many $ T.parens mylex $ taskParser mylex argParser
        return $ \m -> setTaskLists ((fmap pack name, tasks) : getTaskLists m) m

taskConstraintParser :: HasTaskConstraints a =>
    T.TokenParser st -> CharParser st (a -> a)
taskConstraintParser mylex = do
    try $ T.reserved mylex ":ordering"
    T.parens mylex $ do
        orderings <- many $ T.parens mylex $ do
            t1 <- T.identifier mylex
            t2 <- T.identifier mylex
            return (pack t1, pack t2)
        return $ \m -> setTaskConstraints
            (orderings ++ getTaskConstraints m) m


taskParser :: T.TokenParser st -> CharParser st a -> CharParser st (Expr (Atomic a))
taskParser mylex argParser = do
    name <- T.identifier mylex
    terms <- many argParser
    return $ eAtomic (pack name) terms

parseHTNPDDL :: SourceName -> String -> Either ParseError StandardHTNDomain
parseHTNPDDL source input =
    runParser htnParser emptyHDomain source input

parseHTNProblem :: SourceName -> String -> Either ParseError StandardHTNProblem
parseHTNProblem source input =
    runParser htnProblemParser emptyHProblem source input
