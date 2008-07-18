{-# OPTIONS
 -fglasgow-exts
 -fallow-overlapping-instances
 #-}
module HTNTranslation.HTNPDDL (
    module Planning.PDDL.Representation,
    StandardHTNDomain,
    Method, StandardMethod,
    TaskHead, getTaskHead, setTaskHead,
    Branches, getBranches, setBranches,
    Branch(..), emptyBranch,
    TaskList, taskName, taskArgs,
    TermExpr,
    TypedTermExpr,
    parseHTNPDDL
) where

import Data.Generics (Data, Typeable, Typeable1)
import Data.Maybe
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.PrettyPrint

import Planning.PDDL.Representation
import Planning.PDDL.Parser

type TermExpr = Expr (Const :+: Var)
type TypedTermExpr = Expr (Typed TermExpr :+: Const :+: Var)

type StandardHTNDomain = Domain (Expr (DomainItem StandardAction :+: DomainItem StandardMethod))
deriving instance Data (Expr (DomainItem StandardAction :+: DomainItem StandardMethod))

type TaskList = [Expr (StdAtomicType)]
taskName (In (Atomic p _)) = p
taskArgs (In (Atomic _ al)) = al

data Branch c = Branch {
    branchName :: String,
    bparameters :: [TypedVarExpr],
    bprecondition :: Maybe c,
    tasks :: TaskList
} deriving (Data, Eq)
deriving instance Typeable1 Branch


emptyBranch = Branch {
    branchName = "empty",
    bparameters = [],
    bprecondition = Nothing,
    tasks = []
}

{-
data Method c e = Method {
    methodName :: String,
    mparameters :: [TypedVarExpr],
    taskHead :: Expr (Atomic TermExpr),
    mprecondition :: Maybe c,
    branches :: [Branch c]
} deriving Eq
instance Functor (Method c) where
    fmap f (Method n p t pre b) = Method n p t pre b
instance (Eq c) => FuncEq (Method c) where
    funcEq (Method n1 p1 h1 pre1 b1) (Method n2 p2 h2 pre2 b2) =
        (n1 == n2) && (p1 == p2) && (h1 == h2) && (pre1 == pre2) && (b1 == b2)
method name params task precond branches = inject (Method name params task precond branches)

instance PDDLDoc c => PDDLDoc (Method (Expr c)) where
    pddlDoc (Method name params (In task) precond branches) = parens $
        (text ":method" <+> text name) $$
        (text ":parameters" <+> parens (sep $ map pddlExprDoc params)) $$
        (text ":task" <+> pddlDoc task) $$
        (text ":precondition" <+> (case precond of
            Just (In c) -> pddlDoc c
            _ -> parens empty)) $$
        sep [text ":branches", parens (sep $ map branchDoc branches)]
        where
            branchDoc br = parens $
                (text ":branch" <+> text (branchName br)) $$
                (text ":parameters" <+> parens (sep $ map pddlExprDoc $ bparameters br)) $$
                (text ":precondition" <+> (case (bprecondition br) of
                    Just (In c) -> pddlDoc c
                    _ -> parens empty)) $$
                (text ":tasks" <+> sep [pddlDoc t | In t <- tasks br])
-}

data TaskHead f = TaskHead (Expr (Atomic f)) deriving (Data, Eq, Typeable)
unTaskHead (TaskHead h) = h
class (Data a, Data f) => HasTaskHead f a | a -> f where
    getTaskHead :: a -> Expr (Atomic f)
    getTaskHead = unTaskHead . fromJust . gfind
    setTaskHead :: Expr (Atomic f) -> a -> a
    setTaskHead h r = fromJust $ greplace r (TaskHead h)

data Branches c = Branches [Branch c] deriving (Data, Eq)
deriving instance Typeable1 Branches
unBranches (Branches bl) = bl
class (Data a, Data c) => HasBranches c a | a -> c where
    getBranches :: a -> [Branch c]
    getBranches = unBranches . fromJust . gfind
    setBranches :: [Branch c] -> a -> a
    setBranches bl r = fromJust $ greplace r (Branches bl)

data Method c = Method
    Name
    (Parameters TypedVarExpr)
    (TaskHead TermExpr)
    (Precondition c)
    (Branches c)
    deriving (Data, Eq)
deriving instance Typeable1 Method


instance Data c => HasName (Method c)
instance Data c => HasParameters TypedVarExpr (Method c)
instance Data c => HasTaskHead TermExpr (Method c)
instance Data c => HasPrecondition c (Method c)
instance Data c => HasBranches c (Method c)

defaultMethod = Method undefined 
    (Parameters [])
    undefined
    (Precondition Nothing)
    (Branches [])

instance (Data (Expr c), PDDLDoc c) => PDDLDoc (DomainItem (Method (Expr c))) where
    pddlDoc (DomainItem m) = parens $ sep [
        text ":method" <+> text (getName m),
        text ":parameters" <+> parens (sep $ map pddlExprDoc $ getParameters m),
        text ":task" <+> pddlExprDoc (getTaskHead m),
        docMaybe ":precondition" $ getPrecondition m,
        sep [text ":branches", parens ( sep $ map branchDoc $ getBranches m )]]
        where
            branchDoc br = parens $ sep [
                text ":branch" <+> text (branchName br),
                text ":parameters" <+> parens (sep $ map pddlExprDoc $ bparameters br),
                docMaybe ":precondition" $ bprecondition br,
                text ":tasks" <+> parens (sep $ map pddlExprDoc $ tasks br) ]
                
        

type StandardMethod = Method GoalExpr

            
htnLanguage = pddlLanguage {
    T.reservedNames = T.reservedNames pddlLanguage ++
        [":method", ":task", ":tasks", ":branch", ":branches"]
    }

htnLexer = T.makeTokenParser htnLanguage

htnParser :: GenParser Char (StandardHTNDomain) (StandardHTNDomain)
htnParser = let 
        lex = htnLexer 
        condParser = conditionParser lex :: CharParser StandardHTNDomain GoalExpr
        actions =
            (actionParser lex condParser)
            <|>
            (methodParser lex condParser :: CharParser StandardHTNDomain ())
    in
    domainParser lex (domainInfoParser lex (condParser)) actions


parseTypedTerm :: T.TokenParser a -> CharParser a TypedTermExpr
parseTypedTerm lex =
    (try $ do
        In (Const cstr) <- constParser lex
        option (eConst cstr) (do
            T.reserved lex "-"
            tstr <- T.identifier lex
            return $ eTyped (eConst cstr :: TermExpr) (eConst tstr :: Expr Const)))
    <|>
    (do
        In (Var vstr) <- varParser lex
        option (eVar vstr) (do
            T.reserved lex "-"
            tstr <- T.identifier lex
            return $ eTyped (eVar vstr :: TermExpr) (eConst tstr :: Expr Const)))

methodParser :: forall f a .
    ((:<:) (DomainItem (Method f)) a, Data f, Data (Expr a)) => 
    T.TokenParser (Domain (Expr a)) -> 
    CharParser (Domain (Expr a)) f -> 
    CharParser (Domain (Expr a)) ()
methodParser lex condParser = do
    let infoParser = methodInfoParser lex condParser :: CharParser (Domain (Expr a)) (Method f -> Method f)
    try $ T.reserved lex ":method"
    name <- T.identifier lex
    updates <- many infoParser
    let method = foldl (\a t -> t a) (setName name defaultMethod) updates
    updateState (\d -> d { items = domainItem method : items d })

methodInfoParser lex condParser =
    paramParser lex
    <|>
    precondParser lex condParser
    <|>
    (do
        try $ T.reserved lex ":task"
        task <- T.parens lex $ atomicFormulaParser lex (termParser lex)
        return $ setTaskHead (task :: Expr StdAtomicType))
    <|>
    (do
        try $ T.reserved lex ":branches"
        bl <- T.parens lex $ many $ branchParser lex condParser
        return $ setBranches bl)

branchParser lex condParser = T.parens lex $ do
    T.reserved lex ":branch"
    name <- T.identifier lex
    collect (emptyBranch { branchName = name }) (\b ->
        (do
            try $ T.reserved lex ":parameters"
            parameters <- T.parens lex $ many $ parseTypedVar lex
            return $ b { bparameters = parameters })
        <|>
        (do
            try $ T.reserved lex ":precondition"
            cond <- maybeParser lex condParser
            return $ b { bprecondition = cond })
        <|>
        (do
            try $ T.reserved lex ":tasks"
            tasks <- T.parens lex $ many $ taskParser lex
            return $ b { tasks = tasks }))

        

taskParser lex = T.parens lex (do
    name <- T.identifier lex
    terms <- many $ termParser lex
    return $ eAtomic name terms)

parseHTNPDDL source input =
    runParser htnParser emptyDomain source input
