module HTNTranslation.HTNPDDL (
    module Planning.PDDL.Representation,
    HTNDomain,
    Method(..), method, 
    Branch(..), emptyBranch,
    TaskList, taskName, taskArgs,
    TermExpr,
    TypedTermExpr,
    parseHTNPDDL
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.PrettyPrint

import Planning.PDDL.Representation
import Planning.PDDL.Parser

type TermExpr = Expr (Const :+: Var)
type TypedTermExpr = Expr (Typed TermExpr :+: Const :+: Var)

type HTNDomain c = Domain (Expr (Action c :+: Method c))

type TaskList = [Expr (StdAtomicType)]
taskName (In (Atomic p _)) = p
taskArgs (In (Atomic _ al)) = al

data Branch c = Branch {
    branchName :: String,
    bparameters :: [TypedVarExpr],
    bprecondition :: Maybe c,
    tasks :: TaskList
} deriving Eq


emptyBranch = Branch {
    branchName = "empty",
    bparameters = [],
    bprecondition = Nothing,
    tasks = []
}

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




            
htnLanguage = pddlLanguage {
    T.reservedNames = T.reservedNames pddlLanguage ++
        [":method", ":task", ":tasks", ":branch", ":branches"]
    }

htnLexer = T.makeTokenParser htnLanguage

htnParser :: GenParser Char (HTNDomain GoalExpr) (HTNDomain GoalExpr)
htnParser = let 
        lex = htnLexer 
        condParser = conditionParser lex :: CharParser (HTNDomain GoalExpr) GoalExpr
        actions =
            (actionParser lex condParser)
            <|>
            (methodParser lex condParser)
    in
    domainParser lex (domainInfoParser lex (condParser)) actions


parseTypedTerm :: T.TokenParser a -> CharParser a TypedTermExpr
parseTypedTerm lex =
    (try $ do
        In (Const cstr) <- constParser lex
        option (eConst cstr) (do
            T.reserved lex "-"
            tstr <- T.identifier lex
            return $ typed (eConst cstr :: TermExpr) (eConst tstr :: Expr Const)))
    <|>
    (do
        In (Var vstr) <- varParser lex
        option (eVar vstr) (do
            T.reserved lex "-"
            tstr <- T.identifier lex
            return $ typed (eVar vstr :: TermExpr) (eConst tstr :: Expr Const)))

methodParser lex condParser = do
    try $ T.reserved lex ":method"
    name <- T.identifier lex
    T.reserved lex ":parameters"
    params <- T.parens lex $ many $ parseTypedVar lex
    T.reserved lex ":task"
    task <- T.parens lex $ atomicFormulaParser lex (termParser lex)
    T.reserved lex ":precondition"
    precond <- maybeParser lex condParser
    T.reserved lex ":branches"
    bl <- T.parens lex $ many $ branchParser lex condParser
    let m = method name params task precond bl
    updateState (\d -> d { items = m : items d })


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
