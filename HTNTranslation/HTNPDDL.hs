module HTNTranslation.HTNPDDL (
    module Planning.PDDL,
    HTNDomain (..),
    Method(..),
    Branch(..),
    TaskList(..),
    parseHTNPDDL
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T

import Planning.PDDL
import Planning.PDDL.Parser

newtype HTNDomain = HTNDomain (DomainInfo, [Method], [DomainItem])

instance Show HTNDomain where
    show (HTNDomain (info, methods, items)) = 
        let indent = "  " in
        "; " ++ (domainName info) ++ " domain\n" ++
        "(define " ++ (show info) ++ "\n" ++
        (unlines $ map show methods) ++ "\n" ++ 
        (unlines $ map show items) ++
        ")\n"

emptyHTN =
    let (Domain (info, _)) = emptyDomain in
    HTNDomain (info, [], [])

updateInfo f (HTNDomain (d, m, a)) = HTNDomain (f d, m, a)
updateMethods f (HTNDomain (d, m, a)) = HTNDomain (d, f m, a)
updateItems f (HTNDomain (d, m, a)) = HTNDomain (d, m, f a)

data Branch = Branch {
    branchName :: String,
    bparameters :: [(String, Maybe String)],
    bprecondition :: Condition,
    tasks :: TaskList
} deriving Eq

emptyBranch = Branch {
    branchName = "",
    bparameters = [],
    bprecondition = Empty,
    tasks = OrderedTasks []
}

data Method = Method {
    methodName :: String,
    taskHead :: (String, [Term]),
    branches :: [Branch]
} deriving Eq

emptyMethod = Method {
    methodName = "",
    taskHead = ("", []),
    branches = []
}

instance Show Branch where
    show b = 
        let 
            indent = "    " 
            params = bparameters b
        in
        "  (:branch " ++ (branchName b) ++ "\n" ++
        indent ++ ":parameters (" ++ (unwords $ map (\x -> "?" ++ showType x) params) ++ ")\n" ++
        indent ++ ":precondition " ++ (show $ bprecondition b) ++ "\n" ++
        indent ++ ":tasks " ++ (show $ tasks b) ++ "\n" ++
        "  )\n"

instance Show Method where
    show m = let indent = "  " in
        "(:method " ++ (methodName m) ++ "\n" ++
        indent ++ ":task (" ++ (showTaskHead m) ++ ")\n" ++
        indent ++ ":branches (\n" ++
        (unlines $ map show $ branches m) ++
        "  )\n" ++
        ")"
        where
            showTaskHead m =
                let 
                    name = fst $ taskHead m
                    parts = map show $ snd $ taskHead m
                in
                unwords (name : parts)

data TaskList = 
    Task String [Term]
    | OrderedTasks [TaskList]
    deriving Eq

instance Show TaskList where
    show (Task name tl) = "(" ++ (unwords (name : map show tl)) ++ ")"
    show (OrderedTasks tl) = "(" ++ (unwords $ map show tl) ++ ")"


instance DomainInfoSink HTNDomain Condition DomainItem where
    setDomainName n = updateInfo (\i -> i { domainName = n })
    setRequirements r = updateInfo (\i -> i { requirements = r })
    setTypes t = updateInfo (\i -> i { types = t })
    setConstants c = updateInfo (\i -> i { constants = c })
    setPredicates p = updateInfo (\i -> i { predicates = p })
    setFunctions f = id
    setConstraints c = id
    addAction a = updateItems (\al -> a : al)


htnLanguage = pddlLanguage {
    T.reservedNames = T.reservedNames pddlLanguage ++
        [":method", ":task", ":tasks", ":branch", ":branches"]
    }

htnLexer = T.makeTokenParser htnLanguage

htnParser = let 
        lex = htnLexer 
        condParser = conditionParser lex
        actions =
            (actionParser lex condParser)
            <|>
            (methodParser lex condParser)
    in
    domainParser lex (domainInfoParser lex (condParser)) actions

methodParser lex condParser = do
    try $ T.reserved lex ":method"
    name <- T.identifier lex
    method <- collect (emptyMethod { methodName = name }) (methodInfoParser lex condParser)
    updateState (updateMethods (\ml -> method : ml))

methodInfoParser lex condParser method =
    (do
        try $ T.reserved lex ":task"
        T.parens lex (do
            name <- T.identifier lex
            terms <- many $ termParser lex
            return $ method { taskHead = (name, terms) }))
    <|>
    (do
        try $ T.reserved lex ":branches"
        bl <- T.parens lex $ many $ branchParser lex condParser
        return $ method {branches = (branches method) ++ bl})
 
 
branchParser lex condParser = T.parens lex $ do
    T.reserved lex ":branch"
    name <- T.identifier lex
    collect (emptyBranch { branchName = name }) (\b ->
        (do
            try $ T.reserved lex ":precondition"
            cond <- T.parens lex $ condParser
            return $ b { bprecondition = cond })
        <|>
        (do
            try $ T.reserved lex ":tasks"
            tasks <- taskListParser lex
            return $ b { tasks = tasks }))

        

taskListParser lex = T.parens lex (
    (do
        name <- T.identifier lex
        terms <- many $ termParser lex
        return $ Task name terms)
    <|>
    (do
        tasks <- many $ taskListParser lex
        return $ OrderedTasks tasks)
    <|>
    (do
        name <- T.identifier lex
        terms <- many $ termParser lex
        return $ Task name terms))

parseHTNPDDL source input =
    runParser htnParser emptyHTN source input

