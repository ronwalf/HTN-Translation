module HTNTranslation.HTNPDDL (
    module Planning.PDDL,
    HTNDomain (..),
    Method(..),
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

data Method = Method {
    methodName :: String,
    mparameters :: [(String, Maybe String)],
    taskHead :: (String, [Term]),
    mprecondition :: Condition,
    tasks :: TaskList
}

instance Show Method where
    show m = let indent = "  " in
        "(:method " ++ (methodName m) ++ "\n" ++
        indent ++ ":task (" ++ (showTaskHead m) ++ ")\n" ++
        indent ++ ":parameters (" ++ (unwords $ map (\x -> "?" ++ showType x) $ mparameters m) ++ ")\n" ++
        indent ++ ":precondition " ++ (show $ mprecondition m) ++ "\n" ++
        indent ++ ":tasks " ++ (show $ tasks m) ++ "\n" ++
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
        [":method", ":task", ":tasks"]
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
    method <- collect (Method { methodName = name }) (methodInfoParser lex condParser)
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
        try $ T.reserved lex ":parameters"
        T.parens lex (do
            terms <- parseTypedList lex $ (char '?' >> T.identifier lex)
            return $ method { mparameters = terms }))
    <|>
    (do
        try $ T.reserved lex ":precondition"
        T.parens lex (do
            cond <- condParser
            return $ method { mprecondition = cond }))
    <|>
    (do
        try $ T.reserved lex ":tasks"
        tasks <- taskListParser lex
        return $ method { tasks = tasks })

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

