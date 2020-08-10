{-# OPTIONS_GHC
    -freduction-depth=30
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
  #-}
module Main where

import Debug.Trace

import Control.Monad
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text, unpack)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec (CharParser, runParser)
import Text.ParserCombinators.Parsec.Token (parens)

import Planning.PDDL.Parser (atomicParser)

import HTNTranslation.HTNPDDL
import HTNTranslation.ProblemLifter
import HTNTranslation.Translation
import qualified HTNTranslation.ADLTranslation as ATrans
-- import qualified HTNTranslation.ADLTranslation2 as ATrans2
import qualified HTNTranslation.TOTranslation as TOTrans
-- import qualified HTNTranslation.TOTranslation09 as TOTrans09
import qualified HTNTranslation.ProgressionBounds as PB
import qualified HTNTranslation.ProgressionMin as PM

data TranslationType =
    ADLTranslation
    -- | ADLTranslation2
    | STRIPSTranslation
    | TOTranslation
    -- | TOTranslation09

data Options = Options
    { optTrans :: TranslationType
    , optNumIds :: Int
    , optMaxArity :: Int
    , optLift :: [Expr (Atomic TermExpr)]
    , optPostfix :: String
    , optStats :: Bool
    , optVerbose :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optTrans = STRIPSTranslation
    , optNumIds = 0
    , optMaxArity = 20
    , optLift = []
    , optPostfix = ".pddl"
    , optStats = False
    , optVerbose = False
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['t'] ["type"]
        (ReqArg (\t opts -> fmap (\x -> opts { optTrans = x }) $
            case map toLower t of
                "adl" -> return ADLTranslation
                -- "adl2" -> return ADLTranslation2
                "strips" -> return STRIPSTranslation
                "ordered" -> return TOTranslation
                -- "ordered09" -> return TOTranslation09
                _ -> fail $ "Unknown translation type '" ++ t ++ "'")
            "TYPE")
        "Set the translation type (adl|strips|ordered)"
    , Option ['i']  ["identifiers"]
        (ReqArg (\n opts ->
            case reads n of
                [(ids, "")] -> return $ opts { optNumIds = ids}
                _ -> fail "Cannot parse number of identifiers")
         "NUM")
         "Manually set the number of identifiers to insert."
    , Option ['a']  ["arity"]
        (ReqArg (\n opts ->
            case reads n of
                [(a, "")] -> return $ opts { optMaxArity = a}
                _ -> fail "Cannot parse number of identifiers")
         "NUM")
         "Max arity of generated task predicates"
    , Option ['l'] ["lift"]
        (ReqArg (\taskstr opts -> do
            task <- errCheck $ runParser taskParser () taskstr taskstr
            return $ opts { optLift = task : optLift opts })
        "TASK")
        "lift standard PDDL problems into HTNPDDL"
    , Option ['p'] ["postfix"]
        (ReqArg (\pfix opts -> return $ opts { optPostfix = pfix })
        "POSTFIX")
        "postfix for saved files (domain<POSTFIX> and problem<POSTFIX>)"
    , Option ['s'] ["stats"]
        (NoArg (\opts -> return $ opts { optStats = True }))
        "Print problem bound statistics"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> return $ opts { optVerbose = True }))
        "verbose"
    ]

saveFile :: Options -> String -> String -> IO ()
saveFile opts name contents =
    writeFile (name ++ optPostfix opts) contents


errCheck :: (Show t) => Either t b -> IO b
errCheck (Left err) = do
    hPrint stderr err
    exitFailure
errCheck (Right prob) = return prob


taskParser :: CharParser a (Expr (Atomic TermExpr))
taskParser = parens pddlExprLexer $
    atomicParser pddlExprLexer $ termParser pddlExprLexer


replaceInitialTasks :: StandardHTNDomain -> StandardHTNProblem -> (StandardHTNDomain, StandardHTNProblem, Expr (Atomic ConstTermExpr))
replaceInitialTasks domain problem = 
    ( flip stripUnreachable problem' $ injectDomain injectedMethod
    , problem'
    , tname
    )
    where
    problem' = setTaskList [(Nothing, tname :: Expr (Atomic TermExpr))] $
        setConstraints [] $
        setTaskOrdering [] $
        setParameters [] problem
    tname :: forall t . Expr (Atomic t)
    tname = eAtomic "htn_initial_task" ([] :: [t])
    injectDomain :: StandardMethod -> StandardHTNDomain
    injectDomain m = trace (show $ pddlDoc m) $
        setTaskHead (tname : getTaskHead domain) $
        setActions (m : getActions domain) domain
    injectedMethod :: StandardMethod
    injectedMethod =
        let
            taskList = getTaskList problem
            taskOrdering = getTaskOrdering problem
            params = getParameters problem
        in
        setName "assert_initial_tasks" $
        setTaskHead (Just tname) $
        setParameters params $
        setTaskList taskList $
        setConstraints (getConstraints problem) $
        setTaskOrdering taskOrdering defaultMethod

stripUnreachable :: StandardHTNDomain -> StandardHTNProblem -> StandardHTNDomain
stripUnreachable dom prob =
    setActions (reachable ++ headless) dom
    where
    headless = filter (isNothing . getTaskHead) $ getActions dom
    reachable = 
        concatMap (PB.findMethods dom :: Text -> [StandardMethod]) $
        PB.findReachableTasks dom $
        listTaskNames prob

processProblem :: Options -> StandardHTNDomain -> String -> IO (Int, StandardHTNDomain)
processProblem opts domain fname = do
    contents <- readFile fname
    problem <- {-# SCC "problem-parsing" #-} errCheck $ parseHTNProblem fname contents
    let lifted = case optLift opts of
            [] -> problem
            tasks -> liftProblem tasks problem
    let (domain', problem', initialTask) = replaceInitialTasks domain lifted
    traceM $ show $ pddlDoc domain'
    traceM "------------------"
    traceM $ show $ pddlDoc problem'
    traceM "=================="
    let (lbound, lbounds) = {-# SCC "min-progression" #-} PM.minProgression domain' problem'
    when (optStats opts) $
        putStrLn $ "Problem '" ++ unpack (getName problem') ++
            "' min progression bound: " ++ show lbound
    when (optVerbose opts) $
        hPutStrLn stderr $ "Problem '" ++ unpack (getName problem') ++
            "' min progression bound " ++ show lbound ++ ", tasks: " ++ show lbounds

    numIds <- if optNumIds opts > 0
        then return (optNumIds opts)
        else do
            (bound, bounds) <- {-# SCC "progression-bound" #-} PB.boundProgression domain' problem'
            when (optStats opts) $ putStrLn $ "Problem '" ++ show (getName problem') ++
                "' max progression bound: " ++ show bound
            when (optVerbose opts) $
                hPutStrLn stderr $ "Problem '" ++ show (getName problem') ++
                    "' max progression bound: " ++ show bound ++ ", tasks: " ++ show bounds
            return bound
    let pddl_problem = {-# SCC "translating-problem" #-} case optTrans opts of
            TOTranslation -> TOTrans.translateProblem emptyProblem numIds initialTask problem'
            -- TOTranslation09 -> TOTrans.translateProblem emptyProblem numIds initialTask problem'
            ADLTranslation ->  ATrans.translateProblem emptyProblem numIds initialTask problem'
            -- ADLTranslation2 ->  ATrans2.translateProblem emptyProblem numIds initialTask problem'
            STRIPSTranslation -> translateProblem emptyProblem (optMaxArity opts) numIds initialTask problem'
    {-# SCC "saving-file" #-} saveFile opts "problem" $ show $ pddlDoc (pddl_problem :: PDDLProblem)
    return (numIds, domain')

main :: IO ()
main = do
    argv <- getArgs
    (opts, domFile, probFile) <- case getOpt Permute options argv of
        (o,[dom,file],[]) -> do
            opts <- foldM (\opts f -> f opts) defaultOptions o
            return (opts, dom, file)
        (_, _, errs) ->
            ioError $ userError $
            concat errs
            ++ usageInfo "Usage: htntranslate [OPTION...] domain problem" options
    domContents <- readFile domFile
    domain <-  {-# SCC "parse-domain" #-} errCheck (parseHTNPDDL domFile domContents) >>= tailRec opts
    when (optVerbose opts) $ do
        hPutStrLn stderr "Parsed domain:"
        hPrint stderr domain
        hPutStrLn stderr ""
    (numIds, domain') <- processProblem opts domain probFile
    tdomain <- case optTrans opts of
            TOTranslation -> TOTrans.translateDomain emptyDomain defaultAction domain'
                [TOTrans.translateUncontrolled, TOTrans.translateAction, TOTrans.translateMethod1, TOTrans.translateMethod]
            -- TOTranslation09 -> TOTrans.translateDomain emptyDomain defaultAction domain'
                -- [TOTrans.translateUncontrolled, TOTrans.translateAction, TOTrans.translateMethod1, TOTrans09.translateMethod]
            ADLTranslation -> ATrans.translateDomain emptyDomain defaultAction domain'
                    [ATrans.translateUncontrolled, ATrans.translateAction, ATrans.translateMethod]
            -- ADLTranslation2 -> ATrans2.translateDomain emptyDomain defaultAction domain'
                    -- [ATrans2.translateUncontrolled, ATrans2.translateAction, ATrans2.translateMethod1, ATrans2.translateMethod]
            STRIPSTranslation -> translateDomain emptyDomain defaultAction domain' (optMaxArity opts) numIds
                    [translateUncontrolled, translateAction, translateMethod1, translateMethod]
    when (optStats opts) $ do
        putStrLn $ "max parameter arity: " ++ show (maximum $ (0:) $ map (length . getParameters) $ getActions tdomain)
        putStrLn $ "max predicate arity: " ++ show (maximum $ (0:) $ map (length . taskArgs) $ getPredicates tdomain)
        putStrLn $ "unary and type arity: "
            ++ show ((+) (length $ getTypes tdomain) $
                            length $ filter ((== 1) . length . taskArgs) $ getPredicates tdomain)
    saveFile opts "domain" $ show $ pddlDoc tdomain
    return ()
    where
    tailRec :: Options -> StandardHTNDomain -> IO StandardHTNDomain
    tailRec opts domain =
        let tasks = filter (taskHasLooseEnds domain) $ tasksWithSuccessors domain in
        case (optTrans opts, null tasks) of
        (ADLTranslation,_) -> return domain
        (_,True) -> return domain
        (_, False) -> do
            when (optVerbose opts) $
                hPutStrLn stderr $ "Adding dummy last task for these tasks: " ++ show tasks
            let (dtask, dom') = insertDummy defaultMethod domain
            return $ foldl' (\dom task -> ensureLastTask dom dtask task) dom' tasks
