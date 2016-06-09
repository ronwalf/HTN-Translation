{-# OPTIONS_GHC
    -fcontext-stack=30
    -Wall
  #-}
{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
  #-}
module Main where

import Control.Monad
import Data.Char (toLower)
import Data.List (foldl')
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
import qualified HTNTranslation.ADLTranslation2 as ATrans2 
import qualified HTNTranslation.TOTranslation as TOTrans
import qualified HTNTranslation.TOTranslation09 as TOTrans09
import qualified HTNTranslation.ProgressionBounds as PB
import qualified HTNTranslation.ProgressionMin as PM

data TranslationType = 
    ADLTranslation 
    | ADLTranslation2
    | STRIPSTranslation
    | TOTranslation 
    | TOTranslation09

data Options = Options
    { optTrans :: TranslationType
    , optNumIds :: Int
    , optMaxArity :: Int
    , optLift :: [Expr (Atomic ConstTermExpr)]
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
        (ReqArg (\t opts -> liftM (\x -> opts { optTrans = x }) $
            case map toLower t of
                "adl" -> return ADLTranslation
                "adl2" -> return ADLTranslation2
                "strips" -> return STRIPSTranslation
                "ordered" -> return TOTranslation
                "ordered09" -> return TOTranslation09
                _ -> fail $ "Unknown translation type '" ++ t ++ "'")
            "TYPE")
        "Set the translation type (adl|adl2|strips|ordered|ordered09)"
    , Option ['i']  ["identifiers"]
        (ReqArg (\n opts -> do
            case (reads n) of
                [(ids, "")] -> return $ opts { optNumIds = ids}
                _ -> fail "Cannot parse number of identifiers")
         "NUM")
         "Manually set the number of identifiers to insert."
    , Option ['a']  ["arity"]
        (ReqArg (\n opts -> do
            case (reads n) of
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
        "postfix for saved files"
    , Option ['s'] ["stats"]
        (NoArg (\opts -> return $ opts { optStats = True }))
        "Print problem bound statistics"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> return $ opts { optVerbose = True }))
        "verbose"
    ]

saveFile :: Options -> String -> String -> IO ()
saveFile opts name contents = do
    let oname = reverse (tail $ dropWhile (/= '.') $ reverse name) ++ optPostfix opts
    writeFile oname contents
   

errCheck :: (Show t) => Either t b -> IO b
errCheck (Left err) = do
    hPrint stderr err
    exitFailure 
errCheck (Right prob) = return prob


taskParser :: CharParser a (Expr (Atomic ConstTermExpr))
taskParser = parens pddlExprLexer $
    atomicParser pddlExprLexer $ constTermParser pddlExprLexer


processProblem :: Options -> StandardHTNDomain -> String -> IO Int
processProblem opts domain fname = do
    contents <- readFile fname
    problem <- {-# SCC "problem-parsing" #-} errCheck $ parseHTNProblem fname contents
    let lifted = case optLift opts of
            [] -> problem
            tasks -> liftProblem tasks problem
    let (lbound, lbounds) = {-# SCC "min-progression" #-} PM.minProgression domain lifted
    when (optStats opts) $
        putStrLn $ "Problem '" ++ getName lifted ++
            "' min progression bound: " ++ show lbound
    when (optVerbose opts) $
        hPutStrLn stderr $ "Problem '" ++ getName lifted ++
            "' min progression bound " ++ show lbound ++ ", tasks: " ++ show lbounds

    numIds <- if (optNumIds opts > 0)
        then return (optNumIds opts)
        else do
            (bound, bounds) <- {-# SCC "progression-bound" #-} PB.boundProgression domain lifted
            when (optStats opts) $ putStrLn $ "Problem '" ++ getName lifted ++
                "' max progression bound: " ++ show bound
            when (optVerbose opts) $ do
                hPutStrLn stderr $ "Problem '" ++ getName lifted ++
                    "' max progression bound: " ++ show bound ++ ", tasks: " ++ show bounds
            return bound 
    let problem' = {-# SCC "translating-problem" #-} case optTrans opts of
            TOTranslation -> TOTrans.translateProblem emptyProblem numIds lifted
            TOTranslation09 -> TOTrans.translateProblem emptyProblem numIds lifted
            ADLTranslation ->  ATrans.translateProblem emptyProblem numIds lifted
            ADLTranslation2 ->  ATrans2.translateProblem emptyProblem numIds lifted
            STRIPSTranslation -> translateProblem emptyProblem (optMaxArity opts) numIds lifted
    {-# SCC "saving-file" #-} saveFile opts fname $ show $ pddlDoc problem'
    return numIds

main :: IO ()
main = do
    argv <- getArgs
    (opts, domFile, probFiles) <- case getOpt Permute options argv of
        (o,dom:files,[]) -> do
            opts <- foldM (\opts f -> f opts) defaultOptions o
            return (opts, dom, files)
        (_, _, errs) -> 
            ioError $ userError $
            concat errs 
            ++ usageInfo "Usage: htntranslate [OPTION...] domain files..." options
    domContents <- readFile domFile
    domain <-  {-# SCC "parse-domain" #-} (errCheck $ parseHTNPDDL domFile domContents) >>= tailRec opts
    when (optVerbose opts) $ do
        hPutStrLn stderr "Parsed domain:"
        hPutStrLn stderr $ show domain
        hPutStrLn stderr ""
    numIds <- liftM (maximum . (1:)) $ mapM (processProblem opts domain) probFiles 
    tdomain <- case optTrans opts of
            TOTranslation -> TOTrans.translateDomain emptyDomain defaultAction domain
                [TOTrans.translateUncontrolled, TOTrans.translateAction, TOTrans.translateMethod1, TOTrans.translateMethod]
            TOTranslation09 -> TOTrans.translateDomain emptyDomain defaultAction domain
                [TOTrans.translateUncontrolled, TOTrans.translateAction, TOTrans.translateMethod1, TOTrans09.translateMethod]
            ADLTranslation -> ATrans.translateDomain emptyDomain defaultAction domain 
                    [ATrans.translateUncontrolled, ATrans.translateAction, ATrans.translateMethod]
            ADLTranslation2 -> ATrans2.translateDomain emptyDomain defaultAction domain 
                    [ATrans2.translateUncontrolled, ATrans2.translateAction, ATrans2.translateMethod1, ATrans2.translateMethod]
            STRIPSTranslation -> translateDomain emptyDomain defaultAction domain (optMaxArity opts) numIds
                    [translateUncontrolled, translateAction, translateMethod1, translateMethod]
    when (optStats opts) $ do
        putStrLn $ "max parameter arity: " ++ (show $ maximum $ (0:) $ map (length . getParameters) $ getActions tdomain)
        putStrLn $ "max predicate arity: " ++ (show $ maximum $ (0:) $ map (length . taskArgs) $ getPredicates tdomain)
        putStrLn $ "unary and type arity: " 
            ++ (show $ (+) (length $ getTypes tdomain) $ 
                            length $ filter ((== 1) . length . taskArgs) $ getPredicates tdomain)
    saveFile opts domFile $ show $ pddlDoc tdomain
    return ()
    where
    tailRec :: Options -> StandardHTNDomain -> IO StandardHTNDomain
    tailRec opts domain =
        let tasks = filter (taskHasLooseEnds domain) $ tasksWithSuccessors domain in 
        case (optTrans opts, null tasks) of
        (ADLTranslation,_) -> do
            return domain
        (_,True) -> do
            return domain
        (_, False) -> do
            when (optVerbose opts) $ do
                hPutStrLn stderr $ "Adding dummy last task for these tasks: " ++ show tasks
            let (dtask, dom') = insertDummy defaultMethod domain
            return $ foldl' (\dom task -> ensureLastTask dom dtask task) dom' tasks
