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
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Maybe
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
import qualified HTNTranslation.CETranslation as CE
import HTNTranslation.Typing
import HTNTranslation.ProgressionBounds as PB

type TranslationDef = ([TaskIdUseFunc Maybe], [StandardMethod -> StateT (PDDLDomain, TranslationData StandardHTNDomain PDDLAction)  Maybe ()])

basicTranslation :: TranslationDef
basicTranslation = ([], [translateUncontrolled, translateAction, translateMethod [translateTask]])

idTranslation :: TranslationDef
idTranslation = 
    (--[useAtomicId, usePLastId]
     [useAtomicId]
    ,[translateUncontrolled, translateAction, translateMethod [translateTask]])

optimizedTranslation :: TranslationDef
optimizedTranslation = 
    (--[useAtomicId, usePLastId]
     [useAtomicId]
    ,[translateUncontrolled
     , translateCollapsed
     , translateAction
     --, translateHCMethod [ignoreCollapsedTasks, translateTask]
     , translateMethod [ignoreCollapsedTasks, translateTask]])

translations :: [(String, TranslationDef)]
translations =
    [("basic", basicTranslation)
    ,("id", idTranslation)
    ,("opt", optimizedTranslation)
    ]

data Options = Options
    { optCE :: Bool
    , optNumIds :: Int
    , optTranslation :: TranslationDef
    , optLift :: Maybe (Expr (Atomic ConstTermExpr))
    , optPostfix :: String
    , optVerbose :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optCE = True
    , optNumIds = 0
    , optTranslation = basicTranslation
    , optLift = Nothing
    , optPostfix = ".pddl"
    , optVerbose = False
    }

options :: [OptDescr (Options -> IO Options)]
options = 
    [ Option ['i']  ["identifiers"]
        (ReqArg (\n opts -> do
            case (reads n) of
                [(ids, "")] -> return $ opts { optNumIds = ids}
                _ -> fail "Cannot parse number of identifiers")
         "NUM")
         "Manually set the number of identifiers to insert."
    , Option ['l'] ["lift"]
        (ReqArg (\taskstr opts -> do
            task <- errCheck $ runParser taskParser () taskstr taskstr
            return $ opts { optLift = Just task })
        "TASK")
        "lift standard PDDL problems into HTNPDDL"
    , Option ['s'] ["strips"]
        (NoArg (\opts -> return $ opts { optCE = False }))
        "Use a roughly STRIPS-compatible translation (requires setting -i)"
    , Option ['o'] ["opt"]
        (OptArg (\ level opts -> return $ 
            opts { optTranslation = maybe (snd $ last translations) (fromJust . flip lookup translations) level})
         "LEVEL")
        ("Optimization level (STRIPS translation only).  LEVEL=" ++ intercalate ", " (map fst translations))
    , Option ['p'] ["postfix"]
        (ReqArg (\pfix opts -> return $ opts { optPostfix = pfix })
        "POSTFIX")
        "postfix for saved files"
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


processProblem :: Options -> TaskIdUse -> StandardHTNDomain -> String -> IO ()
processProblem opts useId domain fname = do
    contents <- readFile fname
    problem <- errCheck $ parseHTNProblem fname contents
    let lifted = maybe problem (flip liftProblem problem) $ optLift opts
    numIds <- if (optNumIds opts > 0)
        then return 0
        else do
            bounds <- boundProgression domain lifted
            when (optVerbose opts) $ do
                putStrLn $ "Problem " ++ getName lifted ++ " task bounds: " ++ show bounds
            liftM (snd . head) $ boundProgression domain problem
    let problem' = if (optCE opts)
            then CE.translateProblem emptyProblem numIds lifted
                else translateProblem emptyProblem useId numIds lifted
    saveFile opts fname $ show $ pddlDoc problem'
    return ()

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
    domain <- errCheck $ parseHTNPDDL domFile domContents
    when (optVerbose opts) $ do
        putStrLn "Parsed domain:"
        putStrLn $ show domain
        putStrLn ""
    let typemap = findTypes 
            [callSpotTyper, callCountTyper, lastPositionTyper, primitiveTyper, parentPositionTyper]
            domain
    let idUse = flip taskIdUse typemap $ fst $ optTranslation opts
    when (optVerbose opts) $ do
        putStrLn "Task types:"
        mapM_ (\(t, tt) -> putStrLn $ t ++ ": " ++ show tt) $ Map.toList typemap 
    let tdomain = if (optCE opts) 
            then CE.translateDomain emptyDomain defaultAction domain [CE.translateUncontrolled, CE.translateAction, CE.translateMethod]
                else translateDomain emptyDomain defaultAction domain typemap idUse $
                    snd $ optTranslation opts
    saveFile opts domFile $ show $ pddlDoc tdomain
    mapM_ (processProblem opts idUse domain) probFiles 
    return ()
