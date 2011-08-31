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

import Planning.PDDL.Parser (pddlExprLexer, atomicParser)

import HTNTranslation.HTNPDDL
import HTNTranslation.HTNProblemLift
import HTNTranslation.Translation
import HTNTranslation.Typing

type TranslationDef = ([TaskIdUseFunc Maybe], [StandardMethod -> StateT (PDDLDomain, TranslationData StandardHTNDomain PDDLAction)  Maybe ()])

basicTranslation :: TranslationDef
basicTranslation = ([], [translateUncontrolled, translateAction, translateMethod [translateTask]])

idTranslation :: TranslationDef
idTranslation = 
    ([useAtomicId, usePLastId]
    ,[translateUncontrolled, translateAction, translateMethod [translateTask]])

optimizedTranslation :: TranslationDef
optimizedTranslation = 
    ([useAtomicId, usePLastId]
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
    { optNumIds :: Int
    , optTranslation :: TranslationDef
    , optLift :: Maybe (Expr (Atomic ConstTermExpr))
    , optPostfix :: String
    , optVerbose :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optNumIds = 1
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
         "number of identifiers"
    , Option ['l'] ["lift"]
        (ReqArg (\taskstr opts -> do
            task <- errCheck $ runParser taskParser () taskstr taskstr
            return $ opts { optLift = Just task })
        "TASK")
        "lift standard PDDL problems into HTNPDDL"
    , Option ['o'] ["opt"]
        (OptArg (\ level opts -> return $ 
            opts { optTranslation = maybe (snd $ last translations) (fromJust . flip lookup translations) level})
         "LEVEL")
        (intercalate ", " (map fst translations))
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
    let oname = takeWhile (/= '.') name ++ optPostfix opts
    writeFile oname contents
   

errCheck :: (Show t) => Either t b -> IO b
errCheck (Left err) = do
    hPrint stderr err
    exitFailure 
errCheck (Right prob) = return prob


taskParser :: CharParser a (Expr (Atomic ConstTermExpr))
taskParser = parens pddlExprLexer $
    atomicParser pddlExprLexer $ constTermParser pddlExprLexer


processProblem :: Options -> TaskIdUse -> String -> IO ()
processProblem opts useId fname = do
    contents <- readFile fname
    problem <- errCheck $ parseHTNProblem fname contents
    let problem' = translateProblem emptyProblem useId (optNumIds opts) $
            maybe problem (flip liftProblem problem) $ optLift opts
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
    let tdomain = translateDomain emptyDomain defaultAction domain typemap idUse $
            snd $ optTranslation opts
    saveFile opts domFile $ show $ pddlDoc tdomain
    mapM_ (processProblem opts idUse) probFiles 
    return ()
