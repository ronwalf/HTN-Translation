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
import HTNTranslation.ProgressionBounds as PB

data Options = Options
    { optADL :: Bool
    , optNumIds :: Int
    , optLift :: Maybe (Expr (Atomic ConstTermExpr))
    , optPostfix :: String
    , optVerbose :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optADL = False
    , optNumIds = 0
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
    , Option ['a'] ["adl"]
        (NoArg (\opts -> return $ opts { optADL = True }))
        "Use an ADL-compatible translation (used derived predicates)"
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


processProblem :: Options -> StandardHTNDomain -> String -> IO Int
processProblem opts domain fname = do
    contents <- readFile fname
    problem <- errCheck $ parseHTNProblem fname contents
    let lifted = maybe problem (flip liftProblem problem) $ optLift opts
    numIds <- if (optNumIds opts > 0)
        then return (optNumIds opts)
        else do
            bounds <- boundProgression domain lifted
            when (optVerbose opts) $ do
                putStrLn $ "Problem " ++ getName lifted ++ " task bounds: " ++ show bounds
            return $ snd $ head bounds
    let problem' = if (optADL opts)
            then ATrans.translateProblem emptyProblem numIds lifted
                else translateProblem emptyProblem numIds lifted
    saveFile opts fname $ show $ pddlDoc problem'
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
    domain <-  (errCheck $ parseHTNPDDL domFile domContents) >>= tailRec opts
    when (optVerbose opts) $ do
        putStrLn "Parsed domain:"
        putStrLn $ show domain
        putStrLn ""
    numIds <- liftM (maximum . (1:)) $ mapM (processProblem opts domain) probFiles 
    let tdomain = if (optADL opts) 
            then ATrans.translateDomain emptyDomain defaultAction domain 
                [ATrans.translateUncontrolled, ATrans.translateAction, ATrans.translateMethod]
            else translateDomain emptyDomain defaultAction domain numIds
                [translateUncontrolled, translateAction, translateMethod]
    saveFile opts domFile $ show $ pddlDoc tdomain
    return ()
    where
    tailRec :: Options -> StandardHTNDomain -> IO StandardHTNDomain
    tailRec opts domain =
        let tasks = filter (taskHasLooseEnds domain) $ tasksWithSuccessors domain in
        if (optADL opts || null tasks)
            then return domain
            else do
                when (optVerbose opts) $ do
                    putStrLn $ "Adding dummy last task for these tasks: " ++ show tasks
                let (dtask, dom') = insertDummy defaultMethod domain
                return $ foldl' (\dom task -> ensureLastTask dom dtask task) dom' tasks
        
