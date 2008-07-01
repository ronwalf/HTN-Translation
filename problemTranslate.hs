module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO

import HTNTranslation.Translation
import Planning.PDDL.Parser
import Planning.PDDL.Representation

processProblem items arity domfile = do
    contents <- readFile domfile
    printResult $ runParser stdProblemParser emptyProblem domfile contents
    where
        printResult (Left err) = print err
        printResult (Right dom) = do
            --print dom
            --print "--------"
            let problem = translateProblem items arity dom :: StandardProblem
            print problem 

main = do
    stackItems:stackArity:args <- getArgs
    sequence_ [
        processProblem
            (read stackItems :: Int)
            (read stackArity :: Int) 
            domfile 
        | domfile <- args]
