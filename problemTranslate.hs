{-# OPTIONS
 -fcontext-stack=40
#-}
module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import System.IO

import HTNTranslation.Translation
import Planning.PDDL.PDDL3_0

processProblem items arity domfile = do
    contents <- readFile domfile
    printResult $ runParser pddlProblemParser emptyProblem domfile contents
    where
        printResult (Left err) = print err
        printResult (Right dom) = do
            --print dom
            --print "--------"
            let problem = translateProblem items arity dom
            print problem 

main = do
    stackArity:stackItems:args <- getArgs
    sequence_ [
        processProblem
            (read stackItems :: Int)
            (read stackArity :: Int) 
            domfile 
        | domfile <- args]
