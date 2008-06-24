module Main where

import System.Environment
import System.IO

import HTNTranslation.HTNPDDL
import HTNTranslation.Translation

processDomain arity domfile = do
    contents <- readFile domfile
    printResult $ parseHTNPDDL domfile contents
    where
        printResult (Left err) = print err
        printResult (Right dom) = do
            print dom
            print "--------"
            let domain = translateDomain arity dom :: Domain (Expr (Action GoalExpr))
            print domain

main = do
    stackArity:args <- getArgs
    sequence_ [
        processDomain 
            (read stackArity :: Int) 
            domfile 
        | domfile <- args]
