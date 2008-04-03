module Main where

import System.Environment
import System.IO

import HTNTranslation.HTNPDDL
import HTNTranslation.Translation

processDomain domfile = do
    contents <- readFile domfile
    printResult $ parseHTNPDDL domfile contents
    where
        printResult (Left err) = print err
        printResult (Right dom) = do
            print dom
            print "--------"
            domain <- translate dom
            print domain

main = do
    args <- getArgs
    sequence_ [processDomain domfile | domfile <- args]
