module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.FilePath
    

ifNotExists f d m = do
    exists <- doesFileExist f
    if not exists then m else return d
        
readLines f = do
    exists <- doesFileExist f
    if exists then
            return lines `ap` readFile f
        else
            return []

groupHeader = ["Subject", "Rooms", "Packages", "Combined", "Real", "User", "System"]
groupLines subj treatment times = 
    let
        (rooms, trest) = span isDigit $ dropWhile (not . isDigit) treatment
        pkgs = dropWhile (not . isDigit) trest
        combined = show $ (read rooms + read pkgs :: Double)
        real = typeTimes "real" times
        sys = typeTimes "sys" times
        usr = typeTimes "user" times
        trials = maximum [length real, length sys, length usr]
    in
    transpose [
        replicate trials subj, 
        replicate trials rooms,
        replicate trials pkgs,
        replicate trials combined,
        real, usr, sys]
    where
        typeTimes str = map (drop (length str + 1)) . filter (isPrefixOf str)

collectResults results dir = do
    let ffhTimeFile = joinPath [dir, "times.hpddl"]
    let ffnhTimeFile = joinPath [dir, "times.pddl"]
    let ffhBadFile = joinPath [dir, "BADRUN.hpddl"]
    let ffnhBadFile = joinPath [dir, "BADRUN.pddl"]
    ffh <- readResults "ff-h" ffhBadFile ffnhBadFile ffhTimeFile
    ffnh <- readResults "ff-nh" ffnhBadFile ffhBadFile ffnhTimeFile
    return $ ffh ++ ffnh ++ results
    where
        readResults subj badFile1 badFile2 timeFile = do
            rLines <- ifNotExists badFile1 [] $
                    -- ifNotExists badFile2 [] $
                    (readLines timeFile)
            return $ groupLines subj dir rLines
            

main = do
    directories <- getArgs
    results <- foldM collectResults [] directories
    mapM_ (putStrLn . intercalate " ") $ groupHeader : results
    return () 

