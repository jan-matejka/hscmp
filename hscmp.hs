module Main where

import HsCmp
import Data.String.Utils
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

usage prog = "Usage: " ++ prog ++ " [-s] file1 file2"

data Args = Silent | File String

instance Eq Args where
    Silent == Silent = True
    File x == File y = x == y
    _ == _ = False

parseArgs [] = []
parseArgs (a:args)
    | any ((==) a) ["-s", "--silent", "--quiet"] = parseArgs args ++ [Silent]
    | head a == '-' = error $ "Unknown argument " ++ a
    | otherwise = parseArgs args ++ [File a]

files [] = []
files (a:args) = case a of
    File x -> files args ++ [x]
    otherwise -> files args

has_silent pargs = any ((==) Silent) pargs

printResult Same _  _  = exitWith ExitSuccess
printResult d True  _  = exitWith $ ExitFailure 1
printResult d False fs = do
    hPutStrLn stderr $ join " " $ fs ++ [show d]
    printResult d True fs

main = do
    args <- getArgs
    let pargs = parseArgs args
    let files' = files pargs

    if length files' /= 2
        then do
            prog <- getProgName
            putStrLn $ usage prog
        else do
            let x = head files'
                y = last files'
            result <- tryIOError $ compareF x y
            case result of
                Left err -> do
                    hPrint stderr err
                    exitWith $ ExitFailure 2
                Right d -> case d of
                    Same -> exitWith ExitSuccess
                    Differ _ _ -> printResult d (has_silent pargs) [x,y]
