module Main where

import HsCmp
import Data.String.Utils
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import System.Posix.Files


usage = do
    prog <- getProgName
    putStrLn $ "Usage: " ++ prog ++ " [-s] file1 file2"

data Args = Silent | File String

instance Eq Args where
    Silent == Silent = True
    File x == File y = x == y
    _ == _ = False

silencers = ["-s", "--silent", "--quiet"]

parseArgs [] = []
parseArgs (a:args)
    | a `elem` silencers = parseArgs args ++ [Silent]
    | head a == '-' = error $ "Unknown argument " ++ a
    | otherwise = parseArgs args ++ [File a]

files [] = []
files (a:args) = case a of
    File x -> files args ++ [x]
    otherwise -> files args

hasSilent = elem Silent

runCompare True  = optimizedCompare
runCompare False = normalCompare False

optimizedCompare xs = do
    stats <- mapM getFileStatus xs
    let inodes = inodes' stats
    let sizes  = sizes' stats
    shortCircuit ExitSuccess $ head inodes == last inodes
    shortCircuit (ExitFailure 1) $ head sizes  /= last sizes

    normalCompare True xs
  where
    inodes' = map fileID
    sizes'  = map fileSize
    shortCircuit rc True  = exitWith rc
    shortCircuit rc False = return ()

normalCompare silent xs = do
    result <- tryIOError $ compareF (head xs) (last xs)
    print silent result
  where
    print False (Left err)   = do
                               hPrint stderr err
                               print False (Left err)
    print True  (Left err)   = exitWith $ ExitFailure 2
    print _     (Right Same) = exitSuccess
    print False (Right d)    = do
        hPutStrLn stderr $ join " " $ xs ++ [show d]
        print True (Right d)
    print True (Right d)     = exitWith $ ExitFailure 1

main = do
    args <- getArgs
    let pargs = parseArgs args
    usage_or_run (hasSilent pargs) $ files pargs
  where
    usage_or_run silent xs
        | length xs == 2 = runCompare silent xs
        | otherwise      = usage
