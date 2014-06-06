module Main where

import HsCmp
import Data.String.Utils
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

usage = join "\n" [
      "Usage: $0 file1 file2"
    ]

main = do
    args <- getArgs
    if length args /= 2
        then putStrLn usage
        else do
            let x = head args
                y = last args
            result <- tryIOError $ compareF x y
            case result of
                Left err -> do
                    hPrint stderr err
                    exitWith $ ExitFailure 2
                Right d -> case d of
                    Same -> exitWith ExitSuccess
                    Differ _ _ -> do
                        hPutStrLn stderr $ x ++ " " ++ y ++ " " ++ show d
