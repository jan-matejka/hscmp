import System.Environment
import Data.String.Utils
import System.IO.MMap
import System.IO.Error
import System.IO
import System.Exit

compareF :: FilePath -> FilePath -> IO ()
compareF x y = do
    xs <- mmapFileByteString x Nothing
    ys <- mmapFileByteString y Nothing

    exitWith $ rc $ compare xs ys
        where
            rc :: Ordering -> ExitCode
            rc EQ = ExitSuccess
            rc _  = ExitFailure 1

usage = join "\n" [
      "Usage: $0 file1 file2"
    ]

main = do
    args <- getArgs
    if length args /= 2
        then putStrLn usage
        else do
            result <- tryIOError $ compareF (head args) (last args)
            case result of
                Left err -> do
                    hPrint stderr err
                    exitWith $ ExitFailure 2
                Right x -> return x
