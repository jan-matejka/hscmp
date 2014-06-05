import System.Environment
import System.IO
import Data.String.Utils

compareF :: FilePath -> FilePath -> IO Bool
compareF x y = do
    hx <- openBinaryFile x ReadMode
    hy <- openBinaryFile y ReadMode

    xs <- hGetContents hx
    ys <- hGetContents hy

    return $ compare xs ys == EQ

usage = join "\n" [
      "Usage: $0 file1 file2"
    , ""
    , "prints True if files do not differ. Otherwise False"
    ]

main = do
    args <- getArgs
    if length args /= 2
        then putStrLn usage
        else do
            x <- compareF (head args) (last args)
            print x
