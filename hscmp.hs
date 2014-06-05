import System.Environment
import Data.String.Utils
import System.IO.MMap

compareF :: FilePath -> FilePath -> IO Bool
compareF x y = do
    xs <- mmapFileByteString x Nothing
    ys <- mmapFileByteString y Nothing

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
