module HsCmp (
      cmp
    , cmp'
    , compareF
    , CmpResult (..)
    )
where

import Prelude hiding (lines)
import System.IO.MMap
import Data.ByteString
import Data.Word
import Data.Word8

data CmpResult = Same
               | Differ{bytes :: Int, lines :: Int }

instance Show CmpResult where
    show Same = ""
    show x = "differ: byte "
             ++ show (bytes x)
             ++ ", line "
             ++ show (lines x)

instance Eq CmpResult where
    Same == Same = True
    Differ x1 y1 == Differ x2 y2 = x1==x2 && y1==y2
    _ == _ = False

cmp' :: Int -> Int -> [Word8] -> [Word8] -> CmpResult
cmp' bytes lines [] []  = Same
cmp' bytes lines (x:xs) (y:ys)
    | x == _lf && x==y = cmp' (bytes+1) (lines+1) xs ys
    | x == y = cmp' (bytes+1) lines xs ys
    | otherwise = Differ bytes lines

cmp xs ys = cmp' 1 1 (unpack xs) (unpack ys)

compareF :: FilePath -> FilePath -> IO CmpResult
compareF x y = do
    xs <- mmapFileByteString x Nothing
    ys <- mmapFileByteString y Nothing

    return $ cmp xs ys
