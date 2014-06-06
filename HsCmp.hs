module HsCmp (
      cmp
    , cmp'
    , compareF
    , CmpResult (..)
    )
where

import System.IO.MMap
import Data.ByteString
import Data.Word
import Data.Word8

data CmpResult = Same | Differ {
      byte_cnt :: Int
    , line_cnt :: Int
    }

instance Show CmpResult where
    show Same = ""
    show x = "differ: byte "
             ++ show (byte_cnt x)
             ++ ", line "
             ++ show (line_cnt x)

instance Eq CmpResult where
    Same == Same = True
    Differ x1 y1 == Differ x2 y2 = x1==x2 && y1==y2
    _ == _ = False

cmp' :: Int -> Int -> [Word8] -> [Word8] -> CmpResult
cmp' byte_cnt line_cnt [] []  = Same
cmp' byte_cnt line_cnt (x:xs) (y:ys)
    | x == _lf && x==y = cmp' (byte_cnt+1) (line_cnt+1) xs ys
    | x == y = cmp' (byte_cnt+1) line_cnt xs ys
    | otherwise = Differ byte_cnt line_cnt

cmp xs ys = cmp' 1 1 (unpack xs) (unpack ys)

compareF :: FilePath -> FilePath -> IO CmpResult
compareF x y = do
    xs <- mmapFileByteString x Nothing
    ys <- mmapFileByteString y Nothing

    return $ cmp xs ys
