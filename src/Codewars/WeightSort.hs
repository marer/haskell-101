module Codewars.WeightSort where
import Data.List (sortBy)
import Data.Char (digitToInt)

orderWeight :: [Char] -> [Char]
orderWeight = unwords . map fst . sortBy compareWeights . map tokenize . words
    where
        tokenize w = (w, foldl (\acc c -> (+) acc $ digitToInt c ) 0 w)
        compareWeights (w1, t1) (w2, t2)
            | t1 > t2 = GT
            | t1 < t2 = LT
            | t1 == t2 = compare w1 w2