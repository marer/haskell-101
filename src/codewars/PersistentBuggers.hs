module Codewars.PersistentBuggers where
import Data.Char (digitToInt)

persistence :: Int -> Int
persistence n = calculate (0, n)
    where calculate (i, n)
            | n < 10 = i
            | otherwise = calculate (i + 1, product $ map digitToInt $ show n )
