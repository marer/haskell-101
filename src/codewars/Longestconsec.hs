module Codewars.Longestconsec where

longestConsec :: [String] -> Int -> String
longestConsec strarr k
        | k == 0 || strarr == [] || length strarr < k  = ""
        | length next > length current = next
        | otherwise = current
    where
        current =  concat $ take k strarr
        next = longestConsec (tail strarr) k