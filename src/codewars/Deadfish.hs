module Deadfish where

parse :: String -> [Int]
parse = reverse . fst . foldl _parse ([], 0)
    where 
    _parse (out, n) op 
        | op == 'i' = (out, n + 1)
        | op == 'd' = (out, n - 1)
        | op == 's' = (out, n * n)
        | otherwise = (n : out, n)


-- i increments the value (initially 0)
-- d decrements the value
-- s squares the value
-- o outputs the value into the return array