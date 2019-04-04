module Codewars.FizzBuzzCuckoo where

import Data.List

fizzBuzzCuckooClock :: String -> String
fizzBuzzCuckooClock time
    | minutes == 0  = intercalate " " ( replicate times "Cuckoo" )
    | minutes == 30 = "Cuckoo"
    | by3 && by5    = "Fizz Buzz"
    | by3           = "Fizz"
    | by5           = "Buzz"
    | otherwise     = "tick"
    where
        hours = mod ( read $ take 2 time ) 12
        times = if hours > 0 then hours else 12
        minutes = read $ take 2 $ drop 3 time :: Int
        by3 = mod minutes 3 == 0
        by5 = mod minutes 5 == 0

