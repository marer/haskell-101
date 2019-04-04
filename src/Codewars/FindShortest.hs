module Codewars.FindShortest where
import Data.List (sortBy)

find_shortest :: String -> Integer
find_shortest = head . sortBy compare . map ( toInteger . length ) . words