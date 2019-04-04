module Codewars.DontGiveMeFive where

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = length $ filter (notElem '5' . show) [start..end]