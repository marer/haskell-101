module Codewars.Isomorph (isomorph) where
import Data.List (group)

isomorph :: String -> String -> Bool
isomorph w1 w2 = digest w1 == digest w2

digest :: String -> [(Elem, Int)]
digest = map (\e -> (head e, length e)) . group . tokenize 0 . map Left

tokenize :: Int -> [Elem] -> [Elem]
tokenize _ [] = []
tokenize i (c : cs) = case c of
    Left x -> tokenize (i + 1) $ replace_ c (Right i) (c : cs)
    Right _ -> c : tokenize i cs

replace_ :: Elem -> Elem -> [Elem] -> [Elem]
replace_ x y = map (\c -> if c == x then y else c)

type Elem = Either Char Int