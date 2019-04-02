module Lesson17 where
import Data.Semigroup
import Data.List
import Data.Monoid

data Color = Red | Yellow | Blue | Purple | Orange | Green | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Yellow = Orange
    (<>) Yellow Red = Orange
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Blue Yellow = Green
    (<>) Yellow Blue = Green
    (<>) a b 
        | a == b = a
        | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
        | all (`elem` [Yellow, Blue, Orange]) [a, b] = Orange
        | all (`elem` [Yellow, Blue, Green]) [a, b] = Green
        | otherwise = Brown
        