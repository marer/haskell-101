module Lesson9 where
import Data.Char

x = ["foo", "bar", "baz", "omg"]

myFoldl s = foldl (++) "" s

myFoldr s = foldr (++) "" s

myElem a =  (<) 0 . length . filter (== a)

myIsPalindrome xs = words == reverse words
    where words = filter isLetter $ map toLower xs