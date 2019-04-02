module ToWeirdCase where
import Data.Char

toWeirdCase :: String -> String
toWeirdCase = unwords . map processWord . words
    where 
        processWord = map (\(i, l) -> if mod i 2 == 0 then toUpper l else toLower l ) . zip [0..]

foo :: String -> String
foo = zipWith ($) (cycle [toUpper, toLower, toLower])