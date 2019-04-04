module Codewars.CreatePhoneNumber where
import Data.Char (intToDigit)

createPhoneNumber :: [Int] -> String
createPhoneNumber l = "(" ++ [a, b, c] ++ ") " ++ [d, e, f] ++ "-" ++ rest
  where
    a:b:c:d:e:f:rest = fmap intToDigit l