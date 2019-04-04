module Codewars.Hashtag where
import Data.Char (isLetter, toUpper)
import Data.List (intercalate)

generateHashtag :: String -> Maybe String
generateHashtag = format . generate
    where
    format str
        | str == "#"       = Nothing
        | length str > 140 = Nothing
        | otherwise        = Just str
    generate = (:) '#' . intercalate "" . map capitalize . map (filter isLetter) . words
    capitalize = zipWith ($) (toUpper : cycle [id])

