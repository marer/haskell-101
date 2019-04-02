module Codewars.DecodeMorseAdvanced where
-- import Kata.DecodeMorseAdvanced.Preload
import Data.Map.Strict ((!))
import Data.List (intercalate, dropWhileEnd, group)
import Data.List.Split (splitOn)
import Data.Char (isSpace)

decodeBits :: String -> String
decodeBits bits = mconcat $ zipWith decode groups lengths
    where 
    groups = group $ trim (=='0') bits
    lengths = map length groups
    blockSize = minimum lengths
    decode gr len
        | head gr == '1' && len == blockSize = "."
        | head gr == '1' = "-"
        | len == blockSize = ""
        | len == 3 * blockSize = " "
        | len == 7 * blockSize = "   "

    
-- decodeMorse :: String -> String
-- decodeMorse = unwords . map mapWord . splitOn "   " . trim isSpace
--     where
--     mapWord =  intercalate "" . map (morseCodes !) . words

trim :: (a -> Bool) -> [a] -> [a]
trim f = dropWhileEnd f . dropWhile f

sample = "1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011"
