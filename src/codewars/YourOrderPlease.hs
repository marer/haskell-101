module Kata.YourOrderPlease where

import Data.List (intercalate, sortBy)
import Data.Char (isNumber)
import Data.Text (pack, unpack, split)

yourOrderPlease :: String -> String
yourOrderPlease str = intercalate " " sorted
    where
        words = fmap unpack $ split (==' ') $ pack str
        numbers = fmap (filter isNumber) words
        sorted = fmap snd $ sortBy (\(a,_) (b,_) -> compare a b) $ zip numbers words