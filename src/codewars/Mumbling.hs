module Codewars.Mumbling where

import Data.Char
import Data.List

accum :: [Char] -> [Char]
accum =
  intercalate "-" . fmap formatPart . (zip [0..])
  where
    formatPart (i, c) = toUpper(c) : replicate i ( toLower c )
