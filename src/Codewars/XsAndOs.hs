module Codewars.XsAndOs where

import Data.Char

xo :: String -> Bool
xo str = length xs == length os
    where
        lw = map toLower str
        os = filter (\c -> c == 'o') lw
        xs = filter (\c -> c == 'x') lw