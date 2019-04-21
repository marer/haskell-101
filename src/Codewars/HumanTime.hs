module HumanTime where

humanReadable :: Int -> String
humanReadable x = mconcat [pad hours, ":", pad minutes, ":", pad seconds]
    where
    (rest, seconds) = divMod x 60
    (hours, minutes) = divMod rest 60
    pad i = replicate (2 - length s) '0' ++ s where s = show i
    