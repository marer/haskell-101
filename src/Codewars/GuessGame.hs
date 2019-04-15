module Codewars.GuessGame (guess) where

guess :: Monad m => (Int -> m Bool) -> m Int
guess gt = guessInRange gt 1 100

guessInRange :: Monad m => (Int -> m Bool) -> Int -> Int -> m Int
guessInRange gt lo hi = do
    let x = div (hi - lo) 2 + lo
    check <- gt x
    if (x == lo || x == hi - 1) && check
        then return hi
    else if x == lo
        then return lo
    else if check
        then guessInRange gt (x+1) hi
    else guessInRange gt lo x




---- 73
-- 1 - 100 -> 50
-- 50 - 100 -> 75
-- 50 - 75 -> 62
-- 62 - 75 -> 68
-- 68 - 75 -> 71
-- 71 - 75 -> 73
-- 73 - 75 -> 74