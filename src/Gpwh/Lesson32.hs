module Gpwh.Lesson32 where
import Control.Monad (guard)
import Data.Char (toUpper)

-- list as monad

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x) [1 .. n]

powersOfTwoMonad :: Int -> [Int]
powersOfTwoMonad n = do
    x <- [1 .. n]
    return (2^x)

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
    evenValue <- [2,4 .. n]
    oddValue <- [1,3 .. n]
    return (evenValue,oddValue)
    
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard(even value)
    return value   

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = do
    x <- xs
    guard(f x)
    return x

-- list comprehensions

powersOfTwoLC :: Int -> [Int]
powersOfTwoLC n = [ 2^value | value <- [1 .. n] ]

powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = [(powersOfTwo,powersOfThree)
                        | value <- [1 .. n]
                        , let powersOfTwo = 2^value
                        , let powersOfThree = 3^value]

evensGuardLC :: Int -> [Int]
evensGuardLC n = [ value | value <- [1 .. n], even value]

-- QC 32.3
capitalized :: [String]
capitalized = [ name | 
    word <- ["brown","blue","pink","orange"], 
    let name = mconcat ["Mr. ", [(toUpper . head) word], tail word]]

-- Q 31.1
months :: [(Int, Int)]
months = zip [1 .. ] [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

dates :: [String]
dates = [date | (month, days) <- months, day <- [1 .. days],
    let date = mconcat [show month, "-", show day]]

-- Q 31.2
datesDo :: [String]
datesDo = do
    (month, days) <- months
    day <- [1 .. days]
    let date = mconcat [show month, "-", show day]
    return date

datesLambdas :: [String]
datesLambdas = months >>= 
    (\(month, days) -> [1 .. days] >>= 
        (\day -> 
            (\date -> return date) 
            (mconcat [show month, "-", show day])
        )
    )
