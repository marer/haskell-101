module Gpwh.SumLazy where
import Control.Monad
import System.Environment


main :: IO ()
main = do
  userInput <- getContents
  let ints = toInts userInput
  print (sum ints)

toInts :: String -> [Int]
toInts = map read . lines