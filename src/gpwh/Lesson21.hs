module Gpwh.Lesson21 where
import System.Random

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
   putStrLn "Hello! What's your name?"
   name <- getLine
   let statement = helloPerson name
   putStrLn statement
