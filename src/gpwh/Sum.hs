module Gpwh.Sum where
import System.Environment
import Control.Monad

-- main :: IO ()
-- main = do
--     args <- mapM (\_ -> getLine) [1..3]
--     mapM_ putStrLn args

ree :: IO ()
ree = do
 args <- getArgs
 let linesToRead = if length args > 0
                   then read (head args)
                   else 0
 numbers <- replicateM linesToRead getLine
 print "sum goes here"