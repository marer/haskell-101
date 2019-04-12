module Gpwh.Lesson30 where

import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int
    
userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                        ,(1002,2)
                        ,(1003,3)
                        ,(1004,4)
                        ,(1005,5)
                        ,(1006,6)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromIdStrange :: Int -> Maybe (Maybe Int)
creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

creditsFromWilCoId :: WillCoId -> Maybe PlayerCredits
creditsFromWilCoId id = lookupGamerId id >>= lookupUserName  >>= lookupCredits
    
-- echo

echo :: IO ()
echo = putStrLn "Enter text" >> getLine >>= putStrLn . ("Ola mówi: " ++)


-- hello fn
askForName :: IO ()
askForName = putStrLn "What's your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

hello :: IO ()
hello = askForName >> getLine >>= (\name -> return (nameStatement name)) >>= putStrLn

-- Q30.1
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f xs = xs >>= return . f

-- Q30.2
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp func v = func >>= (\f -> v >>= (\x -> return (f x)))

-- Q30.3
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) fn = fn a

