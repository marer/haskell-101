module Gpwh.Lesson28 where
import qualified Data.Map as Map

--- example 1: distance calculation

type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]
                
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
    where 
        rlat = toRadians lat
        rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where 
        (rlat1,rlong1) = latLongToRads coords1
        (rlat2,rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

coords :: String -> Maybe LatLong
coords city = Map.lookup city locationDB

mainDistance :: IO ()
mainDistance = do
    putStrLn "Enter first city"
    city1 <- getLine
    putStrLn "Enter second city"
    city2 <- getLine
    printDistance $ haversine <$> (coords city1) <*> (coords city2)
    

--- example 2: min of 3

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

mainMinOfThree :: IO ()
mainMinOfThree = do
   putStrLn "Enter three numbers"
   minInt <- minOfInts
   putStrLn (show minInt ++ " is the smallest")

--- example 3: data in context
data User = User
   { name :: String
   , gamerId :: Int
   , score :: Int
   } deriving Show

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId =  Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

maybeSue :: Maybe User
maybeSue = User <$> serverUsername <*> serverGamerId <*> serverScore

maybeNothing :: Maybe User
maybeNothing = User <$> Nothing <*> serverGamerId <*> serverScore