module Lesson18 where
import qualified Data.Map.Strict as Map

data Organ = Heart | Liver | Spleen | Brain | Leg deriving (Eq, Show, Ord)

organs = [Heart, Heart, Spleen, Liver, Brain]
ids = [3,4,12,45,19]

inventory = Map.fromList $ zip ids organs

supplies = Map.fromList [(Heart, 10), (Spleen, 1), (Liver, 4), (Brain, 0)]

------

data Container = Vat Organ | Cooler Organ | Bag Organ
data Location = Lab | Kitchen deriving Show

instance Show Container where
    show (Vat organ) = show organ <> " in a vat"
    show (Bag organ) = show organ <> " in a bag"
    show (Cooler organ) = show organ <> " in a cooler"

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (l, c) = show c <> " in the " <> show l

processRequest :: Int -> String
processRequest drawer = case  Map.lookup drawer inventory of
    Just organ -> report $ process organ
    Nothing -> "nothing found"