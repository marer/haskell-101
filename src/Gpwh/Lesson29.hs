module Gpwh.Lesson29 where
import Data.List

a :: [Int]
a = pure (6+) <*> pure 1

b :: Maybe Int
b = pure (6+) <*> pure 1

c :: IO Int
c = pure (6+) <*> pure 1

d :: [Int]
d = pure (+) <*> [1000,2000,3000] <*> [500,20000]

--- generating prime numbers
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)

--- generating test data
data User = User {
     name :: String
   , gamerId :: Int
   , score :: Int
   } deriving Show

testNames :: [String]
testNames = ["Stefan", "Roman", "Zofia"]

testIds :: [Int]
testIds = [1 .. 3]

testScores :: [Int]
testScores = [10, 20 .. 50]

testUsers :: [User]
testUsers = pure User <*> testNames <*> testIds <*> testScores

--- exercises
-- Q29.1
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap fn = (pure fn <*>)

-- Q29.2
example :: Int
example = (*) ((+) 2 4) 6

exampleTranslated :: Maybe Int
exampleTranslated = pure (*) <*> pure ((+) 2 4) <*> pure 6

-- Q29.3


bought = [6, 12]
drank = [4]
people = [4, 5]
averageUsage = [3, 4]

stock = pure (-) <*> bought <*> drank
usage = pure (*) <*> people <*> averageUsage
needed = sort $ pure (-) <*> usage <*> stock


