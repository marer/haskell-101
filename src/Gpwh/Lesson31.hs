module Gpwh.Lesson31 where
import qualified Data.Map as Map

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM = (uncurry max <$>)

-- hello name classic

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name -> return (nameStatement name)) >>=
            putStrLn

-- hello name do

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn (nameStatement name)

-- echo
echo :: IO ()
echo = getLine >>= putStrLn

echoDo :: IO ()
echoDo = do
    line <- getLine
    putStrLn line

--- interview grading problem

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)
data Candidate = Candidate
   { candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
   where passedCoding = codeReview candidate > B
         passedCultureFit = cultureFit candidate > C
         educationMin = education candidate >= MS
         tests = [passedCoding, passedCultureFit, educationMin]

testCandidate :: Candidate
testCandidate = Candidate { 
    candidateId = 1,
    codeReview = A,
    cultureFit = A,
    education = PhD }
         
-- io grading

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return (Candidate { candidateId = cId
                     , codeReview = codeGrade
                     , cultureFit = cultureGrade
                     , education = degree })

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

-- maybe grading 

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1 , codeReview = A , cultureFit = A , education = BA }
candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2 , codeReview = C , cultureFit = A , education = PhD }
candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3 , codeReview = A , cultureFit = B , education = MS }
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1), (2,candidate2), (3,candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
   candidate <- Map.lookup cId candidateDB
   let passed = viable candidate
   let statement = if passed then "passed" else "failed"
   return statement

-- list grading
candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
   candidate <- candidates
   let passed = viable candidate
   let statement = if passed then "passed" else "failed"
   return statement

-- universal grading
assessCandidate :: Monad m =>  m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

-- assessCandidate readCandidate
-- assessCandidate (Map.lookup 1 candidateDB)
-- assessCandidate candidates

--- exercises
-- Q31.1
type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2 then p1 else p2
   where costP1 = costPerInch p1
         costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++ "is cheaper at " ++ show costSqInch ++ " per square inch"
    where costSqInch = costPerInch (size,cost)

mainSugar :: IO ()
mainSugar = do
   putStrLn "What is the size of pizza 1"
   size1 <- getLine
   putStrLn "What is the cost of pizza 1"
   cost1 <- getLine
   putStrLn "What is the size of pizza 2"
   size2 <-  getLine
   putStrLn "What is the cost of pizza 2"
   cost2 <- getLine
   let pizza1 = (read size1, read cost1)
   let pizza2 = (read size2, read cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   putStrLn (describePizza betterPizza)

mainDesugar :: IO ()
mainDesugar = putStrLn "What is the size of pizza 1" >> 
    getLine >>= 
    (\size1 -> 
        putStrLn "What is the cost of pizza 1" >> 
        getLine >>= 
        (\cost1 -> 
            putStrLn "What is the size of pizza 2" >> 
            getLine >>=
            (\size2 -> 
                putStrLn "What is the cost of pizza 2" >>
                getLine >>=
                (\cost2 -> 
                    (\pizza1 -> 
                        (\pizza2 -> 
                            (\betterPizza -> 
                                (\description -> putStrLn description)
                                (describePizza betterPizza)
                            )
                            (comparePizzas pizza1 pizza2)                            
                        )
                        (read size2, read cost2)
                    ) 
                    (read size1, read cost1)
                )
            )
        )
    ) 
    
-- Q31.2
listMain :: [(Double, Double, Double, Double)] -> [String]
listMain pizzas = do
   (size1, cost1, size2, cost2) <- pizzas
   let pizza1 = (size1,cost1)
   let pizza2 = (size2,cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)

-- Q31.3
monadMain :: Monad m => m (Double, Double, Double, Double) -> m String
monadMain pizzas = do
   (size1, cost1, size2, cost2) <- pizzas
   let pizza1 = (size1,cost1)
   let pizza2 = (size2,cost2)
   let betterPizza = comparePizzas pizza1 pizza2
   return  (describePizza betterPizza)