module Gpwh.Lesson33 where
import Control.Monad (guard)
import Control.Applicative

-- types

data Name = Name
    { firstName :: String
    , lastName :: String }

instance Show Name where
    show (Name first last) = mconcat [first," ",last]

data GradeLevel = Freshman | Sophmore | Junior | Senior deriving (Eq,Ord,Enum,Show)

data Student = Student
    { studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name } deriving Show

data Teacher = Teacher
    { teacherId :: Int
    , teacherName :: Name } deriving Show

data Course = Course
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int } deriving Show

data Enrollment = Enrollment
    { student :: Int
    , course :: Int } deriving Show

-- data

students :: [Student]
students = [ Student 1 Senior (Name "Audre" "Lorde")
            ,Student 2 Junior (Name "Leslie" "Silko")
            ,Student 3 Freshman (Name "Judith" "Butler")
            ,Student 4 Senior (Name "Guy" "Debord")
            ,Student 5 Sophmore (Name "Jean" "Baudrillard")
            ,Student 6 Junior (Name "Julia" "Kristeva")]

teachers :: [Teacher]
teachers = [Teacher 100 (Name "Simone" "De Beauvior")
           ,Teacher 200 (Name "Susan" "Sontag")]

courses :: [Course]
courses = [Course 101 "French" 100
          ,Course 201 "English" 200]        

enrollments :: [Enrollment]
enrollments = [(Enrollment 1 101)
            ,(Enrollment 2 101)
            ,(Enrollment 2 201)
            ,(Enrollment 3 101)
            ,(Enrollment 4 201)
            ,(Enrollment 4 101)
            ,(Enrollment 5 101)
            ,(Enrollment 6 201) ]          
-- api

_select :: Monad m => (a -> b) -> m a -> m b
_select prop vals = do
    val <- vals
    return (prop val)

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test vals = do
    val <- vals
    guard (test val)
    return val

startsWith :: Char -> String -> Bool
startsWith char string = char == head string

_join :: (Monad m, Alternative m, Eq c) => m a -> m b  -> (a -> c) -> (b -> c) -> m (a, b) 
_join xs ys propx propy = do
    x <- xs
    y <- ys
    guard (propx x == propy y)
    return (x, y)

_hinq selectQuery joinQuery whereQuery = 
    (\joinData -> 
        (\whereResult -> selectQuery whereResult) 
        (whereQuery joinData)
    ) joinQuery

-- _select (firstName . studentName) students
-- _select (\x -> (studentName x, gradeLevel x)) students

-- _where (startsWith 'J' . firstName) (_select studentName students)

englishTeachers :: [Name]
englishTeachers = _hinq 
    (_select (teacherName . fst)) 
    (_join teachers courses teacherId teacher) 
    (_where ((== "English" ). courseTitle . snd))

-- generic HINQ

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)
                -- | HINQ_EMTPY

-- instance Semigroup HINQ where
--     (<>) (HINQ sc1 jc1 wc1) (HINQ sc2 jc2 wc2) = HINQ ()
--     (<>) (HINQ sc1 jc1 wc1) (HINQ_ sc2 jc2) = HINQ ()
--     (<>) (HINQ_ sc1 jc1) (HINQ sc2 jc2 wc2) = HINQ ()
--     (<>) (HINQ_ sc1 jc1) (HINQ_ sc2 jc2) = HINQ_ ()
--     (<>) HINQ_EMTPY h = h
--     (<>) h HINQ_EMTPY = h

-- instance Monoid HINQ where
--     mempty = HINQ_EMTPY
--     mappend = (<>)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

-- queries 

query1 :: HINQ [] (Teacher, Course) Name
query1  = HINQ (_select (teacherName . fst))
               (_join teachers courses teacherId teacher)
               (_where ((== "English") .courseTitle . snd))

query2 :: HINQ [] Student String
query2 = HINQ_ (_select (firstName . studentName)) students

result = runHINQ query1

-- Maybe monad
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher,Course) Name
maybeQuery1  = HINQ (_select (teacherName . fst))
                    (_join possibleTeacher possibleCourse teacherId teacher)
                    (_where ((== "French") .courseTitle . snd))

-- multiple joins
studentEnrollmentsQ = HINQ_ (_select (\(st,en) -> (studentName st, course en)))
                            (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ = HINQ  (_select (fst . fst))
                         (_join studentEnrollments courses snd courseId)
                         (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName =  runHINQ courseQuery
 where courseQuery = HINQ  (_select (fst . fst))
                           (_join studentEnrollments courses snd courseId)
                           (_where ((== courseName) . courseTitle . snd))