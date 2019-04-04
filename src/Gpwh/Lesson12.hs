module Gpwh.Lesson12 where

type FirstName = String
type Foo = String -> Bool

data Sex = Male | Female

checkSex :: Sex -> Char
checkSex s = case s of
    Male -> 'M'
    Female -> 'F'

data Person = Person {
    firstname :: String,
    lastname :: String,
    sex :: Sex
}
stefan = Person { firstname = "Stefan", lastname = "Krewężnik", sex = Male }
