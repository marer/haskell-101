module Lesson3 where

sumSquareOrSquareSum x y = max (x^2 + y^2)  ((x + y)^ 2)

ifEven fn x = if even x then fn x else x

compareLastNames a1 a2 = compare s1 s2
    where 
        s1 = snd a1
        s2 = snd a2

names = [
    ("John", "Smith"),
    ("Ronald", "Regan"),
    ("Rick","James")]

