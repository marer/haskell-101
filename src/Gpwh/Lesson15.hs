module Gpwh.Lesson15 where

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN asize c = toEnum $ flip mod asize $ (+) (div asize 2) $ fromEnum c

data FourLetters = L1 | L2 | L3 | L4 deriving (Enum, Bounded, Show)