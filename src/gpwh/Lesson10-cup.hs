module Lesson10Cup where

cup f10z = \message -> message f10z
getOz aCup = aCup (\f10z -> f10z)

-- drink aCup ozDrank = aCup (\f10z -> cup ( f10z - ozDrank ))
drink aCup ozDrank = cup (max ( getOz aCup - ozDrank )  0)
isEmpty = (== 0) . getOz 

---- 

coffeMug = cup 12
half = drink coffeMug 7
almostEmpty = drink coffeMug 13
afterManySips = foldl drink coffeMug [1,2,1,5]