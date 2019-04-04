module Gpwh.Lesson6 where


rpt x = x : rpt ( x + 1 )

subseq s e =  take ( e - s )  . drop s

inFirstHalf el lst = elem el firstHalf
    where
        firstHalf = take ( div ( length lst ) 2 ) lst