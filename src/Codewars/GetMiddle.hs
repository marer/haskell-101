module Codewars.GetMiddle where


getMiddle :: String -> String
getMiddle s = take t . ( drop d ) $ s
  where
    len = length s
    t = 2 - ( mod len 2 )
    d = ( len - 1 ) `div` 2