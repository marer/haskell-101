module Codewars.DblLinear where

dblLinear :: Int -> Integer
dblLinear n =  head $ drop n $ process [1] 0 0
    where
    fy x = 2 * x + 1
    fz x = 3 * x + 1
    process acc iy iz
        | y < z     = x : process (y : acc) iy (iz + 1)
        | y > z     = x : process (z : acc) (iy + 1) iz
        | otherwise = x : process (y : acc) iy iz
        where
        x = head acc
        y = fy $ (!!) acc iy
        z = fz $ (!!) acc iz

