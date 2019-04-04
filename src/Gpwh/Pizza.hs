module Gpwh.Pizza where

main :: IO ()
main = do
    putStrLn "What is the size of pizza 1"
    p1size <- getLine
    putStrLn "What is the cost of pizza 1"
    p1cost <- getLine
    putStrLn "What is the size of pizza 2"
    p2size <- getLine
    putStrLn "What is the cost of pizza 2"
    p2cost <- getLine
    let pizza1 = (read p1size, read p1cost)
    let pizza2 = (read p2size, read p2cost)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn ( describePizza betterPizza )

type Pizza = (Double,Double)

area :: Double -> Double
area diameter = pi * (diameter / 2) ^ 2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / area size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
   where costP1 = costPerInch p1
         costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++
                            " per square inch"
   where costSqInch = costPerInch (size,cost)