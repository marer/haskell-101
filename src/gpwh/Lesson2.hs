simple x = x

f x = x ^2

calcChange owed given
    | x > 0 = x
    | otherwise = 0
    where x = given - owed

foo n
    | even n    = n - 2
    | otherwise = 3 * n + 1

foo2 n = if even n then n - 2 else 3 * n + 1

