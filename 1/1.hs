-- Multiples of 3 and 5
-- Problem 1
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.

sumOfMultiples :: Int -> Int -> Int -> Int

sumOfMultiples m n up = 
    let a = (up-1) `div` m
        b = (up-1) `div` n
        sub = (up-1) `div` (m*n)
        c = (((1+a)*a) `div` 2) * m
        d = (((1+b)*b) `div` 2) * n
        sub' = (((1+sub)*sub) `div` 2) * (m*n)
    in
        c + d - sub'

