-- Raise x to the power y, using recursion
-- For example, power 5 2 = 25
power :: Int -> Int -> Int
power x y
   | y == 1    = x
   | otherwise = x * power x (y-1)

-- create a list of length n of the fibbonaci sequence in reverse order
-- examples: fib 0 = [0]
-- 	     fib 1 = [1, 0]
--	     fib 10 = [55,34,21,13,8,5,3,2,1,1,0]
-- try to use a where clause
-- performance in ghci using :set +s (0.01 secs, 227,696 bytes)
fib :: (Num a, Eq a) => a -> [a]
fib 0 = [0]
fib x = fibCalc x : fib (x-1)
   where fibCalc 0 = 0
         fibCalc 1 = 1
         fibCalc x = fibCalc (x-2) + fibCalc (x-1)

-- I didn't write this myself, but I knew it had to be possible, its what
-- I wanted to do but couldn't get my head around. (I googled for other answers to this exercise)
-- this function just calculates the fibbonaci sequence the straight forward way
-- by adding the two previous fibbonaci numbers together
-- its also faster and uses less memory that my solution above (0.00 secs, 77,672 bytes)
fib' :: (Num a, Eq a) => a -> [a]
fib' 0 = [0]
fib' 1 = [1,0]
fib' x = (a + b) : fibList
   where fibList@(a:b:_) = fib' (x-1)

-- This is not recursive, but have a go anyway.
-- Create a function which takes two parameters, a number and a step
-- The result is the sign of the original number reversed, and the step added to the absolute value
-- Confused? Some examples: stepReverseSign 6 2 = -8
--			    stepReverseSign -3 1 = 4
--			    stepReverseSign 1 2 = -3
stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign a b
   | a < 0  = -a + b
   | a >= 0 = -a - b

{- Lets calculate pi.
 - The Leibniz formula for pi (http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80)
 - Can be defined as pi = (4/1) - (4/3) + (4/5) - (4/7) ....
 - We can create a function, where given a certain tolerance, we can recursively calculate
 - Pi to within that tolerance.
 - Lets create two functions, piCalc, and piCalc', the latter we will recursively call
 - until our pi calculation is within the tolerance

 - The piCalc function is defined as:
 - piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)

 - Given a tolerance, say, 0.001, it will return a tuple.
 - fst is pi to an accuracy of the tolerance, 0.001 in this case
 - snd is the number of recursive steps taken to calculate it, after all this chapter is about recursion!
 - Example: piCalc 0.001 = (3.1420924036835256,2000)

 - The piCalc' function is defined as
 - piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
 - Lots of parameters!
 - The first parameter is the current denominator from the Leibniz formula
 - The next is our calculation of pi from our previous attempt
 - The next is the tolerance
 - The final parameter is the number of times this function has been called (ie, we add one every time we recurse
 - Example piCalc' 1 0.0 0.001 0 = (3.1420924036835256,2000)
 -
 - Feel free to change the parameter order, what parameters you need etc in order to get this to work for you,
 - But, of course the output of piCalc should remain as (pi, count)
 -
 - You may find the stepReverseSign function handy
 -}

piCalc :: (Fractional a, Integral b, Ord a) => a -> (a, b)
piCalc a = piCalc' 1 0.0 a 0

piCalc' :: (Ord a, Fractional a, Integral b) => a -> a -> a -> b -> (a, b)
piCalc' w x y z
   | seqStep < y && seqStep > (-y) = (x + seqStep, z)
   | otherwise = piCalc' (stepReverseSign w 2) (x + seqStep) y (z+1)
   where seqStep = 4/w

