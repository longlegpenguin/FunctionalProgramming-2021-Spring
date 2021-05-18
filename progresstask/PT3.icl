module PT3

import StdEnv

// Given a list of integers, write a function that goes through the element, 
// and for every element i,  generates the i-th Fibonnaci number, and only 
// check it's even or odd, only keeps the even ones.
// Example [0,1,2] -> [2] because the 0th fib is 1, the 1st fib is 1 and the 
// 2nd dib is 2,  but only 2 is even, so it's in the final list.

fib :: Int -> Int
fib n = fibAux n 1 1

fibAux 0 a b = a
fibAux i a b | i > 0 = fibAux (i-1) b (a+b)

fiblist :: [Int] -> [Int]
fiblist [] = []
fiblist [x:xs] = [fib x : fiblist xs]

keepEven :: [Int] -> [Int]
keepEven [] = []
keepEven [x:xs]
| x rem 2 == 1 = keepEven xs
= [x: keepEven xs]

generateFibEven :: [Int] -> [Int]
generateFibEven x = keepEven (fiblist x)

// Start = generateFibEven [0,1,2] // [2]
// Start = generateFibEven [3,7,11] //[144]
// Start = generateFibEven [4..10] //[8,34]
// Start = generateFibEven [0..11] //[2,8,34,144]