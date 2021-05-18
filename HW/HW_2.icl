module HW_2
import StdEnv

//1. Give a list of numbers, multiplying all even numbers by 2 and all odd numbers by 3
multiply :: [Int] -> [Int]
multiply [] = []
multiply [x : xs]
| x rem 2 == 0 = [x*2 : multiply xs]
= [x*3 : multiply xs]

// Start = multiply [14, 22, 45, 56] // [28, 44, 135, 112]
// Start = multiply [13, 27, 44] // [39, 81, 88]
// Start = multiply [] // []

// 2. Given a list of integers, find the prime numbers and compute the sum of them.
// Return 0 for empty lists or if there are no primes.

// 2 is prime.
isPrime :: Int Int -> Bool
isPrime _ 1 = True
isPrime a b
| a < 2 = False  // 0,1 are not prime. 
| a rem b == 0 = False
= isPrime a (b-1)

sum_of_prime :: [Int] -> Int
sum_of_prime [] = 0
sum_of_prime [x : xs]
| isPrime x (x-1) = x + sum_of_prime xs
= sum_of_prime xs

// Start = sum_of_prime [14, 22, 45, 56] // 0
// Start = sum_of_prime [13, 27] // [13]
// Start = sum_of_prime [13, 3, 76, 17] // 33
// Start = sum_of_prime [] // 0
// Start = sum_of_prime [1] // 0
// Start = sum_of_prime [2, 3] // 5
/*
3. Given two lists of integers of the same length, check if the elements in two lists with the same index
are of the same property (both even or both odd).
Return True for empty lists
*/
same :: [Int] [Int] -> Bool
same [] [] = True
same [x : xs] [y : ys]
| (isOdd x && isOdd y) || (isEven x && isEven y) = same xs ys
= False

// Start = same [1,2,3] [2,4,6] // False
// Start = same [1,2,3,4] [3,8,5,12] // True
// Start = same [] [] // True
// Start = same [0,1] [0,20] // False