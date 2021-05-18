module superdigit
import StdEnv

// We define super digit of an integer

// using the following rules:

// If x has only digit, then its super digit is x
// Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.


// For example, super digit of 9875 
// will be calculated as:
// super_digit (9875) = super_digit (9+8+7+5) 
//                    = super_digit (29) 
//                    = super_digit (2+9)
//                    = super_digit (11)
//                    = super_digit (1+1)
//                    = super_digit (2)
//                    = 2.

// You are given two numbers n and k. You have to calculate the super digit of P.
// Where P is created when n is concatenated k times. That is, if n = 123 and k = 3, then P = 123123123

super_digit :: Int Int -> Int
super_digit n k = supering p 
    where 
        p = sum (intToList n) * k

intToList :: Int -> [Int]
intToList a
| a < 10 = [a]
= intToList (a / 10) ++ [a rem 10]

supering :: Int -> Int
supering x
| x/10 == 0 = x
= supering (sum (intToList x))

// Start = super_digit 9875 1 // 2