module ex10
import StdEnv

// 1. Generate 100 even numbers using list comprehensions
l1 :: [Int]
l1 = take 100 [x \\ x <- [1..] | isEven x]

// Start = l1


// 2. Generate teh following list [4, 16, 36, 64, 100, 144, 196, 256, 324, 400]
l2 :: [Int]
l2 = [x*x \\ x <- [2,4..20]]

// Start = l2


// 3. List powers of 2 from 1 to 10.
//hint: use x^y (x at power y)
l3 :: [Int]
l3 =  [2^y \\ y <- [1..10]]

// Start = l3


// 4. List the divisors of 90.
l4 :: [Int]
l4 = [x \\ x <- [1..90] | 90 rem x == 0]

// Start = l4


// 5. List �dominoes�: [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),...,(9,9)]
// Domino (1,0) is not in the list because it is already in it as (0,1).
l5 :: [(Int, Int)]
l5 = [(x, y) \\ y <- [0..9], x <- [0..y]]

// Start = l5


// 6. Construct the list [(1,'a'),(2,'b'),�(�,'z')]
l6 :: [(Int, Char)]
l6 = [(x, y) \\ x <-[1..] & y <-['a'..'z']]

// Start = l6


// 7. Generate a list of length 10 whose elements are False, True, False, True, � (alternating)
l7 :: [Bool]
// l7 = [isEven x \\ x <- [1..10]]
l7 = flatten [x \\ x <-[[False,True]], y <-[1..5]]
// Start = l7


// 8. Is 123457 a prime number?
l8 :: Bool
l8 = length ([x \\ x <-[1..123457] | 123457 rem x == 0]) == 2

// Start = l8


// 9. Generate the list [(0,10),(1,9),�,(10,0)].
l9 :: [(Int, Int)]
// l9 = [(x,y) \\ x <-[0..10] & y <-[10,9..]] 
l9 = [(x,y)\\ x <- [0..10], y<-[0..10] | x+y == 10]
// Start = l9


// 10. Generate a list that contains all (hour, minute) pairs in a day.
l10 :: [(Int, Int)]
l10 = [(hour,min) \\ hour <-[1..23], min <- [1..59]]

// Start = l10


// 11. (bonus point) Generate a list that contains all (month, day) pairs in a 365-day 
l11 :: [(Int, Int)]
l11 = [(x, y) \\ x <- [1,3,5,7,8,10,12] , y <- [1..31]] ++ [(x, y) \\ x <- [4,6,9,11] , y <- [1..30]] ++ [(x, y) \\ x <- [2] , y <- [1..28]]

// Start = l11