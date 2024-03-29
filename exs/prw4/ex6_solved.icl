module ex6_solved

import StdEnv

// Start = filter isEven [1..10] // [2,4,6,8,10]

odd x = not (isEven x)
// Start = odd 23 // True

// Start = filter (not o isEven) [1..100] // [1,3,5,..,99]

// Start = takeWhile isEven [2,4,6,7,8,9] // [2, 4, 6]

// Start = dropWhile isEven [2,4,6,7,8,9] // [7, 8, 9]

plus x y = x + y

successor :: (Int -> Int)
successor = plus 1

//Start = successor 4 // 5

succ = (+) 1

//Start =  succ 5 // 6

// the function adding 5 to something
//Start = map (plus 5) [1,2,3] // [6,7,8]

//Start = until ((<)10) ((+)2) 0 // 12

// iteration of a function

//Start = take 100000 (iterate inc 1) // infinite list [1..]

// Start = map inc [1, 2, 3]        // [2, 3, 4]

// Start = map double [1, 2, 3]     // [2, 4, 6]

// lambda expressions
// Start = map (\x = x*x+2*x+1) [1..10] // [4,9,16,25,36,49,64,81,100,121]

//Start = foldr (+) 10 [1, 2, 3]   // 16

product [] = 1
product [x:xs] = x * product xs

product1 = foldr (*) 1  // 1*2*3*1 - 6

product11 = foldr (*) 10  // 1*2*3*10 - 60

//Start = product1 [1, 2, 3] // 6

and1 [] = True
and1 [x:xs] = x && and1 xs

//Start = and1 [True, True, False]

and2 = foldr (&&) True

//Start = and2 [True, True, False] // False

sum1 = foldr (+) 0  // 1+2+3+0

//Start = sum1 [1, 2, 3] // 6

///////////////
//Exercises
// 1. Map a list of functions to a value. 
//E.g. mapfun [f,g,h] x = [f x, g x, h x]
mapfun :: [Int -> Int] Int -> [Int]
mapfun [] x = []
mapfun [f:fs] x = [f x : mapfun fs x]

//Start = mapfun [inc, inc, inc] 3 // [4, 4, 4]


// 2. compute n! factorial using foldr
f :: Int -> Int
f n = foldr (*) 1 [1..n]  // 1*2*3*4*5*1 = 120

//Start = f 5 // 120


// 3. rewrite flatten using foldr 
// (for the following list [[1,2], [3, 4, 5], [6, 7]] => [1,2,3,4,5,6,7])
flat :: [[Int]] -> [Int]
flat x = foldr (++) [] x  // [1,2] ++ [3, 4, 5] ++ [6, 7] ++ []

//Start = flat [[1,2], [3, 4, 5], [6, 7]] // [1,2,3,4,5,6,7]


// 4. using map and foldr compute how many elements are altogether in the following list 
// [[1,2], [3, 4, 5], [6, 7]] => 7
gg :: [[Int]] -> [Int]
gg x = map length x 

g :: [[Int]] -> Int
g x = foldr (+) 0 (  map length x )

//Start = g [[1,2], [3, 4, 5], [6, 7]] // 7


// 5. using map extract only the first elements of the sublists in 
// [[1,2], [3, 4, 5], [6, 7]] => [1,3,6]
firsts :: [[Int]] -> [Int]
firsts x = map hd x

//Start = firsts [[1,2], [3, 4, 5], [6, 7]] // [1,3,6]


// 6. compute the squares of the elements of a list using map
// [1, 2, 3] -> [1, 4, 9]
sq :: Int -> Int
sq x = x*x

sqrs :: [Int] -> [Int]
sqrs x = map sq x

//Start = sqrs [1, 2, 3] // [1, 4, 9]


// 7. same as 6. with lambda expression 
sqrs_lambda :: [Int] -> [Int]
sqrs_lambda x = map (\ x = x^2) x

Start = sqrs_lambda [1, 2, 3] // [1, 4, 9]


