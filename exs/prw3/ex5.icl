module ex5

import StdEnv


// Operations with lists: write functions for the followings
// without map or foldr.

// 1. Keep every non-empty sublist.
// e.g. [[1, 2, 3], [], [3, 4],[],[],[5, 7, 8, 9],[]] -> [[1, 3, 5],[3,4],[5,7,8,9]]
f1 :: [[Int]] -> [[Int]]
f1 [] = []
f1 [x:xs]
| x <> [] = [x: f1 xs]
= f1 xs


// Start = f1 [[1, 2, 3], [3, 4], [5, 7, 8, 9]]


// 2. Keep the last two elements of the sublists in 2 versions (ignore if can not be done)
// e.g. [[1, 2, 3], [3, 4], [], [5, 7, 8, 9]] -> [[2, 3], [3,4], [8, 9]]
last2 :: [[Int]] -> [[Int]]
last2 [] = []
last2 [x : xs]
| length x < 2 = last2 xs
= [drop n x : last2 xs]
where n = length x - 2

// Start = last2 [[1, 2, 3], [3, 4], [], [5, 7, 8, 9]]


// 3. Triple the head elements of the elements of a list.
triples :: [[Int]] -> [[Int]]
triples [] = []
triples [x : xs]
| x == [] = [[] : triples xs]
triples [[y:ys] : xs]
= [[y*3: ys] : triples xs]

// Start = triples [[1..5],[1..10],[],[1],[1,2,3],[1..4]]


// 4. Check if the numbers of a list are even.
// e.g. [1,2,4,5]  -> [False, True, True, False]
evens :: [Int] -> [Bool]
evens [] = []
evens [x:xs] = [x rem 2 == 0 : evens xs]

// Start = evens [1..5]


// 5. Add 100 as last number of a list.
add100 :: [Int] -> [Int]
add100 a = a ++ [100]

// Start = add100 [1..8]


// 6. Check if the numbers of a list are multiple of 10.
ismult10 :: [Int] -> [Bool]
ismult10 [] = []
ismult10 [x:xs] = [x rem 10 == 0 : ismult10 xs]

// Start = ismult10 [1..20] //[False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,True]


// 7. Collect in a list the last digists of the numbers of a list.
lastdigits :: [Int] -> [Int]
lastdigits [] = []
lastdigits [x:xs] = [x rem 10 : lastdigits xs]

// Start = lastdigits [1..35]


// 8. Compute the cube of the numbers of a list.
cubes :: [Int] -> [Int]
cubes [] = []
cubes [x:xs] = [x^3 : cubes xs]

// Start = cubes [1..10]
//Start = cubes []

// 9.  Compute the average of the numbers from 1..N (N positive).
// addn :: Int -> Int
// addn 0 = 0
// addn a 
// | a < 0 = abort "must pos!"
// = (sum a) / a

// sum :: Int -> Int
// sum 0 = 0
// sum num = num + sum (num-1)

addn2 :: Int -> Int
addn2 0 = 0
addn2 a 
| a < 0 = abort "must pos!"
= (1+a)*a/2 / a

// Start = addn 5
// Start = addn 0
// Start = addn -2
// Start = addn2 10


// 10. Compute the length of every sublist of a list
lens :: [[Int]] -> [Int]
lens [] = []
lens [x:xs] = [length x : lens xs]

// Start = lens [[1,2,3],[5,6],[],[7,8,9,10]]

// Start = sum [1,2, 4,10]

seleteven :: [Int] -> [Int]
seleteven [] = []
seleteven [x:xs] 
| x rem 2 == 1  = seleteven xs
= [x : seleteven xs]

Start = selectodd [1..20]
























selectodd :: [Int] -> [Int]
selectodd [] = []
selectodd [x:xs] 
| x rem 2 == 0 = selectodd xs
= [x  : selectodd xs]

