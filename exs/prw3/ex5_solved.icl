module ex5_solved
import StdEnv


// Operations with lists: write functions for the followings
// without map or foldr.

// 1. Keep every non-empty sublist.
// e.g. [[1, 2, 3], [], [3, 4],[],[],[5, 7, 8, 9],[]] -> [[1, 3, 5],[3,4],[5,7,8,9]]
f1 :: [[Int]] -> [[Int]]
f1 [] = []
f1 [x:xs]
| x == [] = f1 xs
= [x : f1 xs]

//Start = f1 [[1, 2, 3], [], [3, 4],[],[],[5, 7, 8, 9],[]]


// 2. Keep the last two elements of the sublists in 2 versions (ignore if can not be done)
// e.g. [[1, 2, 3], [3, 4], [], [5, 7, 8, 9]] -> [[2, 3], [3,4], [8, 9]] 
last2 :: [[Int]] -> [[Int]]
last2 [] = []
last2 [x:xs] = [last2aux x : last2 xs]

last2aux :: [Int] -> [Int]
last2aux x
| length x < 2 = x
= drop ((length x)-2) x

//Start = last2aux [1, 2, 3]
//Start = last2 [[1, 2, 3], [3, 4], [], [5, 7, 8, 9], [2], [1..10]]


// 3. Triple the head elements of the elements of a list.
triples :: [[Int]] -> [[Int]]
triples [] = []
triples [x : xs] = [tri x : triples xs]

tri :: [Int] -> [Int]
tri [] = []
tri x = [3* hd x] ++ tl x
 
//Start = triples [[1..5],[1..10],[],[1],[1,2,3],[1..4]] 
// [[3,2,3,4,5],[3,2,3,4,5,6,7,8,9,10],[],[3],[3,2,3],[3,2,3,4]]


// 4. Check if the numbers of a list are even. 
// e.g. [1,2,4,5]  -> [False, True, True, False] 
evens :: [Int] -> [Bool]
evens [] = []
//evens [x:xs] = [isEven x : evens xs]
evens [x:xs] = [x rem 2 == 0 : evens xs]

// evens [1: [2,3,4,5]] = [False : evens [2,3,4,5]]
// evens [2: [3,4,5]] = [False : [True : evens [3,4,5]]]
// evens [3: [4,5]] = [False : [True : [False : evens [4,5]]]]
// evens [4: [5]] = [False : [True : [False : [True : evens [5]]]]]
// evens [5: []] = [False : [True : [False : [True : [False : evens []]]]]]
// evens [] = []
// [False : [True : [False : [True : [False : []]]]]]
// [False, True, False, True, False] 

//Start = evens [1..5]


// 5. Add 100 as last number of a list.
add100 :: [Int] -> [Int]
add100 x = x ++ [100]

//Start = add100 [1..8]


// 6. Check if the numbers of a list are multiple of 10.
ismult10 :: [Int] -> [Bool]
ismult10 [] = []
ismult10 [x:xs] = [(x rem 10==0) : ismult10 xs]

//Start = ismult10 [1..20]


// 7. Collect in a list the last digists of the numbers of a list.
lastdigits :: [Int] -> [Int]
lastdigits [] = []
lastdigits [x:xs] = [ x rem 10 : lastdigits xs]

//Start = lastdigits [1..35]


// 8. Compute the cube of the numbers of a list.
cubes :: [Int] -> [Int]
cubes [] = []
cubes [x:xs] = [ x^3 : cubes xs]

//Start = cubes [1..10]
//Start = cubes []


// 9.  Compute the average of the numbers from 1..N (N positive).
addn :: Int -> Int
addn 0 = 0
addn n = sum [1..n] / n

//Start = addn 5
//Start = addn 0
//Start = addn -2
//Start = addn 10


addnR :: Int -> Real
addnR 0 = 0.0
addnR n = toReal (sum [1..n]) / toReal n

//Start = addnR 10


// 10. Compute the length of every sublist of a list
lens :: [[Int]] -> [Int]
lens [] = []
lens [x:xs] = [length x : lens xs]

Start = lens [[1,2,3],[5,6],[],[7,8,9,10]]  // [3,2,0,4]
