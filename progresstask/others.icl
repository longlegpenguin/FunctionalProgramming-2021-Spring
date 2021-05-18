module others
import StdEnv


// Given a list of integers, write a function that goes 
// through the elements, and for every element i,  generates
//  the i-th Fibonnaci number,  check if it's even or odd, 
//  only keeps the odd ones.
// Example [0,1,2] -> [1,1] because the 0th fib is 1, the 
// 1st fib is 1 and the 2nd fib is 2,  but 2 is an even 
// number, so it's not in the final list.
fib :: Int -> Int
fib n = fibAux n 1 1

fibAux 0 a b         = a
fibAux i a b | i > 0 = fibAux (i-1) b (a+b)

// fiblist :: [Int] -> [Int]
// fiblist [] = []
// fiblist [x:xs] = [fib x : fiblist xs]

// extractodd :: [Int] -> [Int]
// extractodd [] = []
// extractodd [x:xs]
// | x rem 2 == 0 = extractodd xs
// = [x: extractodd xs]

generateFibOdd :: [Int] -> [Int]
// generateFibOdd x = extractodd (fiblist x)
generateFibOdd list = [fib x \\ x<-list | isOdd(fib x)]

// Start = generateFibOdd [0,1,2] // [1,1]

// Start = generateFibOdd [3,7,11] //[3,21]

// Start = generateFibOdd [4..10] //[5,13,21,55,89]


/* 
 * You are given a tuple of lower and upper bounds and a list of tuples,
 * where the first elemnet is course code anf the second one is course 
 * score. Filter the list and return the list of course codes, which have 
 * scores greater or equal to the lower bound nd less or equal to the 
 * upper bound.
 */
rangeFilter :: (Int, Int) [(String, Int)] -> [String]
rangeFilter (lbd, ubd) tlist = [course \\ (course, grade) <- tlist | lbd <= grade && grade <= ubd]
// rangeFilter (lbd, ubd) tlist = map fst (filter (\ (course,grade) = (lbd <= grade && grade <= ubd)) tlist)

// Start = rangeFilter (10,15) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A","E"]
// Start = rangeFilter (3,13) [("A",12),("B",3),("C",5),("E",14),("F",16)] //["A","B","C"]
// Start = rangeFilter (5,7) [] //[]

/* 
 * Intersection 
 * 
 */
Intersection :: [Int] [Int] -> [Int]
Intersection list1 list2 = [inter \\ inter<-list1 & check<-list2| inter == check]

// Start = Intersection [1,3,2,3,4,6] [1,2,2,2,3,6]

/*
Given a list of integers, for each element do the following:
if the element is even, check if this integer appears even amount of times in the list
if the element is odd, check if this integer appears odd amount of times in the list
*/
cntAppear :: Int [Int] -> Int
cntAppear x list = length [z \\ z<-list | x == z]

tOrF :: Int [Int] -> Bool
tOrF a list 
| a rem 2 == 0 = (cntAppear a list rem 2 == 0)
= (cntAppear a list rem 2 <> 0)

check :: [Int] -> [Bool]
check listi = [tOrF x listi \\ x<-listi]

// Start = check [] // []
// Start = check [1,2,2,1] // [False,True,True,False]
// Start = check [1,1,1,2,2,2,3,5,3,3,5] // [True,True,True,False,False,False,True,False,True,True,False]

/* 
Create record "City". It should contain 3 fields: name(string), aera(Int) and
population(Int). Write a function which takes list of cities and 
returns number of cities that have area less than 100 or population less tha 300000
*/

:: City = {name :: String, aera :: Int, population :: Int}

budapest={name="Budapest",aera=525,population=1756000}
kutaisi={name="kutaisi",aera=67,population=147000}
debrecen={name="debrecen",aera=461,population=202000}
berlin={name="berlin",aera=891,population=3645000}
pisa={name="pisa",aera=185,population=90000}

smallCityCount :: [City] -> Int
smallCityCount citylist = length [x \\ x<-citylist | x.aera < 100 || x.population < 300000]


// Start = smallCityCount []
// Start = smallCityCount [budapest,kutaisi,debrecen,berlin,pisa] //3
// Start = smallCityCount [budapest,berlin] //0
// Start = smallCityCount [kutaisi] //1

:: Tree a = Node a (Tree a) (Tree a) | Leaf

bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))

ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

shortTree = Node 14(Node 11 Leaf(Node 13 Leaf Leaf))(Node 17(Node 15 Leaf Leaf)Leaf)

noTree = Leaf

unitTree = Node 1337 Leaf Leaf
// Start = unitTree

/*
Define a type tree.

Write a function that takes a tree as a parameter

and returns a list of the numbers of the nodes whose children are both Leaf.

An empty tree will return [] and a single element tree will return a list of one element*/

leaves :: (Tree Int) -> [Int]
leaves Leaf = []
leaves (Node x Leaf Leaf) = [x] 
leaves (Node x l r) = leaves l ++ leaves r

// Start = leaves bestTree //[3,12,16,18,20]

// Start = leaves ourTree //[1,8,11,19,24,28]

// Start = leaves unitTree //[1337]

// Start = leaves noTree //[]