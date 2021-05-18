module Cons3

import StdEnv

//isPrime



//#1 Function to compare lists
Divisors :: Int Int -> [Int]
Divisors a b
|a == b  = []
| a rem b == 0 = [b] ++ Divisors a (b+1)                                     
|otherwise = Divisors a (b+1)

isPrime :: Int -> Bool
isPrime n = (Divisors n 1) == [1,n]
/*
            [1,3,10,9]  == [1,9]
            [1,3] == [1,9] -> False
*/

//Start = isPrime 200001020302 // -> [1,99]
//#2 Function to check if prime
isPrime2 :: Int -> Bool
isPrime2 a = isPrimeAux a 2

isPrimeAux :: Int Int -> Bool
isPrimeAux a n 
| a == n = True
| a rem n == 0 = False 
= isPrimeAux a (n+1)

// isPrime 7 2 -> isPrime 7 3 -> isPrime 7 4 .. -> isPrime 7 7

// Start = isPrime2 2000010

//#3 We talk about later


/*
Given a list of integers, write a function that returns a list of 2 integers,
where the first integers is a sum of elements on odd indexes (start indexing from 1)
and the second integers is a sum of elements on even indexes.
*/
onlyThirds :: [Int] -> [Int]
onlyThirds list = [(onlyThirdEvenAux list 0),(onlyThirdOddAux list 1)]

onlyThirdEvenAux :: [Int] Int -> Int
onlyThirdEvenAux list n
| n >= (length list) = 0 
= list!!n + onlyThirdEvenAux list (n+2)

onlyThirdOddAux :: [Int] Int -> Int
onlyThirdOddAux list n
| n >= (length list) = 0 
= list!!n + onlyThirdOddAux list (n+2)

onlyThirds2 :: [Int] -> [Int]
onlyThirds2 list = [sum a,sum b]
where
    a = [list!!a \\ a<-[0,2.. (length list)-1]]
    b = [list!!a \\ a<-[1,3.. (length list)-1]]

//Start = onlyThirds2 [1,2,2,3,4] // [7,5]  
// Start = onlyThirds2 [3] // [3,0]
// Start = onlyThirds2 [1,2] // [1,2]

/*
a = [list!!a \\ a<-[0,2.. (length list)-1]]
b = [list!!a \\ a<-[1,3.. (length list)-1]]
a= [list!!0] ++ [list!!a \\ a<-[2,4.. (length list)-1]]
a= [list!!0] ++[list!!2] ++ [list!!4]
*/

/*
[1,2,2,3,4]
(1,2,4), (2,3)
(0,2,4), (1,3)
*/
//Start = onlyThirds [1,2,2,3,4] // [7,5]  
// Start = onlyThirds [3] // [3,0]
// Start = onlyThirds [1,2] // [1,2]

// 2. Insert x as second element in every sublist of a list.
// if the sublist was empty then x will be the only element in the new sublist. 
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]

insertAtTwo:: [Int] Int -> [Int]
insertAtTwo [] a = [a]
insertAtTwo [x:xs] a = [x,a:xs] 

twolist :: [[Int]] Int -> [[Int]]
twolist [] n = []
twolist [x:xs] n = [(insertAtTwo x n):(twolist xs n)]

twolist2 ::[[Int]] Int -> [[Int]]
twolist2 list n = [(insertAtTwo a n)\\ a<-list]


Start = twolist2 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10
 

/*
[[1,2], [3,4,5], [6,5,9,7], [], [8]]
[insertAtTwo [1,2] 10: twolist [[3,4,5], [6,5,9,7], [], [8]]]
[[1,10,2]:[insertAtTwo [3,4,5] 10: twolist [[3,4,5], [6,5,9,7], [], [8]]]
*/

// Start = twolist [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10

//Given two lists of Int. Write a function which will return a list containing 
//at each position the bigger number from those lists at that position
// lists are of the same length
//Example: [2,3,6] [1,7,0]->[2,7,6]
isBigger :: [Int] [Int] -> [Int]
isBigger [] _ = []
isBigger [x:xs] [y:ys]
| x > y = [x] ++ isBigger xs ys
= [y] ++ isBigger xs ys

//Start= isBigger [2,3,6] [1,7,0] 

///////////// LIST COMPREHENSION 

/*
List comprehensions are all about building lists.
They like to use lists.
*/
/*
When to use List Comprehension:
- You need to process a bunch of elements.
- You need to build a new list.
- You need to do operations on one or more lists.
*/
/*
[Element \\ Element <- List | Guards]
[a \\ a <- [1,2,3,4,5] | isEven a]
[] ++ [a \\ a<- [2,3,4,5] |isEven a]
[] ++ [2] ++ [a \\ a<- [3,4,5] |isEven a]
[] ++ [2] ++ [] ++ [4] ++ []  -> [2,4]
*/
//Start = [a \\ a <-[1,3,4,5,4] |isEven a]
// Returns the squares of the number that we pass into it 
Squares :: [Int] -> [Int]
Squares list = [a^2 \\ a <- list]

// Start = Squares [1..10]
 
// Returns the boolean value of whether the number is even or not
isEvenList :: [Int] -> [Bool]
isEvenList list = [isEven a \\ a <- list]

//Start = isEvenList [1..5]

Divisors2 :: Int -> [Int]
Divisors2 n = [a \\ a <- [1..n] | n rem a == 0]

// Start = Divisors2 1002939495
// Start = [1..1002939495]

isPrime3 :: Int -> Bool
isPrime3 n = [a \\ a <- [2..n-1] | n rem a == 0] == []
            //  [a\\ a <- [2..6] | n rem a ==0]
            //  [a\\ a <- [2..7] | 8 rem 2 ==0] -> [2, 4] <> []

// Start = isPrime3 7
// Start = isPrime3 8






