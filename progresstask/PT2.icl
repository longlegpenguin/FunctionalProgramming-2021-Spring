module PT2
import StdEnv


// Given a list of integers, write a function that returns a list of 2 integers
// , where the first integers is a sum of elements on odd indexes 
// (start indexing from 1) and the second integers is a sum of 
// elements on even indexes.

onlyThirds :: [Int] -> [Int]
onlyThirds [] = []
onlyThirds [a]
| length [a] < 1 = []
onlyThirds a = [sumoflist ([(a!!0)] ++ onlyThirds (drop 2 a))]

sumoflist :: [Int] -> Int
sumoflist [] = 0
sumoflist [x:xs] = x + sumoflist xs
 
Start = onlyThirds [1,2,2,3,4] // [7,5]  
//Start = onlyThirds [3] // [3,0]
//Start = onlyThirds [1,2] // [1,2]