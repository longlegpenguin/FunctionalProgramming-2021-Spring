module PT7
import StdEnv


/*
Given a list of tuples, return a list that:
for each tuple (a, b) in the original list:
if the sum of a and b is even, multiply a and b.
if the sum of a and b is odd, compute a^b.
And only preserve the elements that are greater than 10.
e.g. [(1,2), (2,6)] -> [1,12] and 1 is less than 10 it should be left out,
so the function should return [12].
*/
processTuple :: (Int,Int) -> Int
processTuple (a,b)
| (a+b) rem 2 == 0 = a * b 
= a^b 

tup :: [(Int, Int)] -> [Int]
tup tlist = [processTuple t \\ t<-tlist | processTuple t > 10]

// Start = tup [] // []
// Start = tup [(1,2),(2,6)] // [12]
// Start = tup [(1,2),(2,6),(4,4)] // [12,16]
// Start = tup [(1,1),(1,2),(2,2),(3,5),(3,3)] // [15]