module mypt03
import StdEnv
//Write a function which takes a [Int] and checks 
//if the first two integers are equal to the last two
//Important note: if the list has less than 2 elements return False
firstLast :: [Int] -> Bool
firstLast a 
| length a < 2 = False
= modifithelist a == reverse (modifithelist a)

modifithelist :: [Int] -> [Int]
modifithelist a = take 2 a ++ drop (length a - 2) a

firstLast2 :: [Int] -> Bool
firstLast2 aa
| length aa < 2 = False
= take 2 aa == take 2 (reverse aa)

// Start = firstLast2 [1..10]//False
// Start = firstLast2 [1,4,3,4,3,1,4]//False
// Start = firstLast2 [1,2,1]//True
// Start = firstLast [1,1]//True
// Start = firstLast [1]//False

//Write a function which takes a [Int] and returns a
//[Int] containing the middle element of that list.
//Note: lists with odd number of elements will only
//return a list with one middle element, lists with
//even number of elements should return a list with
//two elements.
middle :: [Int] -> [Int]
middle [] = []
middle a
| length a rem 2 == 1 = [a!!n]
= [a!!(n-1), a!!n]
where n = length a / 2

// Start=middle [1..10]//-1 
// Start=middle [1,2,3,4,5]//3
// Start=middle []//-1


//Given a [[Int]], write a function that reverses
//sublists of odd length.
reveroddlen :: [Int] -> [Int]
reveroddlen a
| isOdd(length a) = reverse a
= a 

reverseLists :: [[Int]] -> [[Int]]
reverseLists [] = []
reverseLists [x:xs] = [reveroddlen x : reverseLists xs]

reverseLists2 :: [[Int]] -> [[Int]]
reverseLists2 x = [reverse a\\a<- x | isOdd(length a)]
// Start = reverseLists2 [[1..10],[4..10],[5,4,3,2,1]]

// Start = reverseLists [[1..10],[4..10],[5,4,3,2,1]] //[[1,2,3,4,5,6,7,8,9,10],[10,9,8,7,6,5,4],[1,2,3,4,5]]
// Start = reverseLists [[],[1,2,3],[4,5,6,7],[8,9,0]] // [[],[3,2,1],[4,5,6,7],[0,9,8]]
// Start = reverseLists [] // []
Start = [a \\ a <- [1,2,3,4] | isEven a]


takeEven :: [Int] -> [Int]
takeEven x = [x!!a \\ a <- [0,2..length x - 1]]

// Start = takeeven [1,2,3,4,5,6,7,8,9,10]