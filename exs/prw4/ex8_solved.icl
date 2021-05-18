module ex8_solved
import StdEnv

// 1. write a recursive function that computes n at power k (n, k positive numbers)
power :: Int Int -> Int
power n 0 = 1
power n k = n* power n (k-1)

//Start = power 2 5 // 32


// 2. add 3 to every element of a list
f1 :: [Int] -> [Int]
f1 x = map ((+) 3) x

//Start = f1 [1,5,3,1,6]  // [4,8,6,4,9]  


// 3. compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
// hints: first filter it then use map 
f2 :: [Int] -> [Int]
f2 x = map (\ x= 2*x) (filter ((<) 0) x)

//Start = f2 [1, 2, -2, 3, -4] // [2, 4, 6]


// 4. write a function that keeps the integers of a list up to the first 0 encounterred 
// and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
// hints: use takeWhile then map
f3 :: [Int] -> [Int]
f3 x = map div2 (takeWhile ((<>) 0) x)

div2 x = x/2

//Start = f3 [1, 2, -2, 3, 0, -4] // [0, 1, -1, 1]


// 5. write a function for the square of every element of a list and sublists
// [[1,2],[3,4,5,6],[7,8]]  -> [[1,4],[9,16,25,36],[49,64]]  
// hint: map in map
f4 :: [[Int]] -> [[Int]]
f4 x = map (map sq) x

sq x = x*x

//Start = f4 [[1,2],[3,4,5,6],[7,8]] // [[1,4],[9,16,25,36],[49,64]]


// 6. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
// [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
replicate :: Int Int -> [Int]
replicate 0 x = []
replicate n x = [x : replicate (n-1) x]

//Start = replicate 5 10


f5 :: Int [Int] -> [[Int]]
f5 n list = map (replicate n) list

//Start = f5 3 [3..6]


// 7. insert 0 at the beginning of each sublist
// [[1,2], [3,4,5], [6,7]] -> [[0,1,2], [0,3,4,5], [0,6,7]]
f6 :: [[Int]] -> [[Int]]
f6 lists = map ((++) [0]) lists

//Start =  f6 [[1,2], [3,4,5], [6,7]]


// 8. filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]
f7 :: Int [Int] -> [Int]
//f7 n list = filter ((>) n) list  
f7 n list = filter (\y = n > y) list

//Start = f7 3 [1,5,3,2,1,6,4,3,2,1] 


// 9. using notempty eliminate the empty lists: 
// [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

notempty :: [Int] -> Bool
notempty x = not (x == [])

f8 :: [[Int]] -> [[Int]]
//f8 lists = filter notempty lists
f8 lists = filter (\y = y <> []) lists

//Start = f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]


// 10. compute the sum of the sublist using foldr [[1,2,3], [3,4,5], [2,2]] -> [6, 12, 4]
f9 :: [[Int]] -> [Int]
f9 lists = map (foldr (+) 0) lists
//f9 x = map sum x

Start = f9 [[1,2,3], [3,4,5], [2,2]]


// 11. (bonus point) rewrite map using foldr
//mymap :: (a -> b) [a] -> [b]

// Start = mymap inc [1..10]

// Start = power 100 0 // []
/*
 * // Start = power 10 2 // [1,2,4,8,16,32,64,128,256,512] // 
 * If it is power from 0 to ten then there should be 11 terms instead this 10 terms?
 *  i.e [1,2,4,8,16,32,64,128,256,512,1024]
 */
// Start = power 15 3 // [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969, 14348907]
// Start = power 0 0 // []
isEvenForFloat :: Real -> Bool
isEvenForFloat x 
| (toInt x) rem 2 == 1 = False
| toInt (x * 10.0) rem 2 == 1 = False
= True
