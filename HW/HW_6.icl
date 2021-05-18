module HW_6
import StdEnv

/*1. Given two sorted lists of unique integers, return lists which contains their union.
Union of the two lists should contain all integers which appear in at least one of them.
Each element should be added only once. Elements in union should be sorted in ascending order.
Ex.: [1,2,3,4,5] [4,5,6,7] -> [1,2,3,4,5,6,7]
*/

listUnion :: [Int] [Int]-> [Int]
listUnion list1 list2 = sort (removeDup (list1 ++ list2))

// Start = listUnion [1,3,4,7,8,12,13] [1,4,6,10,11,12,15] // [1,3,4,6,7,8,10,11,12,13,15]
// Start = listUnion [1..5] [1,3..10] // [1,2,3,4,5,7,9]
// Start = listUnion [1,2] [] // [1,2]
// Start = listUnion [] [] // []

/*2. Given the list of points and a distance. Each point is represented with tuple, 
containing X and Y coordinates in 2D plane.
 Return how many pair of pointrs are there so that, 
 distance between them is equal to the given value.
*/
equalDis :: (Int,Int) (Int,Int) Int -> Bool
equalDis (x,y) (a,b) dis = ((x-a)*(x-a) + (y-b)*(y-b) == dis*dis)

pointDistance :: [(Int,Int)] Int -> Int
pointDistance tlist dist = length goodtuples
    where 
        goodtuples = [x \\ x<-sortedl,y<-sortedl | x > y && equalDis x y dist]
        sortedl = sort tlist
// Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 5 // 2
// Start = pointDistance [(1,1), (4,5), (8,8), (10,3)] 2 // 0
// Start = pointDistance [(3,4), (3,8), (7,8)] 4 // 2
// Start = pointDistance [] 3 // 0
// Start = pointDistance [(1,1)] 2 // 0


/*3. Given the list of integers, modify it in a following way:
I. Remove all numbers which are multiple of 3
II. Sort remaining list in descending order
III. Swap 1st and 2nd elements, 3rd and 4th, 5th and 6th and so on.
*/

listSwap :: [Int] -> [Int]
listSwap x 
| length x < 2 = x
listSwap [x,y:xs] = [y,x: listSwap xs]

shuffleSort :: [Int] -> [Int]
shuffleSort list = listSwap sortedl
    where
        sortedl = reverse (sort noThreel)
        noThreel = [x \\ x<-list | x rem 3 <> 0]

// Start = shuffleSort [4,3,2] // [2,4]
// Start = shuffleSort [4,1,3,2,5,6,7] // [5,7,2,4,1]
// Start = shuffleSort [3,6,3,9,12] // []
// Start = shuffleSort [2,4,5,7,14,17] // [14,17,5,7,2,4]
// Start = shuffleSort [] // []