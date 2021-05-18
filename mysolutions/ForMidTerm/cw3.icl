module cw3
import StdEnv

/*
Keep the odd elements in the first list and the even elements in  the second list,
the odd in the third list and the even in  the second list, and so on..
*/
keepodd :: [Int] -> [Int]
keepodd x = [a \\ a <- x | isOdd a]

keepeven :: [Int] -> [Int]
keepeven x = [a \\ a <- x | isEven a]

OddthEventh :: [[Int]] Int -> [[Int]]
OddthEventh x n
| n == length x = []
| n rem 2 == 1 = [keepeven (x!!n) : OddthEventh x (n+1)]
= [keepodd (x!!n) : OddthEventh x (n+1)]

f3::[[Int]] ->[[Int]]
f3 x = OddthEventh x 0
// Start = f2 [[1,2,3],[2,3,4,5],[],[4,4,5,5,6],[1]]  //[[1,3],[2,4],[],[4,4,6],[1]]
// Start = f3 [[1,0,1],[2,1,0,3,4,5],[1],[4,4,0,0,5,7,5,6],[1,2]] //[[1,1],[2,0,4],[1],[4,4,0,0,6],[1]]

/*
use map to insert n in the middle of every sublist of a list.
if there is one element in the middle of sublist, insert n before it 
e.g. if n = 4 , [1,2,3] -> [1,4,2,3] (insert 4 before 2)
*/

f44::Int [[Int]] ->[[Int]]
f44 n llist = map (\list = insertAt (length list / 2) n list) llist

// Start = f44 2 [[],[1],[1,3],[3,4,5]] //[[2],[2,1],[1,2,3],[3,2,4,5]]
// Start = f44 8 [[0,1],[1],[1,6,5,3],[3,4,5,7,0],[]]//[[0,8,1],[8,1],[1,6,8,5,3],[3,4,8,5,7,0],[8]]

/*compute the squares of the elements of a list using map with lambda expression .
  and use foldr to compute the product of the list.
*/

f4::[Int]->Int
f4 list = foldr (*) 1 (map (\x = x*x) list)

//Start = f4 [1,2,3] //14
// Start = f4 [1,1,3,7]  //441


