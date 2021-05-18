module cw2

import StdEnv

/* 1.
Delete the middle eltement of each list (if length if odd) in the list.
e.g. [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] -> [[1,3],[],[4,5,6,7],[],[0,1,3,5]]
*/
deletemid :: [Int] -> [Int]
deletemid xs
| isEven(length xs) = xs
= take n xs ++ drop (n+1) xs
where n = length xs / 2

f1::[[Int]]->[[Int]]
f1 [] = []
f1 [x:xs] = [deletemid x : f1 xs]

// Start = f1 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]]  //[[1,3],[],[4,5,6,7],[],[0,1,3,5]]
// Start = f1 [[1,2,6,8,3],[9,3],[0,5,0,6,7,0,0],[],[0,1,0,6,3,5]] //[[1,2,8,3],[9,3],[0,5,0,7,0,0],[],[0,1,0,6,3,5]]
// Start = f1 [[0],[3,6],[4,5,6],[],[0,1,6,9,7,3,5]]  //[[],[3,6],[4,6],[],[0,1,6,7,3,5]]


/* 2.
Delete the third eltement of each list in the list.
e.g. [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] -> [[1,2],[3],[4,5,7],[],[0,1,3,5]]
*/

// f2::[[Int]]->[[Int]]
// f2 x = [x!!]


//Start = f2 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]]  //[[1,2],[3],[4,5,7],[],[0,1,3,5]]
//Start = f2 [[1,2,6,8,3],[9,3],[0,5,0,6,7],[],[0,1,6,3,5,8]]  //[[1,2,8,3],[9,3],[0,5,6,7],[],[0,1,3,5,8]]
//Start = f2 [[0],[3],[4,5,6],[],[0,1,6,9,7,3,5]]  //[[0],[3],[4,5],[],[0,1,9,7,3,5]]


/* 3.
Move the elements that are odd and less than 7 to the front of the list.
*/
moveOdd :: [Int] -> [Int]
moveOdd [] = []
moveOdd [x:xs] 
| isEven x || x >= 7 = moveOdd xs ++ [x]
= [x: moveOdd xs]

modd :: [Int] -> [Int]
modd x = [a \\ a <- x | isOdd a && a < 7] ++ [a \\ a <- x | isEven a || a >= 7]
//f3::[Int]->[Int]
// Start = modd [15,13..1] //[5,3,1,15,13,11,9,7]
Start = modd [12321, 1, 4, 34, 7, 9, 525] //[12321,1,4,34,7,9,525]
// Start = modd [1..10]

//Start = f4 [1..10] //[1,3,5,2,4,6,7,8,9,10]
//Start = f4 [15,13..1] //[5,3,1,15,13,11,9,7]
//Start = f4 [12321 ,1, 4, 34, 7, 9, 525] //[1,12321,4,34,7,9,525]
