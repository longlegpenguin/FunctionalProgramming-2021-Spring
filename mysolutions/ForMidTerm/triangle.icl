module triangle
import StdEnv


// Given an array of integers, print a sum triangle from it such that the first level has all array elements.
// From then, at each level number of elements is one less than the previous level 
// and elements at the level is be the Sum of consecutive two elements in the previous level.
twoTwoAdd :: [Int] -> [Int]
twoTwoAdd x
| length x < 2 = []
twoTwoAdd [x,y:xs]
= [x+y: twoTwoAdd[y:xs]]

triangleAux :: [Int] -> [[Int]]
triangleAux [a] = [[a]]
triangleAux list = [list : triangleAux (twoTwoAdd list)]

triangle :: [Int] -> [[Int]]
triangle list = reverse okaylist
    where 
    okaylist = triangleAux list

Start = triangle [1,2,3,4,5]    //[ [48],
                                //  [20, 28] 
                                //  [8, 12, 16] 
                                //  [3, 5, 7, 9] 
                                //  [1, 2, 3, 4, 5] ]
