module PT4
import StdEnv

// You are given a tuple of two lists. The first list contains 
// tuples of arguments (x, y),
// and the second list contains tuples of coefficients (a, b, c) 
// for equation a*x2-b*y+c*y2. 
// Your task is to calculate the result of each equation.
// Example: ([(1,2),(1,2)], [(1,2,3), (0,1,-1)]) -> [9, -6] , 
// because the 1st equation is 1*12-2*2+3*22 and 2nd equation 
// is 0*12-1*2+(-1)*22.

// Note: You are guaranteed that lists have the same size.

tuple :: (Int, Int) (Int, Int, Int) -> Int
tuple (x, y) (a, b, c) = a*(x^2) - b*y + c*(y^2)

solver :: ([(Int,Int)], [(Int,Int,Int)]) -> [Int]
solver (a, b) = [tuple x y \\ x <- a & y <- b]

// Start = solver ([], []) // []
// Start = solver ([(1,2)], [(1,2,3)]) // [9]
// Start = solver ([(1,1),(2,0),(5,1)], [(1,1,1),(2,3,5),(10,0,0)]) // [1,8,250]
// Start = solver ([(1,2),(1,2)], [(1,2,3), (0,1,-1)]) // [9,-6]
// Start = solver ([(1,-1),(2,3),(5,1),(-5,0),(0,0)], [(1,3,1),(2,7,1),(10,0,0),(0,0,0),(2,2,2)]) // [5,-4,250,0,0]