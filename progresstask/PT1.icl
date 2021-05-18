module PT1

import StdEnv

//Write a recursive(!) function for the sum of cubes of the first 'n' positive values.

//1*1*1+2*2*2+3*3*3+ ... +n*n*n 

//Assume that 'n' is non-negative.

sumofcube :: Int -> Int
sumofcube 0 = 0
sumofcube n = n*n*n + sumofcube (n-1)

//Start = sumofcube 3  //36
//Start = sumofcube 4  //100
//Start = sumofcube 0  //0
Start = sumofcube 1  //1