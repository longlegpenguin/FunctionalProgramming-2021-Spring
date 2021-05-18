module HW_3
import StdEnv

/*
1. Compute the average of a list of float point numbers with foldr
*/
avg :: [Real] -> Real
avg [] = 0.0
avg x = (foldr (+) 0.0 x) / toReal n 
where n = length x 

// Start = avg [16.2, 17.8, 11.5]//15.1666666666667
// Start = avg [13.0, 40.9] // 26.95
// Start = avg [] // 0

/* 2.
Write a function that takes two integers a and b.
Use iterate to take first a number of elements from the list
which contains the result of b^n (n goes from 0 to a).
Return an empty list for illegal integers a or b.
*/
power :: Int Int -> [Int]
power a b 
| a < 1 || b < 1 = []
= take a (iterate ((*) b) 1 )

// Start = power 100 0 // []
// Start = power 10 2 // [1,2,4,8,16,32,64,128,256,512]
// Start = power 15 3 // [1, 3, 9, 27, 81, 243, 729, 2187, 6561, 19683, 59049, 177147, 531441, 1594323, 4782969]
// Start = power 1 1 //[1]

/* 3.
write a function that takes a list of lists(suppose that there are no empty sublists) and an integer,
do the following operations:
1) select the smallest value from each sublist
2) pick elements from the above resulting list which is smaller than the given integer 
   and is an even number at the same time.
Assume that Real number is even if 'toInt' gives an even number.
*/
selectSmallOneList :: [Real] -> Real
selectSmallOneList [ a : []] = a
selectSmallOneList [x, y : xs] 
| x < y = selectSmallOneList [x: xs]
= selectSmallOneList [y: xs]

select :: [[Real]] Int -> [Real]
select y i = filter (\ x = isEven (toInt x) && x < (toReal 3)) (map selectSmallOneList y)

// Start=select [[3.2,4.9],[2.4,2.4,5.0],[2.2,3.3,4.4],[2.0],[1.3,3.8]] 3 // [2.4,2.2,2]
// Start=select [[5.0,2.5,7.7],[5.0,3.8,2.4],[5.6,7.9,10.76]] 4 // [2.5,2.4]
// Start=select [[3.2,4.9],[3.4,12.4,5.0],[21.2,3.3,4.4],[2.0],[1.3,3.8,7.9]] 3 // [2]
// Start=select [] 1//[]