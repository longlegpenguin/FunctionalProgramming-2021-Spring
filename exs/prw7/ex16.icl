module ex16

import StdEnv

// Sample test, earlier midterm
// For the mark 2 ex1 and ex2 and ex3 and ex4 must be done
// For a mark >2 ex.1,2,3,4 + any from ex. 5(1p), 6(1p), 7(1p), 8(1p)

// 1. Create using an input list of tuples a new list of tuples like:
// [(1,1), (2,6), (3,9)] -> [(1,1,2), (2,6,8), (3,9,12)] 

create :: [(Int, Int)] -> [(Int, Int, Int)]
create tlist = [(x,y,x+y) \\ (x,y) <- tlist]

// Start =  create [(1,1), (2,6), (3,9)]
 

// 2. Compute the average of tuple elements using map
averages :: [(Int, Int)] -> [Int]
averages tlist = map (\(x,y) = (x+y)/2) tlist

// Start = averages [(1,1), (2,6), (3,9)]


// 3. Put the product of the sublist elements in a list, you must use foldr
sublistsp :: [[Int]] -> [Int]
sublistsp llist = foldr (\ x y = [prod x] ++ y) [] llist

// Start = sublistsp [[1, 2, 3], [3, 4], [5, 7, 1]] // [6,12,35]


// 4. Generate the following list of lists
// [[1],[2,1],[3,2,1],[4,3,2,1],[5,4,3,2,1]]
genNlist :: Int -> [[Int]]
genNlist n = [reverse [1..x] \\ x<-[1..n]]

// Start = genNlist 5


// 5. "Reverse" a number: 1234 -> 4321
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x/10) ++ [x rem 10]

revnr :: Int -> Int
revnr n = sum [x*(10^k) \\ x <- intlist & k <- [0..]]
    where intlist = intToList n

// Start = revnr 1234


// 6. Delete every second element of a list
// e.g. [1,2,3,4,3,2,4,5] -> [1,3,3,4]
delsecond :: [Int] -> [Int]
delsecond listOfInt = [listOfInt!!x \\ x<-[0,2..((length listOfInt) - 1)]]

// Start = delsecond [1,2,3,4,3,2,4,5]


// 7. Insert a value after every element of a list 
// [1,2,3,4,5] 0 -> [1,0,2,0,3,0,4,0,5,0]
insertx :: [Int] Int -> [Int]
insertx list elem = flatten [[x,elem] \\ x <-list]

// Start = insertx [1,2,3,4,5] 0


// 8. Insert 0 after every digit of a number: 123 -> 102030
digit0 :: Int -> Int
digit0 x = sum [z*(10^k) \\ z <- intlist & k <- [0..]]
    where 
        intlist = reverse (insertx origlist 0)
        origlist = intToList x

// Start = digit0 123  


// digit0 x = sum [z*(10^k) \\ z <- (reverse (insertx (intToList x) 0)) & k <- [0..]]

// digit0 x = sum [z*(10^k) \\ z <- intlist & k <- [0..]]
//     where intlist = reverse (insertx (intToList x) 0)