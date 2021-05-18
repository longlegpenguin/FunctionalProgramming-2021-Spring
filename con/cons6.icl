module cons6
import StdEnv

// add :: Int -> Int
// add x = x + 1
// myMap :: (a -> b) [a] -> b
// myMap _ [] = 0
// myMap func [x:xs] = func x + myMap func xs
// Start = myMap add [1..5]

dotProd :: [Int] [Int] -> Int
dotProd list1 list2 = sum [x*y \\ x<-list1 & y<-list2]

// Start = dotProd [1,2,3] [2,3,4]

/* 
 * give a range, give back [(even, firstprime,secndprime)]
 * where firstprime+secondprime = x
 * x is even number between the range
 * firstprime will be the smallest possible prime
 */
isPrime :: Int -> Bool
isPrime n = length [x\\x<-[2..n-1] | n rem x == 0] == 0

decompose :: Int -> (Int,Int,Int)
decompose num 
| length comlist < 1 = abort "Wrong"
= hd comlist
where 
    comlist = [(num,fstprime,secprime) \\ fstprime<-[2..num],secprime<-[2..num] 
               |(fstprime + secprime == num) && isPrime fstprime && isPrime secprime]

evenPrime :: Int Int -> [(Int,Int,Int)]
evenPrime lower upper = [decompose x \\ x<-[lower..upper] | isEven x]

// Start = evenPrime 10 20
// Start = decompose 24
// Start = isPrime 3