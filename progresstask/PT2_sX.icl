module PT2_sX
import StdEnv


// Given a list of integers, write a function that returns a list of 2 integers
// , where the first integers is a sum of elements on odd indexes 
// (start indexing from 1) and the second integers is a sum of 
// elements on even indexes.

// onlyThirds2 :: [Int] -> [Int]
// onlyThirds2 [] = [0]
// onlyThirds2 a = [sumoflist([(a!!0)] ++ onlyThirds2 (drop 2 a))] 
// sumoflist :: [Int] -> Int
// sumoflist [] = 0
// sumoflist [x:xs] = x + sumoflist xs

// 方法一
onlyThirds :: [Int] -> [Int]
onlyThirds a = [onlyThirdsAux a] ++ [onlyThirdsAux (drop 1 a)]

onlyThirdsAux :: [Int] -> Int
onlyThirdsAux [] = 0
onlyThirdsAux a = (a!!0) + onlyThirdsAux (drop 2 a)

// 方法二
onlyThirds4 :: [Int] -> [Int]
onlyThirds4 list = [sum a,sum b]
where
    a = [list!!a \\ a<-[0,2.. (length list)-1]]
    b = [list!!b \\ b<-[1,3.. (length list)-1]]

// onlyThirdsAux :: int [Int] [Int] -> [Int]
// onlyThirdsAux n [a] [x, y]
// | n > length a = [x, y]
// | n rem 2 == 0 = onlyThirdsAux n+1) a [x+a, y]
// = onlyThirdsAux 1 a [x, y+a]


 
// Start = onlyThirds [1,2,2,AuxonlyThirdsAux,4] // [7,5]  
// Start = onlyThirds [AuxonlyThirdsAux] // [AuxonlyThirdsAux,0]
Start = onlyThirds4 [1,2] // [1,2]