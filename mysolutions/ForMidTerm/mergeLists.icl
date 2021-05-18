module mergeLists
import StdEnv

// Given two sorted lists A and B, generate all possible lists such that first element is taken from A then from B then from A and so on 
// in increasing order till the lists exhausted. The generated lists should end with an element from B.

mergeLists :: [Int] [Int] -> [Int]
mergeLists _ [] = []
mergeLists [] _ = []
mergeLists [a: as] [b : bs] = [a,b : mergeLists as bs]

Start = mergeLists [1..4] [10..20]