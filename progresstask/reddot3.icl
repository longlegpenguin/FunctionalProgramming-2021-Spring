module reddot3
import StdEnv

combinations :: Int [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n [x:xs] = (map (\z = [x: z]) (combinations (n-1) xs)) ++ (combinations n xs)

// Start = combinations 5 [1,2,3,4,5] // [[1,2,3,4,5]]
// Start = combinations 5 [1,2,3] // []
// Start = combinations 2 ["a", "b", "c", "d"] // [["a","b"],["a","c"],["a","d"],["b","c"],["b","d"],["c","d"]]
// Start = length (combinations 3 ['a'..'z']) // 2600
// Start = combinations 2 [True, False, True] // [[True,False],[True,True],[False,True]]