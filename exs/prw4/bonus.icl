module bonus
import StdEnv

// 11. (bonus point) rewrite map using foldr
ff f y ys = [(f y): ys]
//          (f element : [] )
mymap :: (a -> b) [a] -> [b]
mymap f x = foldr (ff f) [] x


myFilter:: (Int -> Bool) [Int] -> [Int]
myFilter condition list = foldr (\x y |condition x = y ++ [x] = y) [] list

Start = myFilter isEven [1..10]
// Start = mymap inc [1..10]
// Start = mymap isOdd [1..10] //[True,False,True,False,True,False,True,False,True,False]

// foldr  (ff inc 1 (ff inc 2...(ff inc 9 (ff inc 10 []))))
