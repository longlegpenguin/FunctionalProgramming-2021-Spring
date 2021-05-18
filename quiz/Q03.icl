module Q03
import StdEnv

//G1
f::Int [Int] Int -> Int
f a [c] b = a + (length [c]) + b // _只能用于单独一个的， [a]用作list
// f a [_] b = a + (length _) + b  //compile error

// Start = f 1 [2] 3

// Start = hd [1..5]