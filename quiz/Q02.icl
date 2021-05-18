module Q02
import StdEnv

//G2_3
//Runtime error
// f :: [Int] -> Int
f [] = 1 // 缺了这一行，没有定义【】那就变成 1*2*3*4*【】*f【】了
f [x:xs] = x * f xs

// Start = f [1..4]

// G2_4 compile error
add100 :: Real -> Real 
add100 x = x + 100.0  //缺 .0

// Start = add100 3.0


myFunc [a:b] = b
Start = myFunc(myFunc(myFunc[1,2,3]))