module extraHW
import StdEnv

/*
1. Given a predefined Shape type, argument of the Circle constructor is the radius,
side length for Square, and equilateral Triangle, width and height for Rectangle
write a function that calculates the area and circumference
of each shape in the array, store the results of each shape as a tuple in an array.
*/
:: Shape = Circle Real
| Square Real
| Triangle Real
| Rectangle Real Real

caShape :: Shape -> (Real, Real)
caShape (Circle r) = (3.14*r*r, 2.0*3.14*r)
caShape (Square a) = (a*a, 4.0*a)
caShape (Triangle l) = (l*sqrt(3.0)*l/4.0, 3.0*l)
caShape( Rectangle a b) = (a*b, a+a+b+b)

calc :: {Shape} -> {(Real, Real)}
calc arr = {caShape x \\ x<-:arr}

// Start = calc {(Circle 3.0), (Square 2.5)} // {(28.26,18.84),(6.25,10)}
// Start = calc {(Triangle 4.3), (Rectangle 5.4 7.2), (Circle 2.45)} // {(8.00640485798713,12.9),(38.88,25.2),(18.84785,15.386)}
// Start = calc {(Triangle 7.6), (Circle 1.75), (Square 0.95)} // {(25.0108136612946,22.8),(9.61625,10.99),(0.9025,3.8)}

/*
2. Given a predefined MaybeInt type, define a new operator !+!
for accessing the nth element in the list, you can test it with showFifthElement function.
*/

:: MaybeInt = Just Int | Nothing

(!+!) :: [Int] Int -> MaybeInt
(!+!) list n
| length list <= n = Nothing
= Just (list!!n)

showFifthElement :: [Int] -> String
showFifthElement xs
= case xs !+! 4 of
    Nothing -> "There is no fifth element in this list"
    Just n -> "The fifth element of the list is: " +++ toString(n)

// Start = showFifthElement [1,2..10] // "The fifth element of the list is: 5"
// Start = showFifthElement [0,0] // "There is no fifth element in this list"
// Start = showFifthElement [33, 41, 56, 12, 96, 1] // "The fifth element of the list is: 96"

/*
3. Given a predefined type Either,
give a valid implementation of the following type signature
hint: view the types like a * (b + c) = a * b + a * c
tuple is a product type and Either is a sum type
*/
:: Either a b = Left a | Right b

prodToSum :: (a, Either b c) -> Either (a, b) (a, c)
prodToSum (n, Left x) = Left (n, x)
prodToSum (n, Right x) = Right (n, x)

// Start = prodToSum (1, Right (Left "one")) // (Right (1,(Left "one")))
// Start = prodToSum (404, Left "Not Found") // (Left (404,"Not Found"))