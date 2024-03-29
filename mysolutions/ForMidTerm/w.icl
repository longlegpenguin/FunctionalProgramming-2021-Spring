module w
import StdEnv


/**1
  * Write a function, that takes a list of functions, and a list of

  * tuples (Int, Int) where the first Int indicates which function to

  * use and the second Int acts as a parameter and returns a list of

  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */

Router :: [(a->b)] [(Int,a)] -> [b]
// Router _ [] = []
// Router [] _ = []
// Router f list = [(f!!(n-1)) a \\ (n,a) <- list]
Router fuclist tuplelist = foldr (\(a,b) y = [(fuclist!!(a-1)) b] ++ y ) [] tuplelist

// Start = Router [isEven,isOdd] [] //[]
// Router f [(n,a):xs] = map ((f!!(n-1)) a ) [(n,a):xs]
// Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

// Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

// Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]

// Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]

/**2
  * Write a function that takes a list of integers and returns a list of

  * result integers based on how many integers were in the parameter list.

  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)

  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]

  * For 3 integers 'a','b','c' , it will return (a*(b^c))

  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */

Listing :: [Int] -> [Int]
Listing [x] = [x rem 2]
Listing [x, y] = [x..y]
Listing [x,y,z] = [x*(y^z)]
Listing [a,b,c,d] = [a+b, c+d]
Listing x = []

// Start = Listing [5] //[1]

// Start = Listing [4,10] //[4,5,6,7,8,9,10]

// Start = Listing [3,5,2] //[75]

// Start = Listing [13,29,1030,307] //[42,1337]

// Start = Listing [] //[]


/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */
takeEven :: [Int] -> [Int]
takeEven x = [x!!index \\ index <- [0,2..length x - 1]]

SeqCheck :: [Int] -> Bool
SeqCheck x = and (map isOdd (takeEven x)) && and (map isEven (takeEven (drop 1 x)))
// Start = SeqCheck [1..10] //True

// Start = SeqCheck [1,2,3] //True

// Start = SeqCheck [2,3,4] //False

// Start = SeqCheck [1,3,4,5] //False

// Start = SeqCheck [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False

/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */
countapperance :: Int [Int] -> Int
countapperance n list = length (filter (\z = z == n) list)

checkEven :: [Int] -> Bool
checkEven [] = False
checkEven x = and (map (\z = (countapperance z x) rem 2 == 0) x)
// Start = checkEven [1,1,2,2,2,2,3,5,3,5] // True
// Start = checkEven [1,1,2,2,1] // False

// Start = checkEven [] //False


/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

DotProd :: [Int] [Int] -> Int
DotProd list1 list2 = sum ([x*y \\ x<-list1 & y <-list2])

// Start = DotProd [4,6,3] [6,3,7] //63

// Start = DotProd [6,3,7] [4,6,3] //63

// Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0


/**6
// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),

// the second part contains the rest. 
*/

TwoLists :: [Char] -> ([Char], [Char])
TwoLists x = ([z \\ z <- x | isMember z ['1'..'9']], [z \\ z <- x | isMember z ['a'..'z']])

// Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

// Start = TwoLists [] // ([],[])

/**7
// Given a list of lists, for each list, extract the first, middle and last element. 
*/

Points3 :: [[Int]] -> [(Int, Int, Int)]
Points3 [[]] = []
Points3 x = [(hd z, hd (drop (length z / 2) z),last z) \\ z <- x]
// Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

// Start = Points3 [[]] //[]
// Start = Points3 [] //[]

/**8

//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 

//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] 
*/
// tupleSort :: (Int,Int,Int) ->(Int,Int,Int)
// tupleSort (a,b,c)
// | a > b = tupleSort (b,a,c)
// | b > c = (a,c,b)
// = (a,b,c)

// isMtuple :: (Int,Int,Int) [(Int,Int,Int)] -> Bool
// isMtuple _ [] = False
// isMtuple (a,b,c) [(x,y,z):ss]
// | a == x && b == y && c ==z = True
// = isMtuple (a,b,c) ss
// isRight :: (Int,Int,Int)->Bool
// isRight (a,b,c) =  (a*a+b*b==c*c || a*a+c*c==b*b || a*a==b*b+c*c) && a>0 && b>0 && c>0

// notDup :: [(Int,Int,Int)]->[(Int,Int,Int)]
// notDup x = [z \\ z <- x | isRight z && not (isSame z xs)]

isSame :: (Int,Int,Int) [(Int,Int,Int)] -> Bool
isSame _ [] = False
isSame (a,b,c) [(x,y,z):ss]
| ((a==x&&b==y&&c==z) || (a==y&&b==x&&c==z) || (a==z&&b==x&&c==y) || (a==x&&b==z&&c==y)|| (a==y&&b==z&&c==x) || (a==z&&b==y&&c==x)) = True
= isSame (a,b,c) ss

takeRight :: [(Int,Int,Int)]->[(Int,Int,Int)]
takeRight x = [(a,b,c) \\ (a,b,c) <-x | (a*a+b*b==c*c || a*a+c*c==b*b || a*a==b*b+c*c) && a>0 && b>0 && c>0]

rmRepeat :: [(Int,Int,Int)] [(Int,Int,Int)]->[(Int,Int,Int)]
// rmRepeat list b = foldr (\x y | not (isSame x y) = [x] ++ y = y) [] list
rmRepeat [] b = b
rmRepeat [x:xs] b
| isSame x b = rmRepeat xs b
= rmRepeat xs (b++[x])

f8::[(Int,Int,Int)]->[(Int,Int,Int)]
f8 x = rmRepeat (takeRight x) []
// f8 x = reverse (notDup (reverse x))

// Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

// Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]


/**9
 * Use foldr to check if the square root of each integer in a list are all integers. 
 */

f9::[Int] ->Bool
f9 x = length (foldr (\x y |(sqrt (toReal x))^2.0 == (toReal x) = [x] ++ y = y ) [] x) == length x

// Start = f9 [] //True

// Start = f9 [4,16,9] //True

// Start = f9 [1,8] //False

 
/* 10 Insert sum of elements as last element in every sublist of a list. */

addSum :: [[Int]] -> [[Int]]
addSum llist = [x ++ [sum x] \\ x<-llist]
// Start = addSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]