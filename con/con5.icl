module con5
import StdEnv

something :: [(Int, Bool)] -> [Int]
something list = foldr (\(a,b) y |b = [a] ++ y = y) [] list

isSmaller :: [(Int, Int)] -> Bool
isSmaller list = foldr (\(a,b) y = b > a  && y) True list

// Start = isSmaller [(1,2), (6,4), (4,5)]

Router :: [(a->b)] [(Int, a)] -> [b] 
Router [] _ = []
Router fuclist tuplelist = foldr (\(a,b) y = [(fuclist!!(a-1)) b] ++ y ) [] tuplelist

Start = Router [isEven,isOdd] [] //[]
// Start = toInt isEven
// Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

// Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

DotProd :: [Int] [Int] -> Int
DotProd l1 l2 = sum [a*b \\ a <-l1 & b<-l2]
// Start = DotProd [4,6,3] [6,3,7] //63

// Start = DotProd [6,3,7] [4,6,3] //63

// Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0


/**6
// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),

// the second part contains the rest. 
*/

// TwoLists :: [Char] -> ([Char], [Char])
// TwoLists x = ([z \\ z <- x | isMember z ['1'..'9']], [z \\ z <- x | isMember z ['a'..'z']])

// TwoLists list = foldr (\x (numList,charList) | isDig x = ([x] ++ numList,charList) = (numList,[x] ++ charList)) ([],[]) list
// Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])

// Start = TwoLists [] // ([],[])