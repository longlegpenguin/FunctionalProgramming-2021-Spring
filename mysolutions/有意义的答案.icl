/* 
 * 目录
 * 1. primeDivisors 素数除数                                 
 * 2. OddthEventh 针对list偶次位和积次位的元素做不同操作         
 * 3. modd 把表中符合要求的元素提前                             
 * 4. isPrime 检测是否是素数                                  
 * 5.
 * 6.
 * 7.
 * 
 *
 *
 *
 *
 *
 *
 *
 */

/*
 * 素数除数，第一个参数给要拆的数字，第二个参数是起始的divisor，建议从2开始。
 * 如果遇到除数，除数不增加，一直除到该除数除完，所以list有重复，用 removeDup 去掉重复。
 * 除数从最小的除数开始，所以不会出现要拆的数字除完除数以后比下一个要试的数字小了。 
*/
primeDivisors :: Int Int -> [Int]
primeDivisors 0 _ = []
primeDivisors x d 
| d == x = [x]
| x rem d == 0 = [d : primeDivisors (x/d) d]
= primeDivisors x (d + 1)

/* 针对list偶次位和积次位的元素做不同操作
 * 1，3，5，...进行 A(x)
 * 0，2，4，6...进行 B(x)
 * 第一个参数是表中表，第二个参数是起始的位置，建议 0 。 
 */
OddthEventh :: [[Int]] Int -> [[Int]]
OddthEventh x n
| n == length x = []
| n rem 2 == 1 = [A (x!!n) : OddthEventh x (n+1)]
= [B (x!!n) : OddthEventh x (n+1)]

/* 
 * 把表中符合要求的元素提前
 * 挑出符合的和不符合的最后合并
 */
modd :: [Int] -> [Int]
modd x = [a \\ a <- x | isOdd a && a < 7] ++ [a \\ a <- x | isEven a || a >= 7]

/* 
 * 检测是否是素数
 * 第一个参数给要检测的数字，第二个参数给要检测的数字减一
 * 没有限制负数
 */
isPrime :: Int Int -> Bool
isPrime _ 1 = True
isPrime a b
| a < 2 = False  // 0,1 are not prime. 
| a rem b == 0 = False
= isPrime a (b-1)

/* 表中偶数单独挑出成组 */
keepeven :: [Int] -> [Int]
keepeven x = [a \\ a <- x | isEven a]

/* 删除中间项，如果是偶数个元素，不删除 */
deletemid :: [Int] -> [Int]
deletemid xs
| isEven(length xs) = xs
= take n xs ++ drop (n+1) xs
where n = length xs / 2

/* 两列表对应位置是否一样 */
same :: [Int] [Int] -> Bool
same [] [] = True
same [x : xs] [y : ys]
| (isOdd x && isOdd y) || (isEven x && isEven y) = same xs ys
= False

/* 把数字转成表 */
turn_into_list :: Int -> [Int]
turn_into_list a
| a < 10 = [a]
= turn_into_list (a / 10) ++ [a rem 10]

/* 偶次位和 */
onlyThirdsAux :: [Int] -> Int
onlyThirdsAux [] = 0
onlyThirdsAux a = (a!!0) + onlyThirdsAux (drop 2 a)

/* fibonacci */
fib :: Int -> Int
fib n = fibAux n 1 1

fibAux 0 a b         = a
fibAux i a b | i > 0 = fibAux (i-1) b (a+b)

/* 
 * Write a function that checks if a list of numbers is odd,even,odd,even...
 * e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.
 */
takeEven :: [Int] -> [Int]
takeEven x = [x!!index \\ index <- [0,2..length x - 1]]

f7 :: [Int] -> Bool
f7 x = (and (map isOdd (takeEven x)) && and (map isEven (takeEven (drop 1 x))))

// 阶乘
factorial :: Int -> Int
factorial n = prod [1..n]

/* 
 * 数 n 在列表里出现的次数
 * 返回次数
 */
cntApperance :: Int [Int] -> Int
cntApperance n list = length (filter (\z = z == n) list)

/* 
 * 找到有 a 的tuple，
 * 并输出 b
 */
search :: a [(a,b)] -> b | == a
search s [(x,y):ts]
| x == s = y
| otherwise = search s ts

/* 
 * generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]
 * 输出的是正常的1..10的表， 输出多少遍由 y 来决定，
 * 因为是全排列，全排列但是不看后一个
 */
l5 :: [Int]
l5 = [x \\ x <- [1..10], y <- [1..x]]
// l5 = [snd(y,x) \\ x <- [1..10], y <- [1..x]]

/* 
 * 表中表
 * 先确定表中需要的数字，放在最后
 * 将他的每个元素送出，通过另一个确定范围的表，
 * 用来数送出的次数
 * 在comprehension里套一个comprehension
 * generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
 */
l6 :: [[Int]]
l6 = [ [y \\ x <- [1..y]] \\ y <- [1..10]]

/* 
 * 找三角形数
 * 错误示例：
 * l7 = take 6 [(x,y,z) \\ x <- [1..] , y <- [x..] , z <- [y..] |x*x + y*y == z*z]
 * 不能往大了找，因为如果 x = 1 没有pythagoras那找到无限大都没有结果。
 * 正确：只有最长的边可以无限拉长，两条短边被最长边固定，不能超过最长边！
 * generate 6 pythagoras numbers : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
 * 还有一个注意点： 用‘&’是无法前后限制的，要用‘,’才可以。
 *               除非一定要一一对应的情况，不然就使用‘,’。
*/
// 7. 
l7 :: [(Int, Int, Int)]
l7 = take 6 [(x,y,z) \\ z <-[1..], y <-[1..z], x <-[1..y] |  x*x + y*y == z*z]

/* Write a function that removes consecutive duplicates in a list.
 * 删除连续重复
 * 只要重复就一直删
 */
f8 :: [Int] -> [Int] 
f8 [] = []
f8 [a] = [a]
f8 [x, y : xs]
| x == y = f8 (dropWhile (\z = z == x) [x, y : xs])
= [x] ++ f8 [y:xs]

/**8
 * 因为特别复杂就记录一下
 * Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
 *
 * only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] 
 */
// Tuple所含的三个元素是一样的，返回TRUE 
isSame :: (Int,Int,Int) [(Int,Int,Int)] -> Bool
isSame _ [] = False
isSame (a,b,c) [(x,y,z):ss]
| ((a==x&&b==y&&c==z) || (a==y&&b==x&&c==z) || (a==z&&b==x&&c==y) || (a==x&&b==z&&c==y)|| (a==y&&b==z&&c==x) || (a==z&&b==y&&c==x)) = True
= isSame (a,b,c) ss
// 取出是直角三角形的
takeRight :: [(Int,Int,Int)]->[(Int,Int,Int)]
takeRight x = [(a,b,c) \\ (a,b,c) <-x | (a*a+b*b==c*c || a*a+c*c==b*b || a*a==b*b+c*c) && a>0 && b>0 && c>0]
/// 元素相同的去掉
rmRepeat :: [(Int,Int,Int)] [(Int,Int,Int)]->[(Int,Int,Int)]
rmRepeat [] b = b
rmRepeat [x:xs] b
| isSame x b = rmRepeat xs b
= rmRepeat xs (b++[x])
/// 出来顺序反了
// rmRepeat list b = foldr (\x y | not (isSame x y) = [x] ++ y = y) [] list

f8::[(Int,Int,Int)]->[(Int,Int,Int)]
f8 x = rmRepeat (takeRight x) []