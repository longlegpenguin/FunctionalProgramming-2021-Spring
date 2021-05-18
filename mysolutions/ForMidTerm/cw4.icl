module cw4
import StdEnv

// 1.Use foldr to check if every element in a list is a power of n?
powrn :: Int -> [Int]
powrn n = [1] ++ [n^x \\ x<-[1..]]

f1::Int [Int]-> Bool
f1 n list = (foldr (\x l | x == hd ((dropWhile ((>)x) (powrn n))) = l = [x]++l) [] list) == []

// Start = f1 2 [1,2,4,8]  //True

// Start = f1 2 [1,2,4,80] //False

// Start = f1 9 [] //True


// 2.Use list comprehension to generate the list [1,2,2,4,4,4,4,8,8,8,8,8,8,8,8,16...16,32...32]

//f2 :: [Int]

//Start = f2


//3. Use list comprehension to Insert '1' to the head of first sublist 

//and '2' to the second position of the  sencond sublist, and so on.

f3::[[Int]]->[[Int]]
f3 llist = [insertAt y (y+1) x \\ x<-llist & y<-[0..]]


// Start = f3 [[5],[6,9],[],[7,1,3,6]]  //[[1,5],[6,2,9],[3],[7,1,3,4,6]]

// Start = f3 [[0,3],[],[6,7,7,7]] //[[1,0,3],[2],[6,7,3,7,7]]





