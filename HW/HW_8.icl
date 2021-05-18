module HW_8
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))

tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))

tree5 :: Tree Int
tree5 = Node 1 tree3 tree4

/* 1. Given the binary tree, count number of leaf nodes in the tree.
*/

countLeaves :: (Tree a) -> Int
countLeaves Leaf = 1
countLeaves (Node x l r) = countLeaves l + countLeaves r 

// Start = countLeaves tree1 // 7
// Start = countLeaves tree2 // 10
// Start = countLeaves tree3 // 11
// Start = countLeaves tree4 // 38
// Start = countLeaves tree5 // 49

/* 2. Given the binary tree, find how many nodes are there such that they have exactly
3 grandchildren non-leaf nodes.
Ex.: 1
    / \
   2   3
  / \ / \
 4  5 6 Leaf
1st node has exactly 3 grandchildrens, so it's a 'good' node.
*/
goL :: (Tree Int) -> (Tree Int)
goL (Node x l r) = l 

goR :: (Tree Int) -> (Tree Int)
goR (Node x l r) = r 

isLeaf :: (Tree Int) -> Int
isLeaf Leaf = 1
isLeaf _ = 0

isGood :: (Tree Int) -> Bool
isGood (Node x Leaf _) = False
isGood (Node x _ Leaf) = False
isGood (Node x l r) = (isLeaf(ll) + isLeaf(lr) + isLeaf(rr) + isLeaf(rl) == 1)
    where
        ll = goL l
        lr = goR l 
        rr = goR r 
        rl = goL r 

countTripleParents :: (Tree Int) -> Int
countTripleParents Leaf = 0
countTripleParents (Node x l r)
| isGood (Node x l r) = 1 + countTripleParents l + countTripleParents r 
= countTripleParents l + countTripleParents r 

// Start = countTripleParents tree1 // 1
// Start = countTripleParents tree2 // 1
// Start = countTripleParents tree3 // 1
// Start = countTripleParents tree4 // 4
// Start = countTripleParents tree5 // 5

/* 3. Implement a function that interleaves three arrays. So for input arrays {1,2,3}, {4,5,6}
and {7,8,9} the function must return the array {1,4,7,2,5,8,3,6,9}. If an array is out of elements
we continue interleaving the remaining arrays.
Example: {1,2} {3,4,5,6} {7,8,9} -> {1,3,7,2,4,8,5,9,6}
*/
listToArr :: [Int] -> {Int}
listToArr li = {x \\ x<-li}

arrTOList :: {Int} -> [Int]
arrTOList arr = [x\\x<-:arr]

ilFor2List :: [Int] [Int] -> [Int]
ilFor2List [] [] = []
ilFor2List [] l2 = l2
ilFor2List l1 [] = l1
ilFor2List [x:xs] [y:ys] = [x,y] ++ ilFor2List xs ys

ilFor3List :: [Int] [Int] [Int] -> [Int]
ilFor3List [] [] [] = []
ilFor3List [] l1 l2 = ilFor2List l1 l2 
ilFor3List l1 [] l2 = ilFor2List l1 l2 
ilFor3List l2 l1 [] = ilFor2List l1 l2 
ilFor3List [] [] l3 = l3
ilFor3List [] l3 [] = l3
ilFor3List l3 [] [] = l3
ilFor3List [x:xs] [y:ys] [z:zs] = [x,y,z] ++ ilFor3List xs ys zs

interleave :: {Int} {Int} {Int} -> {Int}
interleave arr1 arr2 arr3 = listToArr goodlist
    where
        goodlist = ilFor3List l1 l2 l3
        l1 = arrTOList arr1
        l2 = arrTOList arr2
        l3 = arrTOList arr3

// Start = interleave {1,2,3} {4,5,6} {7,8,9} // {1,4,7,2,5,8,3,6,9}
// Start = interleave {1,2} {3,4,5,6} {7,8,9} // {1,3,7,2,4,8,5,9,6}
// Start = interleave {} {1,2,3} {4} // {1,4,2,3}
// Start = interleave {} {} {} // {}
// Start = interleave {1,2} {3,4,5} {6,7,8,9,10,11,12} // {1,3,6,2,4,7,5,8,9,10,11,12}
