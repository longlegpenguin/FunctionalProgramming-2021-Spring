module cw7
import StdEnv

/*1. generate function as following: 
we pick out the element who will get maximum result 
after applied the function(second parameter).
[1,2,3,4,5] ((*) (~1))  -> 1
[1,2,3,4,5] ((*) 2)  -> 5
 if we have a function : f x = (-1)*(3-x)^2
then : [1,2,3,4,5] f  -> 3
*/

f1 :: [Int] (Int->Int) -> Int
f1 list func = snd (last sortedlist)
    where
        sortedlist = sort funclist
        funclist = [(func x, x) \\ x <-list]

// Start = f1 [1,2,3,4,5] ((*) (~1))  //1

// Start = f1 [1,2,3,4,5] ((*) 2)  //5

f x = (-1)*(3-x)^2

// Start = f1 [1,2,3,4,5] f //3  


/*2. Given a list and a number n,
we group the list to sublist in every n number.
e.g. [1,2,3,4,5] 2 -> [[1,2],[3,4],[5]]

*/

f2::[Int] Int -> [[Int]]
f2 [] _ = []
f2 list n = [take n list : f2 (drop n list) n]

// Start = f2 [1,2,3,4,5] 2 //[[1,2],[3,4],[5]]
// Start = f2 [1..10] 6 //[[1,2,3,4,5,6],[7,8,9,10]]


/*
3. find the avege value of the nodes in a tree
*/

:: Tree a = Node a (Tree a) (Tree a)
          | Leaf

atree = Node 6 (Node 3 (Node 6 Leaf Leaf) Leaf) Leaf

// depthT :: (Tree Int) -> Int
// depthT Leaf = 0
// depthT (Node x le ri) = 1 + (max (depthT le) (depthT ri))
num_of_Node::(Tree Int) -> Int
num_of_Node Leaf = 0
num_of_Node (Node x l r) = 1 + num_of_Node l + num_of_Node r 

sumT :: (Tree Int) -> Int
sumT Leaf = 0
sumT (Node x le ri) = x + sumT le + sumT ri

f3::(Tree Int) -> Int
f3 Leaf = 0
f3 tree = (sumT tree) / (num_of_Node tree)

// Start = f3 Leaf  //0
// Start = sumT atree
// Start = f3 atree //5

