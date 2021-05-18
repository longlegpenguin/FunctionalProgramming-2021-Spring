module cw8
import StdEnv

/*
1. Given a tree and an integer n, find the nodes equal to n and replace by '0'
*/

:: Tree a = Node a (Tree a) (Tree a)
          | Leaf

f1 :: Int (Tree Int) -> (Tree Int) 
f1 _ Leaf = Leaf
f1 n (Node x le ri)
| x == n = Node 0 (f1 n le) (f1 n ri)
= Node x (f1 n le) (f1 n ri)


// Start = f1 3 atree  //(Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) (Node 6 (Node 0 Leaf Leaf) (Node 7 Leaf Leaf)))
atree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))


/*
2. Given a tree, find the level between max node and min node
*/
btree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf)(Node 7 Leaf Leaf))

ctree =  Node 4 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))

// nDepth :: Int (Tree Int) ->Int
// nDepth _ Leaf = 0
// nDepth chek (Node x le ri) 
// | x == chek = 1
// = 1 + max (nDepth chek le) (nDepth chek ri)

myMin :: Int Int -> Int
myMin 0 x = x
myMin x 0 = x
myMin x y = min x y

nDepth :: Int Int (Tree Int) ->Int
nDepth _ _ Leaf = 0
nDepth chek cnt (Node x le ri) 
| x == chek = cnt
= myMin (nDepth chek (cnt+1) le) (nDepth chek (cnt+1) ri)

f2 :: (Tree Int) ->Int
f2 tree = nDepth (last treelist) 1 tree - nDepth (hd treelist) 1 tree
    where 
        treelist = sort (inorder tree)
        inorder :: (Tree Int) ->[Int]
        inorder Leaf = []
        inorder (Node x le ri) = inorder le ++ [x] ++ inorder ri

// Start = nDepth 16 1 ctree
// Start = f2 ctree //3
// Start = f2 btree //0

/*
3. 
Define algebraic type : Day (Mon,Tue,Wed,Thu,Fri,Sat,Sun).
And define function IsWeekend :: Day -> Bool to check if it is Sat or Sun.
if it is weekend, then output "Happy day!",Otherwise,"Oh my god"
*/


:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

f3 :: Day -> String
f3 Sat = "Happy day!"
f3 Sun = "Happy day!"
f3 _ = "oh my god"

// Start = f3 Sun  // "Happy day!"
// Start = f3 Tue  // "Oh my god"
