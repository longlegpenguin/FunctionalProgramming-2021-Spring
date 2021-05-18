module PT10
import StdEnv

/*
Write binary tree ADT. It should have two types: Node with value and left and right subtrees and Leaf.
Write preOrderTraversalPrefix function which takes two arguments N and tree. It should return at most N first
elements from given tree's preorder list. Preorder list is a list of values from the tree, where each node's
value is on the left side of it's all children nodes.

Ex.
Tree: 1
/ \
2 3
/\
4 5
Preorder list: [1, 2, 3, 4, 5]
*/

// TODO
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


// TODO
preOrderTraversalPrefix:: Int (Tree a) -> [a]
preOrderTraversalPrefix _ Leaf = []
preOrderTraversalPrefix n tree = take n (preorder tree)

preorder :: (Tree a) -> [a]
preorder Leaf = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r 

// Start = preOrderTraversalPrefix 10 tree1 // [4,10,6,11,20,12]
// Start = preOrderTraversalPrefix 4 tree2 // [5,10,31,1]
// Start = preOrderTraversalPrefix 0 tree3 // []
// Start = preOrderTraversalPrefix 20 tree4 // [7,11,4,10,6,11,20,12,5,10,31,1,17,31,14,12,11,5,12,11]
// Start = preOrderTraversalPrefix 27 tree5 // [1,12,11,11,32,4,17,5,7,3,4,7,11,4,10,6,11,20,12,5,10,31,1,17,31,14,12]

