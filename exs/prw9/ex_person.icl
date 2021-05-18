module ex_person

import StdEnv

//TASK: Add "_qualify" to the name of persons that are over age of 18 in a tree of persons. 
::Person = { name::String
			, birthday::(Int,Int,Int)
	       }
::Tree a = Node a (Tree a) (Tree a)
	     | Leaf
t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2005,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (1999,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2003,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2005,11,23)} Leaf Leaf)

extractNode :: (Tree a) -> a
extractNode (Node x l r) = x

over18 :: (Int,Int,Int) -> Bool
over18 (a,b,c) 
| a > 2002 = False
= True

isLeaf :: (Tree Person) -> Bool
isLeaf Leaf = True
isLeaf _ = False

qualify :: Person -> Person
qualify a = {a & name = a.name +++ "_qualify"}

//Start = ((extractNode t2).name) +++ "_qualify"
//Start = qualify {name = "hh", birthday = (2001,11,22)}

f3 :: (Tree Person) -> (Tree Person)
f3 Leaf = Leaf
f3 (Node x l r)
| not (over18 x.birthday) = Node x (f3 l)(f3 r)
= Node (qualify x) (f3 l) (f3 r)

//Start = f3 t2 //(Node (Person "hh" (2005,11,22)) (Node (Person "hr_qualify" (2001,11,21)) Leaf Leaf) (Node (Person "ht_qualify" (2001,11,23)) Leaf Leaf))
Start = f3 t3 //(Node (Person "hh_qualify" (1999,11,22)) (Node (Person "hr_qualify" (2001,11,21)) (Node (Person "hh" (2003,11,22)) Leaf Leaf) (Node (Person "hh_qualify" (1998,11,22)) Leaf Leaf)) (Node (Person "ht" (2005,11,23)) Leaf Leaf))
 