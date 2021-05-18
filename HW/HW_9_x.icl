module HW_9
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

minRoot :: Tree Int
minRoot = (Node 4 (Node 10 (Node 11 Leaf Leaf)(Node 16 Leaf Leaf)) (Node 22 (Node 15 Leaf Leaf) Leaf))

minMostLeftLeaf :: Tree Int
minMostLeftLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

minMostRightLeaf :: Tree Int
minMostRightLeaf = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 22 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf (Node 1 Leaf Leaf)) ))

minNode :: Tree Int
minNode = (Node 4 (Node 10 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 16 (Node -12 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 100 Leaf Leaf) ))

/*
1. Given a Tree of integers, find the minimum value of all the nodes.
*/
TreeToList :: (Tree Int) -> [Int]
TreeToList Leaf = []
TreeToList (Node x l r) = TreeToList l ++ [x] ++ TreeToList r 

minTree :: (Tree Int) -> Int
minTree tree = hd sorted
    where
        sorted = sort list
        list = TreeToList tree

// Start = minTree minRoot // 4
// Start = minTree minMostLeftLeaf //2
// Start = minTree minMostRightLeaf //1
// Start = minTree minNode // -12

/*
2. Given a list of Courses that a student has taken,
find the credits they earned if
for each CS major course, he gets 3 more credits
*/

// creds :: [Course] -> Int

 

//Start = creds [Compilers, Astronomy, Basic] // 16
//Start = creds [Thermo_Dynamics, Relativity, Numerical_Methods] // 14
//Start = creds [Compilers, Functional, Programming] //23
//Start = creds [] // 0

// :: Tree a = Node a (Tree a) (Tree a ) | Leaf

// :: (Tree Course)
// treea = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) (Node Thermo_Dynamics Leaf Leaf)) Leaf)
// (Node Basic (Node Compilers (Node Relativity Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

// treeb = (Node Functional (Node Astronomy (Node Programming (Node Astronomy Leaf Leaf) Leaf) Leaf)
// (Node Relativity (Node Compilers (Node Numerical_Methods Leaf Leaf) (Node Astronomy (Node Analysis Leaf Leaf) Leaf)) Leaf))

// treec = (Node Analysis Leaf (Node Programming Leaf (Node Astronomy Leaf (Node Basic Leaf (Node Compilers Leaf
// (Node Thermo_Dynamics Leaf (Node Numerical_Methods Leaf (Node Functional Leaf Leaf))))))))

/*
3. Given a tree of Courses, give back all the CS Courses
whose both children are either Physics courses or Leaf-s.
*/

// getphysics :: (Tree Course) -> [Course]

 

//Start = getphysics treea // [(Course "Programming" CS 5),(Course "Compilers" CS 4)]
//Start = getphysics treeb // [(Course "Functional" CS 5),(Course "Programming" CS 5)]
//Start = getphysics treec // [(Course "Programming" CS 5),(Course "Compilers" CS 4),(Course "Functional" CS 5)]