module endterm_retake

import StdEnv
//-------------------------------------------------------------------------------
/*
	1. Create a `toInt` instance for the Person record. An integer representation of a person
	is the sum of the length of its firstName, their age and height.
*/

::Person={firstName::String, age::Int, height::Int}
Rose::Person
Rose={firstName="Rose",age=23,height=172}
Jack::Person
Jack={firstName="Jack",age=25,height=193}
Emilia::Person
Emilia={firstName="Emilia",age=15,height=160}
Leo::Person
Leo={firstName="Leo",age=16,height=175}
Grace::Person
Grace={firstName="Grace",age=35,height=165}
Harry::Person
Harry={firstName="Harry",age=42,height=180}
Emilia2::Person
Emilia2={firstName="Emilia",age=15,height=180}

// TO DO instance
instance toInt Person
    where
        toInt p = p.age+p.height+(length [x\\x<-:p.firstName])
 
// Start = toInt Rose // 199
// Start = toInt Leo // 194
// Start = toInt Grace // 205

//-------------------------------------------------------------------------------
/*
	2. Create an instance of `isEven` for the Person record. A person is even if the sum of their
	age and height is even.
*/
// TO DO instance
instance isEven Person
    where 
        isEven p = (p.age+p.height) rem 2 == 0


//Start  = isEven Rose // False
// Start = isEven Harry // True

//-------------------------------------------------------------------------------

// 3. Given a list of continents, give back the names of the continents that have 
// at least one country whose continent has prime number of 'i' in it.

::Country={name::String,capital::String}
Macedonia::Country
Macedonia={name="Macedonia",capital="Skopje"}
Hungary::Country
Hungary={name="Hungary",capital="Budapest"}
Spain::Country
Spain={name="Spain",capital="Madrid"}
Brazil::Country
Brazil={name="Brazil",capital="Brasilia"}
Chile::Country
Chile={name="Chile",capital="Santiago"}
Argentina::Country
Argentina={name="Argentina",capital="Buenos Aires"}
China::Country
China={name="China",capital="Beijing"}
India::Country
India={name="India",capital="New Delhi"}

::Continent={contName::String,countries::{Country}}
Europe::Continent
Europe = {contName="Europe",countries={Macedonia,Hungary,Spain}}
Asia::Continent
Asia = {contName="Asia",countries={China,India}}
SouthAmerica::Continent
SouthAmerica ={contName="South America",countries={Argentina,Brazil,Chile}}



isPrime :: Int -> Bool
isPrime n
| n <= 1 = False
= isEmpty[x\\x<-[2..(n-1)]|n rem x == 0]

occur :: String -> Bool
occur cname = isPrime (length [1\\a<-:cname | (a=='i' || a=='I')])

//Start = occur "Beijing"

// TO DO continentsPrimeI
atleastone :: {Country} -> Bool
atleastone carr = (length finallist) > 0
    where
        finallist = [x \\ x<- list | occur x.capital]
        list = [x \\ x<-: carr]

continentsPrimeI :: [Continent] -> [String]
continentsPrimeI conts = [x.contName \\ x<-conts | atleastone x.countries ]

// Start=continentsPrimeI [Europe,Asia]//["Asia"]
// Start=continentsPrimeI [Europe]//[]
// Start=continentsPrimeI [Europe,SouthAmerica,Asia]//["South America","Asia"]


//-------------------------------------------------------------------------------
/* 4
Me and my friends went to play football in the streets, and the game ended as tie, so we were discussing 
if we should go for penalties or not. Help me to decide that.
You will get in a list each one of my team member skill Level and name, and you will get the name of 
the other team's goalkeeper and his/her level of skill.
If the skill of the player is greater or equal than the skill of the goalkeeper, then the penalty will count as scored.
The team would win this virtual game, if at least 3 or more penalties could be scored against the given goalkeeper.
*/

::APlayer = { name ::String, skillLevel :: Int}

shouldWePlay :: [APlayer] APlayer -> Bool
shouldWePlay friends enemy = length [1\\f<-friends | f.skillLevel >= enemy.skillLevel] >= 3

//Start = shouldWePlay [{name = "kareem", skillLevel = 4},{name = "Tarek", skillLevel = 3},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // False
// Start = shouldWePlay [{name = "kareem", skillLevel = 5},{name = "Tarek", skillLevel = 4},{name = "Ali", skillLevel = 3},{name="Hussien", skillLevel=2},{name="Ziad", skillLevel=4}] {name="Gemy", skillLevel=4} // True

//-------------------------------------------------------------------------------
// 5. Given an integer write it in full words. For example:
// 109 should be one-zero-nine.
// 175 should be one-seven-five
// 100 should be one-zero-zero

// TODO convert

full_words :: Int -> String
full_words n
| n == 0 = "zero"
| n < 0 = convert (~n) (-1) ("")
= convert n 1 ("")

numList = ["zero","one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

// convert :: Int Int String -> String
// convert num sign wtf
// | sign == -1 = "minus" +++ "-" +++ intToString num
// = intToString num
convert :: Int Int String -> String
convert num sign wtf
| sign == -1 = "minus" +++ "-" +++ concatanate strlist
= concatanate strlist
    where 
        numlist = numToList num
        strlist = tl (toStringList numlist)

toStringList :: [Int] -> [String]
toStringList [] = []
toStringList [x:xs] = ["-",numList!!x] ++ toStringList xs

concatanate :: [String] -> String
concatanate [] = " "
concatanate [x:xs] = x +++ concatanate xs

numToList :: Int -> [Int]
numToList a
| a < 10 = [a]
= numToList (a / 10) ++ [a rem 10]

// intToString :: Int -> String
// intToString n = (numList!!(hd nums)) +++ (concatanate flist)
//     where
//         flist = [numList!!x \\ x<-(tl nums)]
//         nums = numToList n 

// concatanate :: [String] -> String
// concatanate [] = ""
// concatanate [x:xs] = "-" +++ x +++ (concatanate xs)


// Start = full_words 0 // "zero"
// Start = full_words 175 // "one-seven-five"
// Start = full_words -5 // "minus-five"
// Start = full_words 100 // "one-zero-zero"
// Start = full_words 33333 // "three-three-three-three-three"

//-------------------------------------------------------------------------------
/*
    6. Given a predefined MaybeInt type, define a new operator !+!
    for accessing the nth element in the list, you can test it with showFifthElement function.
*/

:: MaybeInt = Just Int | Nothing

// TODO
// DEFINITION OF OPERATOR !+! ..... YOUR CODE COMES HERE....
// class maybe a
//     where
//         (!+!) :: [a] Int -> MaybeInt

// instance maybe Int
//     where 
//         !+! :: [Int] Int -> MaybeInt
//         !+! arr n 
//         | (length arr) > n = Just (arr!!n)
//         = Nothing
(!+!) infix 9 :: [Int] Int -> MaybeInt
(!+!) list i = find list i 
    where
        find :: [Int] Int -> MaybeInt
        find [x:xs] 0 = (Just x)
        find [x:xs] n = find xs (n-1)
        find [] _ = Nothing
//Just for testing purposed. DO NOT MODIFY
showFifthElement :: [Int] -> String
showFifthElement xs
  = case xs !+! 4 of
      Nothing -> "There is no fifth element in this list"
      Just n  -> "The fifth element of the list is: " +++ toString n

// Start = showFifthElement [1,2..10] // "The fifth element of the list is: 5"
// Start = showFifthElement [0,0] // "There is no fifth element in this list"
// Start = showFifthElement [33, 41, 56, 12, 96, 1] // "The fifth element of the list is: 96"

//-------------------------------------------------------------------------------

// 7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, and it should return false otherwise.

f7 :: {Int} -> Bool
f7 arr = foldr (&&) True [ occur x >=2 \\ x<-:arr]
    where occur x = (length [1\\el<-:arr| x==el])



//Start = f7 {1,2,3,1,3,2,2,2} // True
// Start = f7 {1,2,3,4,3,2,1} // False
// Start = f7 {1,1,1,3,3,4,3,2,4,2} // True

//-------------------------------------------------------------------------------
// 8.
// You are given a binary tree.
// Check if it is a binary search tree (BST).
// In BST values in left subtree should be 
// less then the current node's value and 
// values in right subtree should be greater.

// BST ADT
:: BST a = BSTNode a (BST a) (BST a) | BSTLeaf

// TODO
treeToList :: (BST a) -> [a]
treeToList BSTLeaf = []
treeToList (BSTNode x l r) = treeToList l ++ [x] ++ treeToList r 

isBST :: (BST Int) -> Bool
isBST tree = pos == sorted 
    where
        pos = treeToList tree
        sorted = sort pos

// For testing. DO NOT CHANGE!
bst1 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 3 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 12 (BSTNode 5 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst2 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 9 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
bst3 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 9 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 1 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst4 = (BSTNode 1 BSTLeaf (BSTNode 2 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))


// Start = map isBST [bst1,bst2,bst3,bst4,BSTLeaf] // [True,True,False,False,True]

//-------------------------------------------------------------------------------
// 9.
// Write a filter function for colored rose tree.
// Colored rose Tree is a tree where each node has 
// some value, color and children nodes stored in list.
// Your filter function should take tree, color, a two 
// condition function and filtering type as an argument. Return a list of
// values stored in nodes which have given color and
// satisfy both of the given conditions if filter type is 'AND'
// or satisfy at least one of the given functions if filter type
// is "OR" (Condition function returns
// true for node's value).

:: NodeColor = Red | Green | Blue

:: FilterType = AND | OR

:: ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf

instance == NodeColor
where
    (==) Red Red = True
    (==) Green Green = True
    (==) Blue Blue = True
    (==) _ _ = False

    
// TODO
roseToList :: (ColoredRoseTree a) -> [(a,NodeColor)]
roseToList Leaf = []
roseToList (Node num color list) = [(num, color)] ++ flatten (map roseToList list)

filterColoredTree :: (ColoredRoseTree a) NodeColor FilterType (a -> Bool) (a -> Bool) -> [a]
filterColoredTree tree color OR cond1 cond2 = [num\\(num,colour)<-(roseToList tree) | color == colour && ((cond1 num) || (cond2 num))]
filterColoredTree tree color AND cond1 cond2 = [num\\(num,colour)<-(roseToList tree) | color == colour && (cond1 num) && (cond2 num)]



tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]

// Start = filterColoredTree tree1 Blue OR isEven isOdd // [2,4,3]
// Start = filterColoredTree tree1 Blue AND isEven isOdd // []
// Start = filterColoredTree tree1 Blue AND isOdd isOdd // [3]
// Start = filterColoredTree tree2 Red OR (\x = True) isEven // [1,7,9,10]
// Start::[Int] // Uncomment this line too, to run next test
// Start = filterColoredTree Leaf Green OR isOdd isEven // []
//-------------------------------------------------------------------------------

/*
    10. 
    Implement the following methods of the Dictionary ADT.
	-keysNum
	-valueForKey
	-insert
	-remove
*/

// TODO .dcl and .icl for the Dictionary ADT

::Dictionary a b:==[(a,b)]
dict::Dictionary String Int
dict=[("first",23),("second",234234),("third",21231)]
dict2::Dictionary String Int
dict2=[("a",1)]


/* a) keysNum - Calcualte the number of keys in the dictionary */

//keysNum::(Dictionary String Int)->Int


// Start=keysNum dict//3
//Start=keysNum dict2//1


/*
	b) valueForKey - Gives back the value associated with a given key.
	If the key is not in the dictionary return "The key is not in the dictionary"
*/

//valueForKey::(Dictionary String Int) String- > Int

//Start=valueForKey dict "first"//23
//Start=valueForKey dict "firstt"//The key is not in the dictionary


/*	c) insert-Inserts a new tuple if the key value is not in the dictionary already,
	or give back "The given key already exists" if the key is already in the dictionary
*/

//insert::(Dictionary String Int) (String,Int)->(Dictionary String Int)

//Start  = insert dict ("third",12312)//"The given key already exists"
//Start = insert dict ("fourth",1)//[("first",23),("second",234234),("third",21231),("fourth",1)]

/*	d) remove-remove the (key, value) pair for a given key.
	If the key is not in the dictionary return "The key is not in the dictionary"
*/

//remove::(Dictionary String Int) String->(Dictionary String Int)

//Start=remove dict "first"//[("second",234234),("third",21231)]
//Start=remove dict "someOtherKey"//The key is not in the dictionary
//-------------------------------------------------------------------------------