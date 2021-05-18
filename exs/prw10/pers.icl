module pers
import StdEnv

//Given these Algebraic Data Types, Records, and Tree...
:: Gender = Male | Female | NonBinary | AttackHelicopter | Nghia | OOBLECK
:: LivingStatus = Alive | Deceased | Undead
:: MarriageStatus = Married | Divorced | Single | Tinder
:: Person = { name :: String, gender :: Gender, age :: Int, livingStatus :: LivingStatus, marriageStatus :: MarriageStatus}
:: FamilyTree a = Name a (FamilyTree a) (FamilyTree a) | End | Polygamy [[[[[[[[[[[[[FamilyTree a]]]]]]]]]]]]]

//And these people:
Olivia = {name = "Olivia", gender = Female, age = 19, livingStatus = Alive, marriageStatus = Single}
Amelia = {name = "Amelia", gender = Female, age = 83, livingStatus = Alive, marriageStatus = Married}
Isla = {name = "Isla", gender = Female, age = 40, livingStatus = Alive, marriageStatus = Married}
Emily = {name = "Emily", gender = Female, age = 73, livingStatus = Alive, marriageStatus = Divorced}
Ava = {name = "Ava", gender = Female, age = 18, livingStatus = Alive, marriageStatus = Single}
Lily = {name = "Lily", gender = Female, age = 50, livingStatus = Alive, marriageStatus = Divorced}
Oliver = {name = "Oliver", gender = Male, age = 56, livingStatus = Alive, marriageStatus = Married}
Harry = {name = "Harry", gender = Male, age = 45, livingStatus = Alive, marriageStatus = Married}
Jack = {name = "Jack", gender = Male, age = 90, livingStatus = Deceased, marriageStatus = Married}
George = {name = "George", gender = Male, age = 43, livingStatus = Alive, marriageStatus = Married}
Noah = {name = "Noah", gender = Male, age = 74, livingStatus = Undead, marriageStatus = Divorced}
Freddie = {name = "Freddie", gender = Male, age = 24, livingStatus = Alive, marriageStatus = Single}
Ethan = {name = "Ethan", gender = Male, age = 20, livingStatus = Alive, marriageStatus = Single}

//And each person's immediate parents:
OliviaTree = Name Olivia OliverTree HarryTree
OliverTree = Name Oliver End End
HarryTree = Name Harry AmeliaTree JackTree
AmeliaTree = Name Amelia End End
JackTree = Name Jack End End
EthanTree = Name Ethan GeorgeTree IslaTree
GeorgeTree = Name George AmeliaTree JackTree
IslaTree = Name Isla NoahTree EmilyTree
NoahTree = Name Noah End End
EmilyTree = Name Emily End End
AvaTree = Name Ava LilyTree OliverTree
LilyTree = Name Lily End End
FreddieTree = Name Freddie End End

personsList = [Olivia, Amelia, Isla, Emily, Ava, Lily, Oliver, Harry, Jack, George, Noah, Freddie, Ethan]
familyList = [OliviaTree, OliverTree, HarryTree, AmeliaTree, JackTree, EthanTree, GeorgeTree, IslaTree, NoahTree, EmilyTree, AvaTree, LilyTree, FreddieTree]

instance == Person
where
    (==) a b = (a.name == b.name) && (a.age == b.age)
   

goL :: (FamilyTree a) -> (FamilyTree a)
goL (Name x l r) = l
goR :: (FamilyTree a) -> (FamilyTree a)
goR (Name x l r) = r

// 给了一个头，把往下所有的可能的subtree放进列表
subTreeList :: (FamilyTree a) -> [(FamilyTree a)]
subTreeList End = []
subTreeList tree = subTreeList(goL tree) ++ [tree] ++ subTreeList(goR tree)
extractNode :: (FamilyTree a) -> a
extractNode (Name x l r) = x

//把root的Person record取出来
personlist :: [(FamilyTree a)] -> [a]
personlist x = [extractNode(a) \\ a <- x ]

// 遇到对的人，把他的树从list里拿出来。用hd去掉外面的[] 
persToTree:: Person ->(FamilyTree Person)
persToTree x = hd [tree \\ tree <- familyList | x == (extractNode tree)]

//Start = persToTree Olivia
//Start = persToTree Amelia

/*
Write a function that tests if two persons are cousins
Condition: They share a grandparent.
*/
// persToTree把这个人的家树拿出来
// subTreelist把所有可能的sub树列出来
// personlist把这些树的主人变成表
// 如果两个人不是同一个人且表里有共同亲属就是cousin
areCousins :: Person Person -> Bool
areCousins x y 
| x == y = False
= isAnyMember (personlist (subTreeList (persToTree x))) (personlist (subTreeList (persToTree y)))

//Start = areCousins Ethan Olivia //True
//Start = areCousins Ethan Ava //False
//Start = areCousins George Isla //False
//Start = areCousins Ethan Ethan //False (same person)

/*
Given an array of Int and a single Int, use array
comprehension to double each element of the array,
keeping only the multiples of the second Int argument.
*/
f1 :: {Int} Int -> {Int}
f1 arr n ={l \\ l <-: (f11 arr) | (l rem n == 0) }

f11 :: {Int} -> {Int}
f11 array = { x*2 \\ x <-: array}

//Start = f11 {1,2,3,4}
// Start = f1 {1,2,3,4} 4 //{4,8}
//Start = f1 {3,4,5,7,2,9} 3 //{6,18}
// Start :: {Char}
// Start = {x \\x <-: "hello"}