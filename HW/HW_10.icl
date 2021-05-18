module HW_10
import StdEnv

:: University = ELTE | BME | Corvinus
:: Student = {name::String, uni :: University, grades:: [Int]}
 

Rose::Student
Rose = {name="Rose",uni=ELTE, grades =[5,5,3,4,2,4,5,5]}
Peter::Student
Peter = {name="Peter",uni=BME, grades =[3,2,3,4,2,4,2,1,4,3,2,4]}
Noah::Student
Noah = {name="Noah",uni=Corvinus,grades=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]}
James::Student
James = {name="James",uni=ELTE,grades=[5,5,5,5,3,4,5,4,5]}
Lily::Student
Lily = {name="Lily",uni=BME,grades=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]}
Harry::Student
Harry = {name="Harry",uni=Corvinus,grades=[3,4,1,3,4,2,3,5,5]}
Eros::Student
Eros = {name="Eros",uni=Corvinus,grades=[4,2,4,4,4,4,4,5,2]}
Isabella::Student
Isabella = {name="Isabella",uni=BME,grades=[5,5,5,4,5,5,4,5,4,5]}
Oliver::Student
Oliver = {name="Oliver",uni=ELTE,grades=[2,3,3,4,3,2,1,3,2,3]}
 
/* 1.
Given array of students, find the University which has highest
average of student average GPA.
 
Example:
{Peter, Eros, Harry}
Peter's average GPA - 2.83
Eros's average GPA - 3.67
Harry's average GPA - 3.33
 
Hence:
ELTE's average grades - []
BME's average grades - [2.83]
Corvinus's average grades - [3.67, 3.33]
 
Corvinus has highes averafe - 3.5
*/
unilist :: [University]
unilist = [ELTE,BME,Corvinus]

instance == University
    where
        == ELTE ELTE = True
        == BME BME = True
        == Corvinus Corvinus = True
        == _ _ = False

stuAvg :: Student -> Real 
stuAvg stu = toReal(sum gra) / toReal(len)
    where 
        gra = stu.grades
        len = length gra

uniAvg :: {Student} University -> Real
uniAvg stuArr uni = sum list
    where
        list = [stuAvg stu \\ stu <-stul | stu.uni == uni]
        stul = [x\\x<-:stuArr]

uniWithHighestAverage :: {Student} -> University
uniWithHighestAverage stuArr = unilist!!un
    where
        (gpa,un) = last gpalist
        gpalist = sort gpatup
        gpatup = [(uniAvg stuArr (unilist!!uni), uni) \\ uni<-[0,1,2]]

// Start = uniWithHighestAverage {Rose,Harry,Isabella,Oliver,James,Noah,Lily,Peter,Eros} // ELTE
// Start = uniWithHighestAverage {Rose,Harry,Isabella} // BME
// Start = uniWithHighestAverage {Oliver, Noah,James,Lily} // ELTE
// Start = uniWithHighestAverage {Peter, Eros, Harry} // Corvinus


/* 2.
Define an instance of the built-in class ==
for Student. Students are equal if they have same
name and are from the same university.
*/
instance == Student
    where
        == s1 s2 = s1.name == s2.name

// Start = Rose == Harry // False
// Start = Harry == Harry // True
// Start = {name="John", uni=ELTE, grades=[]} == {name="John", uni=ELTE, grades=[1,2,3]} // True
 

/* 3.
You are given a binary tree.
Check if it is a binary search tree (BST).
In BST values in left subtree should be
less then the current node's value and
values in right subtree should be greater.
*/
:: BST a = BSTNode a (BST a) (BST a) | BSTLeaf

TreeToList :: (BST a) -> [a]
TreeToList BSTLeaf = []
TreeToList (BSTNode x l r) = TreeToList l ++ [x] ++ TreeToList r 

isBST :: (BST Int) -> Bool
isBST t = unsort == sorted
    where
        unsort = TreeToList t 
        sorted = sort unsort

bst1 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 3 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 12 (BSTNode 5 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst2 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 9 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
bst3 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 9 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 1 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst4 = (BSTNode 1 BSTLeaf (BSTNode 2 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
 

// Start = map isBST [bst1,bst2,bst3,bst4,BSTLeaf] // [True,True,False,False,True]
