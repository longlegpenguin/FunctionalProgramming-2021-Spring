module endterm
import StdEnv

/* 1.
	Create a class MyMultDiv which has the operations *~ , /~ and has the neutral element as
	myOne. 
	So given two elements apply multiplication and division:
	*~ -> multiplication
	/~ -> division
	After that create an instace for Char.
	Hint: Operations in Char can be done on their ineteger representation and then convert back to string
	Be careful when you multiply and divide to stay in the range of 255 (you can use modulo)
*/
class MyMultDiv a
	where
		(*~) :: a a -> a 
		(/~) :: a a -> a 
		myOne :: a

instance MyMultDiv Char
	where
		(*~) c1 c2 = toChar (((toInt c1) * (toInt c2)) rem 255 )
		(/~) c1 c2 = toChar ((toInt c1) / (toInt c2))
		myOne = toChar(1)
	
// Start = 'a' *~ 'b' //'G'
// Start = 'k' *~ myOne //'k'
// Start= 'z' /~ 'A' //''

::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={tname::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}
ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BME::University
BME={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}
Peter::Teacher
Peter={tname="Peter",subject="Functional"}
Viktor::Teacher
Viktor={tname="Viktor",subject="Math"}
Mary::Teacher
Mary={tname="Mary",subject="OOP"}
John::Teacher
John={tname="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}


/*2 Given a University, return an array of all the 
students or teachers names which are shorter than 6*/
isShortName6 :: String -> Bool
isShortName6 name = length nlist < 6
	where
		nlist = [x\\x<-:name]

shorterThan6::University->{String}
shorterThan6 uni = {x\\x<-nameslist}
	where 
		nameslist = slist ++ tlist
		tlist = [x.tname \\ x<- uni.teachers | isShortName6 x.tname]
		slist = [x.studentName \\ x<- uni.students | isShortName6 x.studentName]

// Start=shorterThan6 BME//{"Ana","Josh","Sofi","John","Peter"}
// Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"}
// Start=shorterThan6 EmptyUni//{}

/*3 Write a function which will take an array of Universities and return the University with the highest overall gpa (the average of the average of each student)*/
// ulist :: [University]
// ulist = [ELTE, BME, EmptyUni]

stuAvg :: Student -> Real
stuAvg stu = sum [toReal x \\ x <-glist] / (toReal(length glist))
	where
		glist = [x\\x<-:stu.grades]

uniAvg :: University -> Real
uniAvg uni 
| len == 0 = 0.0
= sum [stuAvg s \\ s<-slist] / (toReal len) 	
	where
		len = length slist
		slist = uni.students

highestGpa::{University}->String
highestGpa uarr 
| len == 0 = abort "No universities given"
= bestuni.uniName
	where
		bestuni = unilist!!ord
		unilist = [x\\x<-:uarr]
		(avggpa, ord) = last sortedtup
		sortedtup = sort gpatups
		gpatups = [(uniAvg (unilist!!x) , x) \\ x<-[0..len-1]]
		len = length unilist

// Start = uniAvg EmptyUni
// Start=highestGpa {ELTE,BME,EmptyUni}//"BMI"
// Start=highestGpa {ELTE,BME} //"BMI"
// Start=highestGpa {BME,ELTE} //"BMI"
// Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
// Start=highestGpa {ELTE} //"ELTE"
// Start=highestGpa {}//"No universities given"


/*4	Create a toString instance for Student such that for given student ex. Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},
favoriteTeacher=Peter} it gives "Nikola 3.6 Peter" where 3.6 is the student's gpa and Peter is the student's favorite teacher's name*/
instance toString Student
	where
		toString s = s.studentName +++ " " +++ gpa +++ " " +++ s.favoriteTeacher.tname
			where
				gpa = (toString (stuAvg s))

// Start=toString Nikola//"Nikola  3.6  Peter"
// Start=toString Marko//"Marko  4.25  Mary"
// Start=toString Nik//"Nik  3.8  Peter"
// Start=toString Dame//"Dame  3.5  Peter"


/* 5
A good person is the person who never lies, so let's test this quote, we have list of people,
each person has name
and list of the names of the people that he lies to, 
your task is to get the people who can say the truth to all the people in the given list 
list can be empty if all the people did lie.
Example : 
goodPeople 
[{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]},
{fake_name = "Lucifier", peopleToLie = ["Rafaat Ismail"]}
Output : [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]}] 
because Adel is not on the given list.
note : ofcourse we will consider fake names for this expirement so all the names here are fictional.
*/

::Person = {fake_name :: String, peopleToLie :: [String]}

isGoodOne :: String [Person]-> Bool
isGoodOne pname plist = len == 0
	where
		len = length aprlist
		aprlist = [x \\ x<-plist | x.fake_name == pname]

isGoodAll :: Person [Person] -> Bool
isGoodAll p plist = and [isGoodOne x plist \\ x <-p.peopleToLie]

goodPeople :: [Person] -> [Person]
goodPeople plist = [x \\ x<-plist | isGoodAll x plist]

// Start = goodPeople [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]},{fake_name = "Lucifier", peopleToLie = ["Adel","Rafaat Ismail"]},{fake_name = "elkenona", peopleToLie = ["Adel","Lucifier"]}] // [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]}]
// Start = goodPeople [{fake_name = "Alaa Abdelazim", peopleToLie = ["Brnadt","Shelby"]},{fake_name = "Bartaleaa", peopleToLie = ["Alaa Abdelazim","Shelby"]},{fake_name = "Shelby", peopleToLie = ["Bartaleaa"]}] // []

//6. Given two arrays,
// return new array such that i-th element of it is maximum of i-th element of first and second arrays.
// So for example, when we calculate 5th element of result array,
// we look at 5th element of first and 5th element of second arrays
// And choose maximum of the two.
// You can assume that arrays have same length. 
maxForList :: [Int] [Int] -> [Int]
maxForList [] [] = []
maxForList [x:xs] [y:ys] 
| x > y = [x: maxForList xs ys]
= [y: maxForList xs ys]


maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo arr1 arr2 = {x\\x<-goodlist}
	where 
		goodlist = maxForList l1 l2
		l1 = [x \\x<-:arr1]
		l2 = [x \\x<-:arr2]

// Start = maxOfTwo {} {} // {}
// Start = maxOfTwo {1} {5} // {5}
// Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
// Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}

//7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, 
// and it should return false if any element is distinct.
Appear2 :: Int [Int] -> Bool
Appear2 n list = length [x\\x<-list | n==x ] > 1

isTwice :: [Int] -> Bool
isTwice l = and [Appear2 x l \\ x<-l]

f7 :: {Int} -> Bool
f7 arr = isTwice list
	where
		list = [x\\x<-:arr]

// Start = f7 {1,2,3,1,3,2,2,2} // True
// Start = f7 {1,2,3,4,3,2,1} // False
// Start = f7 {1,1,1,3,3,4,3,2,4,2} // True




//8.Array is monotonic if it is either monotone increasing or monotone decreasing
// A is monotone increasing if for all i<=j, A[i]<=A[j]
// A is monotone decreasing if for all i<=j, A[i]>=A[j]
// Given array, your task is to decide if it is monotonic.

isMonotonic :: {Int} -> Bool
isMonotonic arr = list == sorted || list == reved 
	where
		list = [x\\x<-:arr]
		sorted = sort list
		reved = reverse sorted

// Start = isMonotonic {6,5,4,4} // True
// Start = isMonotonic {1,3,2} // False
// Start = isMonotonic {1,2,4,5} // True
// Start = isMonotonic {1,1,1} // True



:: Tree a = Node a (Tree a) (Tree a) | Leaf

instance == (Tree a) | == a
where
    == Leaf Leaf = True
    == (Node x1 l1 r1) (Node x2 l2 r2) = and[x1==x2, l1==l2, r1==r2]
    == _ _ = False

specialTree :: (Tree Int)
specialTree = Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf))(Node 5 Leaf (Node 6 Leaf Leaf)))(Node 15 (Node 12 (Node 11 Leaf Leaf)(Node 13 Leaf Leaf))(Node 17 (Node 16 Leaf Leaf)(Node 19 (Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))

notPrime :: Int -> Bool
notPrime x
| x <= 1 = True
= not(isEmpty[n\\n<-[2..(x-1)]|x rem n == 0])

/* 9
Please write a function that, given a Tree and a predicate,
will find nodes that do not return True for the predicate
and will remove those nodes and their subtrees.
Note: The expected return results are listed below with an equality
for your convenience, so that you do not have to manually check your result.
If your result is correct, the Start statement should return a True.
*/
pruneTree :: (Tree a) (a -> Bool) -> (Tree a)
pruneTree Leaf _ = Leaf
pruneTree (Node x l r) pre 
| pre x = (Node x (pruneTree l pre) (pruneTree r pre))
= Leaf

// Start = pruneTree specialTree isEven == (Node 10 (Node 4 Leaf Leaf) Leaf) //True
// Start = pruneTree specialTree ((<)7) == (Node 10 Leaf (Node 15 (Node 12 (Node 11 Leaf Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 16 Leaf Leaf) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf))))) //True
// Start = pruneTree specialTree notPrime == (Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf) Leaf) Leaf) (Node 15 (Node 12 Leaf Leaf) Leaf)) //True


/*10
Given a tree and an integer. Find all the nodes that equal to the integer and give the sum 
of their direct children.(Leaf count as 0).
*/
extractNode :: (Tree Int) -> Int
extractNode Leaf = 0
extractNode (Node x l r) = x 

f10::(Tree Int) Int->Int
f10 Leaf _ = 0
f10 (Node x l r) n
| x==n = extractNode l + extractNode r + f10 l n + f10 r n
= f10 l n + f10 r n

// Start = f10 (Node 2 Leaf Leaf) 3 //0
// Start = f10 (Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) 3 //2
// Start = f10 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf)) 1 //2
// Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))) 2 //7
// Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 Leaf (Node 1 Leaf Leaf))) 2 //4