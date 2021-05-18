module endterm
import StdEnv


::Human={firstName::String, age::Int, height::Int}
Rose::Human
Rose={firstName="Rose",age=23,height=172}
Jack::Human
Jack={firstName="Jack",age=25,height=193}
Emilia::Human
Emilia={firstName="Emilia",age=15,height=160}
Leo::Human
Leo={firstName="Leo",age=16,height=175}
Grace::Human
Grace={firstName="Grace",age=35,height=165}
Harry::Human
Harry={firstName="Harry",age=42,height=180}
Emilia2::Human
Emilia2={firstName="Emilia",age=15,height=180}

/*
	1. Create an instance of `==` for the Human record. Two people are equal if all their
	attributes are the same
*/
instance == Human
	where
		== h1 h2 = h1.firstName == h2.firstName && h1.age == h2.age && h1.height == h2.height

// Start = Leo == Rose // False
// Start = Harry == Harry // True
// Start = Emilia == Emilia2 // False


/*
	2. Given a list of tuples [(Int,Int)]. Overload the * operator on  [(Int,Int)] such that:
	[(a,b),(c,d)]*[(a2,b2),(c2,d2),(e2,f2)] =
	[(a*a2,b*a2),(a*b2,b*b2),(c*c2,d*c2),(c*d2,d*d2)]
*/
tupMul :: (Int, Int) (Int, Int) -> [(Int, Int)]
tupMul (a, b) (a2, b2) = [(a*a2,b*a2),(a*b2,b*b2)]

instance * [(Int,Int)]
	where
		(*) tuplist1 tuplist2 = flatten [tupMul x y \\ x<-tuplist1 & y<-tuplist2]

// Start = [(1,2)]*[] //[]
// Start = [(1,2),(3,4),(5,6)]*[(3,5)]//[(3,6),(5,10)]
// Start = [(1,2),(3,4),(5,6)]*[(3,5),(6,2),(4,5),(9,7)]//[(3,6),(5,10),(18,24),(6,8),(20,24),(25,30)]
// Start = [(1,2),(2,1),(3,2)]*[(2,2),(0,0)]//[(2,4),(2,4),(0,0),(0,0)]


/*
	3. Given an array of lists of Ints and an Int,
	 keep the lists whose difference between max and min element is less than the given number
	There are no [] in the array
*/
minMaxDiff :: {[Int]} Int -> {[Int]}
minMaxDiff larr n = {l \\ l<-:larr | (last (sort l) - hd (sort l)) < n}

// Start = minMaxDiff {[1,21,2],[1,1,1,1,1],[1]} 5//{[1,1,1,1,1],[1]}
// Start = minMaxDiff {[1,21],[1..10],[4,3]} 5//{[4,3]}
// Start = minMaxDiff {[1..10],[5..345]} -3//{}


/*
	4. Given array find minimum of it and return new array which has all occurrences of minimum removed.
	For example, if given array is {1,4,5,3,3,2,4,5,1,3,4}, minimum is 1, 
	so answer should be {4,5,3,3,2,4,5,3,4}.
*/

rem_min :: {Int} -> {Int}
rem_min arr = {x \\ x<-:arr | x <> min}
	where
		min = hd (sort list)
		list = [x \\ x<-:arr]

// Start = rem_min {1,4,5,3,3,2,4,5,1,3,4} // {4,5,3,3,2,4,5,3,4}
// Start = rem_min {1,42,42,52,452,4} // {42,42,52,452,4}
// Start = rem_min {5} // {}
// Start = rem_min {} // {}


/*
	5. Given two Strings as parameters, remove all characters of second string from the first one.
*/
remove_from_first_string :: String String -> String
remove_from_first_string s1 s2 = {x \\ x<-:s1 | not (isMember (toUpper x) l2)}
	where
		l2 = [toUpper x \\ x<-:s2]

// Start = remove_from_first_string "Zuka" "z"// "Zuka"
// Start = remove_from_first_string "XccEcxacXmXs aXcrccXe hXaXccXbrXd" "Xbc"// "Exams are hard"
// Start = remove_from_first_string "Clean is the best" " "// "Cleanisthebest"
// Start = remove_from_first_string "It's a nice weather outside" ""// "It's a nice weather outside"
// Start = remove_from_first_string "" ""// ""


/*
	6. Given a predefined Shape type, argument of the Circle constructor is the radius,
    side length for Square, and equilateral Triangle, width and height for Rectangle,
    write a function that calculates the area and circumference
    of each shape in the array, store the results of each shape as a tuple in an array.
*/
pi :: Real
pi = 3.14

:: Shape = Circle Real
        | Square Real
        | Triangle Real
        | Rectangle Real Real

shapeCal :: Shape -> (Real, Real)
shapeCal (Circle radius) = (pi * radius*radius, 2.0*pi*radius)
shapeCal (Square w) = (w*w, 4.0*w)
shapeCal (Triangle s) = (s*(sqrt 3.0 * (s/2.0) /2.0), 3.0*s)
shapeCal (Rectangle a b) = (a*b, a+a+b+b)

calc :: {Shape} -> {(Real,Real)}
calc arr = {shapeCal x \\ x<-: arr}

// Start = calc {(Circle 3.0), (Square 2.5)} // {(28.26,18.84),(6.25,10)}
// Start = calc {(Triangle 4.3), (Rectangle 5.4 7.2), (Circle 2.45)} // {(8.00640485798713,12.9),(38.88,25.2),(18.84785,15.386)}
// Start = calc {(Triangle 7.6), (Circle 1.75), (Square 0.95)} // {(25.0108136612946,22.8),(9.61625,10.99),(0.9025,3.8)}


::Moral = Happy | Sad

::Player = {player_name :: String, goals :: [Int], moral :: Moral}

/*
	7. A coach was choosing the starting team for his important game, unfortunately he/she has an issue
	choosing the starting striker, let's help to choose, the best striker is a striker who has the
	best average in scorig goals. There is another factor which the moral if the player is happy, this
	adds 0.1 to his average. And of course we should choose the striker with the best average. (just return the name)
	NOTE: If there are 2 players with the same average (after considiring the moral of course) return any of them.
*/
instance == Moral
	where
		== Happy Happy = True
		== Sad Sad = True
		== _ _ = False

calGoal :: Player -> Real
calGoal p
| p.moral == Happy = avg [toReal x \\ x<-p.goals] + 0.1
= avg [toReal x \\ x<-p.goals]

bestStriker :: [Player] -> String
bestStriker pl = name
	where
		(point, name) = last sorted
		sorted = sort tuplist
		tuplist = [(calGoal p, p.player_name) \\ p <-pl]

// Start = bestStriker [{player_name = "Abo Trika", goals = [1,1,0,0,2], moral = Happy}, {player_name = "Ronaldo", goals = [1,1,2,0,2], moral = Sad}, {player_name = "Messi", goals = [1,1,2,1,2,2,0], moral = Happy} ] // Messi
// Start = bestStriker [{player_name = "Abo Trika", goals = [2,2], moral = Sad}, {player_name = "Ronaldo", goals = [2,2], moral = Happy}, {player_name = "Messi", goals = [1,1,2,1,2,2,0], moral = Sad} ] //Ronaldo



/*
	8. In a boxing match, there are two fighters and we need to run a simulations to determine who will win
	(return the winner's name and the number of round he won at in a tuple).
	Each fighter has: health points, attacking points and defense points.
	The match is 10 rounds,
	in each round one of the two fighters can punch the other fighter and the round ends,
	and in the next round the other fighter will be the one who throw the punch and so on.
	The punch should subtract x points from the health points of the fighter (who gets hit of course), 
	x = attacking points - defending points.
	In case fighter's health points reaches 0, he/she loses.
	NOTE: The first fighter (first argument in the function) always starts.
	NOTE: If 10 rounds passed without any winner reutrns ("No winner", 22)
	NOTE: If the defending points is bigger than the attacking points of the other fighter the health points stay the same
	Example:
	Fighter A has health points: 100, attacking points: 50, defending points: 30.
	Fighter B has health points: 40, attacking points: 20, defending points: 10.
	when A hits B, B's health points will be equal to 40 - (50 - 10) = 0.
*/

:: Fighter = {fighter_name :: String, health_points :: Int, attacking_points :: Int, defending_points :: Int}

changePoints :: Fighter Fighter -> Fighter
changePoints attacker defender 
| defender.defending_points > attacker.attacking_points = defender
= {defender& health_points = defender.health_points - (attacker.attacking_points - defender.defending_points)}

getAux :: Fighter Fighter Int -> (String, Int)
getAux _ _ 10 = ("No winner", 22)
getAux f1 f2 cnt 
| f1.health_points <= 0 = (f2.fighter_name, cnt)
| f2.health_points <= 0 = (f1.fighter_name, cnt)
| isEven cnt = getAux f1 (changePoints f1 f2) (cnt+1)
= getAux (changePoints f2 f1) f2 (cnt+1)

getWinnerRound :: Fighter Fighter -> (String, Int)
getWinnerRound f1 f2 = getAux f1 f2 0

// Start = getWinnerRound {fighter_name = "Ali", health_points = 100, attacking_points = 30, defending_points = 40} {fighter_name = "Tayson", health_points = 90, attacking_points = 50, defending_points = 20} // ("No winner", 22)
// Start = getWinnerRound {fighter_name = "Ali", health_points = 100, attacking_points = 70, defending_points = 40} {fighter_name = "Tayson", health_points = 100, attacking_points = 50, defending_points = 20} // ("Ali",3)
// Start = getWinnerRound {fighter_name = "Ali", health_points = 50, attacking_points = 70, defending_points = 40} {fighter_name = "Tayson", health_points = 150, attacking_points = 80, defending_points = 20} // ("Tayson",4)



/*
	9. Write a filter function for colored rose tree.
	Colored rose Tree is a tree where each node has
	some value, color and children nodes stored in list.
	Your filter function should take tree, color and a
	condition function as an argument. Return a list of
	values stored in nodes which have given color and
	satisfy given condition (Condition function returns
	true for node's value)
*/


::NodeColor = Red | Green | Blue
::ColoredRoseTree a = Node a NodeColor [ColoredRoseTree a] | Leaf


tree1 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Leaf])]
tree2 = Node 1 Red [(Node 2 Blue [Node 4 Blue []]), Leaf, Leaf, (Node 3 Blue [Leaf,Node 7 Red [Node 9 Red [], Node 10 Red []]])]

instance == NodeColor
	where
		== Red Red = True
		== Green Green = True
		== Blue Blue = True
		== _ _ = False
/* 
Alternative way: 

roseToList :: (ColoredRoseTree a) -> [(NodeColor, a)]
roseToList Leaf = []
roseToList (Node n color list) = [(color,n)] ++ flatten (map roseToList list)

filterColoredTree :: (ColoredRoseTree a) NodeColor (a->Bool) -> [a]
filterColoredTree rose col func = [x \\ (color,x) <- (roseToList rose) | col == color && func x]
*/
filterColoredTree :: (ColoredRoseTree a) NodeColor (a->Bool) -> [a]
filterColoredTree Leaf _ _ = []
filterColoredTree (Node n color list) col func
| col == color && func n = [n] ++ flatten [filterColoredTree x col func\\ x<- list]
= flatten [filterColoredTree x col func\\ x<- list]

// Start = filterColoredTree tree1 Blue isEven // [2,4]
// Start = filterColoredTree tree1 Blue isOdd // [3]
// Start = filterColoredTree tree2 Red (\x = True) // [1.7,9,10]
// Start::[Int] // Uncomment this line too, to run next test
// Start = filterColoredTree Leaf Green isOdd // []



/*
	10. You are given a family tree. Each Node stores
	Person and his/her parent's family trees. If
	the data about the person is not known than the
	node is 'Unknown'. Your task is to count how many
	person are related to ELTE. A Person is related to
	ELTE if his/her father or mother studied at ELTE.
*/

::Gender = Male | Female
::Uni = ELTE | BME | MIT | TUM | LMU

::Person = { name :: String
           , gender :: Gender
           , almaMater :: Uni
           }

::FamilyTree = Member Person FamilyTree FamilyTree | Unknown

p1 = {name="A",gender=Male,almaMater=ELTE}
p2 = {name="B",gender=Female,almaMater=TUM}
p3 = {name="C",gender=Male,almaMater=BME}
p4 = {name="D",gender=Female,almaMater=ELTE}
ftree1 = Member p2 (Member p1 (Member p3 Unknown Unknown) Unknown) (Member p4 (Member p2 Unknown Unknown) (Member p4 Unknown Unknown))
ftree2 = Member p3 ftree1 (Member p3 (Member p1 Unknown ftree1) (Member p4 ftree1 ftree1))
ftree3 = Member p1 (Member p3 ftree2 (Member p4 ftree1 ftree2)) (Member p3 Unknown (Member p4 (Member p3 ftree2 Unknown) ftree2))

instance == Uni
	where
		== ELTE ELTE = True
		== _ _ = False

extractNode :: FamilyTree -> Uni
extractNode (Member x _ _) = x.almaMater

isRelated :: FamilyTree -> Bool
isRelated (Member _ Unknown Unknown) = False
isRelated (Member _ l Unknown) = extractNode l == ELTE
isRelated (Member _ Unknown r) = extractNode r == ELTE
isRelated (Member _ l r) = extractNode r == ELTE || extractNode l == ELTE

countRelated :: FamilyTree -> Int
countRelated Unknown = 0
countRelated (Member x l r)
| isRelated (Member x l r) = 1 + countRelated l + countRelated r 
= countRelated l + countRelated r 

// Start = countRelated ftree1 // 2
// Start = countRelated ftree2 // 9 
// Start = countRelated Unknown // 0
// Start = countRelated ftree3 // 40
