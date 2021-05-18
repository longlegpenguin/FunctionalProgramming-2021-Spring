module HW_7_r
import StdEnv

/* 1. Write a function that takes an array of integers and gives back a tuple which contains:
(the integer in the array, a boolean value)
the boolean value tells if when cut the integer in half it consists of
the same number, e,g, 2020 -> 20 20 so it keeps it but 2008 -> 20 08 it doesn't.
*/
intToList :: Int -> [Int]
intToList n 
| n < 10 = [n]
= intToList (n/10) ++ [n rem 10]

isCut :: Int -> Bool
isCut x = hdlist == tllist
    where 
        hdlist = take half intlist 
        tllist = drop half intlist
        intlist = intToList x 
        half = length intlist / 2

toTuple :: {Int} -> {(Int, Bool)}
toTuple arr = {(num, isCut num) \\ num<-:arr}

// Start = toTuple {} // {}
// Start = toTuple {100, 2020, 1919} // {(100,False),(2020,True),(1919,True)}
// Start = toTuple {312, 1001, 1010} // {(312,False),(1001,False),(1010,True)}
// Start = toTuple {312, 1001, 100100} // {(312,False),(1001,False),(1010,True)}

/* 2. Define a Person record which contains name and height two fields,
with type of String and Real respectively. Write a function which takes a person
and a certain height, if the person is taller than 1.70, subtract their height by
1%

*/
::Person1 = { name :: String, tall :: Real}
John::Person1
John={name = "John", tall= 1.78}
Mike::Person1
Mike={name = "Mike", tall= 1.58}
Lily::Person1
Lily={name = "Lily", tall= 1.85}

ChangeHeight :: Person1 -> Person1
ChangeHeight rec 
| rec.tall > 1.70 = {rec & tall=rec.tall-(rec.tall*0.01)}
= rec

// Start = ChangeHeight John // (Person1 "John" 1.7622)
// Start = ChangeHeight Mike // (Person1 "Mike" 1.58)
// Start = ChangeHeight Lily // (Person1 "Lily" 1.8315)



::Person={name::String, mass::Real, height::Real, bmi::Real}
Rose::Person
Rose={name="Rose", mass=147.71, height=1.72, bmi=0.0}
Jack::Person
Jack={name="Jack", mass=158.73, height=1.93, bmi=0.0}
Emilia::Person
Emilia={name="Emilia", mass=121.25, height=1.60, bmi=0.0}
Leo::Person
Leo={name="Leo", mass=85.98, height=1.75, bmi=0.0}
Grace::Person
Grace={name="Grace", mass=112.43, height=1.65, bmi=0.0}
Harry::Person
Harry={name="Harry", mass=169.76, height=1.80, bmi=0.0}

/* 3.
Given an array of Persons, write a function which calculates the BMI of each Person
BMI: body mass index = m / h^2
m = mass (in kilograms)
h = height (in meters)
note: the mass given in the records are in pounds, you need to convert before using the formula
hint: 1 pound = 0.453592kg
*/

calcBMI :: {Person} -> {Person}
calcBMI arrP = {cal person \\ person<-:arrP}

cal :: Person -> Person
cal pes = {pes&mass=m, height=100.0*h, bmi=bMi}
    where 
        bMi = m / (h * h)
        m = toReal(toInt (pes.mass * 0.453592))
        h = pes.height

// Start = calcBMI {Rose,Jack,Emilia} // {(Person "Rose" 67 172 22.6473769605192),(Person "Jack" 72 193 19.3293779698784),(Person "Emilia" 55 160 21.484375)}
// Start = calcBMI {Leo,Grace,Harry} // {(Person "Leo" 39 175 12.734693877551),(Person "Grace" 51 165 18.732782369146),(Person "Harry" 77 180 23.7654320987654)}
// Start = calcBMI {} // {}
