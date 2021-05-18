module HW1

import StdEnv

// Write a function that will take a digit (Int)
// and return the respective word for it (String).
// For example input of 1 should output One; input of 0 should output Zero; input of 5 should output Five.
// Anything that is not the digit (0-9) should output "Not a digit"

wordnumbers = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"]
digit_to_string :: Int -> String
digit_to_string n 
| n >= 0 && n < 10 = wordnumbers !! n
= abort "Not a digit"

//Start = digit_to_string 4 //"Four"
//Start = digit_to_string 0 //"Zero"
//Start = digit_to_string 5 //"Five"
//Start = digit_to_string 10 //"Not a digit"
//Start = digit_to_string -1 //"Not a digit"
//Start = digit_to_string 42 //"Not a digit"
//Start = digit_to_string 9 //"Nine"



// Write a function that takes Int and checks if this number is prime or not.
// handle the case of negative numbers (negative numbers are not primes).
// 0 and 1 are not prime numbers.

// Extra fuction "isprime_twoparameters"
// one extra parameter (smaller by 1) for counting down and as candinate factor, 
// no restritions of data input at this point yet.
isprime_twoparameters :: Int Int -> Bool 
isprime_twoparameters a 1 = True
isprime_twoparameters a b 
| a rem b == 0 = False
= isprime_twoparameters a (b-1) 

// Start = isprime_twoparameters 8 7 // False

is_prime :: Int -> Bool
is_prime x 
| x <= 1 = False
= isprime_twoparameters x (x-1)
// Start = is_prime 5 // True
// Start = is_prime 0 // False
// Start = is_prime 1 // False
// Start = is_prime 2 // True
// Start = is_prime 2017 // True


// Write a function that takes Int argument and checks if this number is a palindrome.
// Palindrome is a number that is the same when we read from left to right or from right to left.

// Extra function "length_of_integer" calculates the number of digits of an integer.
// This function does not handle special case integer 0.
length_of_integer :: Int -> Int
length_of_integer 0 = 0
length_of_integer l = 1 + length_of_integer (l / 10)
//Start = length_of_integer 12345 // 5
// Start = length_of_integer 1 // 1

// Extra function "turn_into_list" turns integer into a list of digits seperately. 
// Two parameters, the integer being convert and the length of the integer.
// This function does not handle case when the integer smaller than zero.
turn_into_list :: Int Int -> [Int]
turn_into_list a 0 = []
turn_into_list a l = [a / (10 ^ (l - 1))] ++ turn_into_list (a rem (10^(l-1))) (l - 1)
// Start = turn_into_list 1235 4 //[1,2,3,5]
// Start = turn_into_list 1205 4

is_palindrome :: Int -> Bool
is_palindrome x
| x < 0 = False  // all negative numbers are not palindome because of the - sign
| turn_into_list x (length_of_integer x) <> reverse (turn_into_list x (length_of_integer x)) = False
=True  // zero case and normal case

// Start = is_palindrome 0 // True
// Start = is_palindrome 55 // True
// Start = is_palindrome 49594 // True
// Start = is_palindrome 1337 // False          
// Start = is_palindrome -1331 // False
// Start = is_palindrome 1010101 // False          