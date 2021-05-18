module PT6
import StdEnv

/* Given two lists A and B. 
Return the list which contains their superior set. 
We define a superior set as a list,
where i-th character of A is included if A_i >= B_i. Ex. [1,2,4,4] [1,4,3,5] -> [1,4] 
 */
superior :: [Int] [Int] -> [Int]
superior listA listB = [super \\ super<-listA & cond<-listB | super >= cond]

// Start = superior [1,2,4,4] [1,4,3,5] // [1,4]
// Start = superior [1..5] [1..5] // [1,2,3,4,5]
// Start = superior [1,1,2,3,4,6] [1,2,4,2,3,6] // [1,3,4,6]
// Start = superior [] [1..4] // []
// Start = superior [1..10] [2..7] // []