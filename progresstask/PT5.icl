module PT5
import StdEnv

/* 
  Task:
  You are given lower and upper bounds and a list of tuples, where the first element is course code,
  the second one is course score and the third one is bool,
  which is true if the course is interesting. 
  Filter the list and return the list of tuple of course codes and scores, 
  which have score greater or equal to the lower bound and less or equal to the upper bound and are interesting.
 */
rangeFilter :: Int Int [(String,Int,Bool)] -> [(String,Int)]
rangeFilter lbd ubd tttlist = [(course, scores) \\ (course,scores,interesting) <-tttlist | interesting && lbd <= scores && scores <= ubd]

// Start = rangeFilter 10 15 [("A",12,True),("B",3,False),("C",5,True),("E",14,False),("F",16,False)] // [("A",12)]
// Start = rangeFilter 3 13 [("A",12,True),("B",3,True),("C",5,True),("E",14,True),("F",16,True)] // [("A",12),("B",3),("C",5)]
// Start = rangeFilter 5 7 [] // []
// Start = rangeFilter 15 3 [("A",12,True),("B",3,False),("C",5,True),("E",14,True),("F",16,True)] // []
// Start = rangeFilter 0 12 [("A",12,False),("B",3,False),("C",5,False),("E",14,False),("F",16,True)] // []