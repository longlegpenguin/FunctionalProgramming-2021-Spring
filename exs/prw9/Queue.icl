implementation module Queue

import StdEnv 

:: Queue a :==[a]


newQueue ::     (Queue a)                 // empty queue
newQueue = []

empty    ::     (Queue a) -> Bool
empty [] = True
epmty _ = False

push     ::  a  (Queue a) -> Queue a       // push new element at the end of the queue
push x q = q ++ [x]

//pushes   :: [a] (Queue a) -> Queue a      // Consecutively push new elements to queue
pop      ::     (Queue a) -> Queue a       // Remove the top element from the queue
pop q = tl q

//popn     :: Int (Queue a) -> Queue a       // Remove the top n elements from the queue
top      ::     (Queue a) -> a             // Return the top element from the queue
top q = hd q

//topn     :: Int (Queue a) -> [a]           // Return the top n elements from the queue
//elements ::     (Queue a) -> [a]           // return all ements from the queue
count    ::     (Queue a) -> Int             // count the number of elements on the queue
count q = length q

//Start = newQueue
q1 = push 1 newQueue 
q2 = push 2 q1 
//Start = top q2
//Start = count q2
//Start = q2
Start = pop q2