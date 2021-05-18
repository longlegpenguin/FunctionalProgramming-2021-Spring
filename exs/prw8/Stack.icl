implementation module Stack

import StdEnv

:: Stack a :==[a]

newStack :: Stack a
newStack = []

empty  :: (Stack a) -> Bool
empty  [] = True
empty  x  = False

push :: a (Stack a) -> Stack a
push e s = [e : s]

pushes :: [a] (Stack a) -> Stack a 
pushes [] s = s 
pushes [x:xs] s = pushes xs (push x s)

pop	:: (Stack a) -> Stack a
pop [e:s] = s

popn :: Int (Stack a) -> Stack a
popn n s = drop n s 

top	:: (Stack a) -> a
top [e:s] = e

topn     :: Int (Stack a) -> [a]
topn n s = take n s

elements :: (Stack a) -> [a]
elements x = x

count :: (Stack a) -> Int
count x = length x

//	You can use this Start-function to test your implementations:
Start				= ( "s0 = newStack = ",        s0,'\n'
					  , "s1 = push 1 s0 = ",       s1,'\n'
					  , "s3 = pop s1 = ",          s3,'\n'
					  , "s5 = top s1 = ",          s5,'\n'
					  , "test = empty s1 = ",     test, '\n'
					  , "count s1 = ",     n,'\n'
					  , "elements s1 = ",     s1,'\n'
				      , "ss1 = push 2 s1 = ",       ss1,'\n'
					//   , "ss2 = push 3 s1 = ",       ss2,'\n'
					//   , "topn 2 ss2 = ",            top2,'\n'
					  , "pushes [1,2,3] top2 = ",   pushes1,'\n'
					  , "popn 2 pushes1 = ",		popn1,'\n'
					  )
where
	s0				= newStack
	s1				= push   1      s0
	s3				= pop           s1
	s5				= top           s1
	test            = empty         s1
	n               = count s1
	ss1				= push   2      s1
	ss2				= push   3      ss1
	top2            = topn   2      ss2	
	pushes1			= pushes [1,2,3] s1
	popn1			= popn 	 1 		pushes1
