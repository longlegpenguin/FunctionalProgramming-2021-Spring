module coverBoard
import StdEnv

// Given an area of N X M. 
// You have infinite number of tiles of size 2^i X 2^i, where i = 0, 1, 2,… so on. 
// The task is to find minimum number of tiles required to fill the given area with tiles.
cnt2s :: Int Int -> Int
cnt2s x cnt 
| x == x/2*2 = cnt2s (x/2) (cnt+1)
= cnt

sizeOfTiles :: Int Int -> Int
sizeOfTiles n m = min m2 n2
    where
        m2 = cnt2s m 0
        n2 = cnt2s n 0

NumOfTiles :: Int Int -> Int
NumOfTiles n m = n*m / (2^i * 2^i)
    where 
        i = sizeOfTiles n m 

// Start = NumOfTiles 4 4
