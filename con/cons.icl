module cons
import StdEnv

f1 :: [Int] Int -> Bool
f1 list n = isEven( length (filter ((==)n) list))

// f2 list = map f1 list

checkEven :: [Int] -> Bool
checkEven list = and (map (\x = f1 list x) list)

Start = checkEven [1,1,2,2,2,2,3,5,3,5]