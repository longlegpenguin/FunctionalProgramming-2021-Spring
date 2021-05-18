module test
import StdEnv



// Start = span (\x = x<>' ') l 
// Start :: String
// Start = {y\\y<-l}
//     where l = [x\\x<-:"hello word"]
// Start = 
//     where   
//         s = span (\x = x<>' ') l
//         l = [x\\x<-:"hello word"]