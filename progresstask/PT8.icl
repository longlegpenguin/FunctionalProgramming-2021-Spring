module PT8
import StdEnv

/* Create record `City`. It should contain 3 fields: name(String), area(Int)
and population(Int). Write `smallCityCount` function which takes list of Cities
and returns number of cities that have area less than 200 and population less than 300 000.
*/
:: City = {name :: String, area :: Int, population :: Int}

budapest={name="Budapest", area=525, population=1756000}
kutaisi={name="Kutaisi", area=67, population=147000}
debrecen={name="Debrecen", area=461, population=202000}
berlin={name="Berlin", area=891, population=3645000}
pisa={name="Pisa", area=185, population=90000}

smallCityCount :: [City] -> Int
smallCityCount citylist = length [x \\ x<-citylist | x.area < 200 && x.population < 300000]

// Start = smallCityCount [] // 0
// Start = smallCityCount [budapest,kutaisi,debrecen,berlin,pisa] // 2
// Start = smallCityCount [budapest,berlin] // 0
// Start = smallCityCount [kutaisi] // 1