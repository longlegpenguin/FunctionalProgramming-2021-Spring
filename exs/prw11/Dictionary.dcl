definition module Dictionary

::Dictionary a b


keysNum::(Dictionary String Int)->Int
valueForKey::(Dictionary String Int) String -> Int
insert::(Dictionary String Int) (String,Int)->(Dictionary String Int)
remove::(Dictionary String Int) String->(Dictionary String Int)
