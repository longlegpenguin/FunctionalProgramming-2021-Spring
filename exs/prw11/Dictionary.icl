implementation module Dictionary
import StdEnv

dict::Dictionary String Int
dict=[("first",23),("second",234234),("third",21231)]
dict2::Dictionary String Int
dict2=[("a",1)]

::Dictionary a b:==[(a,b)]

/* a) keysNum - Calcualte the number of keys in the dictionary */
keysNum::(Dictionary String Int)->Int
keysNum dic = length dic

// Start=keysNum dict//3
// Start=keysNum dict2//1

/*
	b) valueForKey - Gives back the value associated with a given key.
	If the key is not in the dictionary return "The key is not in the dictionary"
*/
valueForKey::(Dictionary String Int) String -> Int
valueForKey dic ke 
| length keys > 0 = hd keys
= abort "The key is not in the dictionary"
    where
        keys = [num \\ (key, num)<-dic | ke == key]

// Start=valueForKey dict "first"//23
// Start=valueForKey dict "firstt"//The key is not in the dictionary

/*	c) insert-Inserts a new tuple if the key value is not in the dictionary already,
	or give back "The given key already exists" if the key is already in the dictionary
*/

insert::(Dictionary String Int) (String,Int)->(Dictionary String Int)
insert dic new=:(newkey, newnum)
| length has > 0 = abort "The given key already exists"
= dic ++ [new]
    where
        has = [1 \\ (key, num) <- dic | key == newkey]

// Start  = insert dict ("third",12312)//"The given key already exists"
// Start = insert dict ("fourth",1)//[("first",23),("second",234234),("third",21231),("fourth",1)]

/*	d) remove-remove the (key, value) pair for a given key.
	If the key is not in the dictionary return "The key is not in the dictionary"
*/

remove::(Dictionary String Int) String->(Dictionary String Int)
remove dic badkey
| length has == 0 = abort "The key is not in the dictionary"
= [(goodkey, goodnum)\\(goodkey, goodnum)<-dic | goodkey <> badkey]
    where
        has = [1 \\ (key, num) <- dic | key == badkey]

// Start=remove dict "first"//[("second",234234),("third",21231)]
// Start=remove dict "someOtherKey"//The key is not in the dictionary