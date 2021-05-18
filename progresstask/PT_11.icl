module PT_11
import StdEnv

:: University = ELTE | BME | Corvinus
:: Student = {name::String, uni :: University, grades:: [Int]}

Rose::Student
Rose = {name="Rose",uni=ELTE, grades =[5,5,3,4,2,4,5,5]}
Peter::Student
Peter = {name="Peter",uni=BME, grades =[3,2,3,4,2,4,2,1,4,3,2,4]}
Noah::Student
Noah = {name="Noah",uni=Corvinus,grades=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]}
James::Student
James = {name="James",uni=ELTE,grades=[5,5,5,5,3,4,5,4,5]}
Lily::Student
Lily = {name="Lily",uni=BME,grades=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]}
Harry::Student
Harry = {name="Harry",uni=Corvinus,grades=[3,4,1,3,4,2,3,5,5]}
Eros::Student
Eros = {name="Eros",uni=Corvinus,grades=[4,2,4,4,4,4,4,5,2]}
Isabella::Student
Isabella = {name="Isabella",uni=BME,grades=[5,5,5,4,5,5,4,5,4,5]}
Oliver::Student
Oliver = {name="Oliver",uni=ELTE,grades=[2,3,3,4,3,2,1,3,2,3]}


/*
1. Given an array of Student-s, return array of those student's name and average GPA
who have 'e' or 'E' in their name and GPA is at least 3.0.
*/
hasE :: String -> Bool
hasE name = length [x \\x<-charlist | x == 'e' || x == 'E'] > 0
    where
        charlist = [x \\ x<-:name]

stuGpa :: Student -> Real
stuGpa stu = avg reallist
    where
        reallist = [toReal x \\ x<-stu.grades]

selectStudent :: {Student} ->{(String, Real)}
selectStudent stuarr = {x \\x<-stulist}
    where
        stulist = [(stu.name, stuGpa stu) \\ stu<-:stuarr | hasE stu.name && (stuGpa stu) >= 3.0 ]

// Start = selectStudent {Rose,Harry,Isabella} // {("Rose",4.125),("Isabella",4.7)}
// Start = selectStudent {Oliver, Noah,James,Lily} //{("James",4.55555555555556)}
// Start = selectStudent {Peter,Rose,Eros} //{("Rose",4.125),("Eros",3.66666666666667)}
// Start = selectStudent {Rose,Harry,Isabella,Oliver,James,Noah,Lily,Peter,Eros} // {("Rose",4.125),("Isabella",4.7),("James",4.55555555555556),("Eros",3.66666666666667)}
// Start = selectStudent {Harry,Lily,Peter} // {}
