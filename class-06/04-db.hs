{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}
import System.Environment
import Data.List
import System.IO

data Student = Student {surname :: String, firstname :: String,patronymic :: String, age :: Double, course :: Int , group' :: Int} deriving (Show,Eq,Ord)


dataBase:: String -> [Student]    
dataBase contents =  foldl (\acc x ->   createStudent x: acc) []  (lines contents)


createStudent:: String -> Student
createStudent inf = Student {surname = s, firstname = f, patronymic = p, age = read age :: Double, course = read course::Int,group'= read group'::Int}
    where [sfp,age, course,group'] = filter (\x -> x /= ";" ) $ groupBy (\x y -> x /= ';' && y /= ';') inf 
          [s,f,p] = words sfp 

countAge:: [Student] -> Int -> Int ->Double
countAge inf gr course' = (/) (foldl (\acc x -> (age x) + acc) 0 infGC) len
    where infGC = filter (\x -> course x == course' && group' x == gr) inf 
          len = fromIntegral $length infGC

f1::[String] -> IO()        
f1 args = do
    let [fname,course,group'] = args
    contents <- readFile fname
    let inf = dataBase contents
    print $ countAge inf (read group'::Int) (read course::Int)

countStudentsPrint:: [Student] -> IO()
countStudentsPrint inf  = do 
         let courseGroup = reverse $listCourseGroup inf
         let count = reverse $ foldl (\acc (c,g) -> (foldl (\ac x -> if (course x, group' x)== (c,g) then ac+1 else ac) 0 inf ): acc)  [] courseGroup
         mapM_ putStrLn $ zipWith (\(c,g) y -> "course " ++ (show c) ++ " group " ++ (show g) ++ " number of students is " ++ (show y) ) courseGroup count

listCourseGroup :: [Student] -> [(Int,Int)]
listCourseGroup inf =   foldl (\acc x -> if elem (course x, group' x) acc then acc else (course x, group' x): acc ) [] inf
    

f2::[String] -> IO() 
f2 args = do
  let fname = head args
  contents <- readFile fname
  let inf = reverse $ dataBase contents
  countStudentsPrint inf

f3:: [String] -> IO()  
f3 args = do
    let fname = head args
    contents <- readFile fname
    let db = reverse $ dataBase contents 
    mapM_ (\x -> appendFile ((show $ course x) ++ "_" ++(show $ group' x)++".txt" ) (surname x ++ " "++ firstname x++" "++ patronymic x ++ "\n")  ) db    


functions:: Int -> [String] -> IO ()
functions numb args= 
   (!!) [f1,f2,f3] numb args
    

main = do
    args <- getArgs
    let number = read $ head args 
    functions number $tail args


