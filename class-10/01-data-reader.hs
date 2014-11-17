{-
   Дан текстовый файл, содержащий данные о нескольких студентах в следующем формате: три последовательные
   строки соответствуют имени, возрасту и номеру группы (например, 4.8). Определить соответствующий тип
   данных (data) и организовать чтение файла в список значений этого типа.

   Для двух данных файлов объединить загруженные списки в один список, упорядоченный по имени и сохранить
   результат в новый файл того же формата. Указание: всюду следует использовать монадический синтаксис
   (операции >>= и >>, функции ap, liftM и другие функции монадической обработки данных, использование
   блока do не допускается).
-}
import Control.Applicative
import System.Environment
import Control.Monad
import Data.List

type Name = String
type Age = Int
type Group = Double
data Student = Student Name Age Group deriving (Show,Ord,Eq)


createStudentList :: [String] -> [Student]
createStudentList [] = []
createStudentList ls =  (let [sname , age, group ] = take 3 ls in (Student sname (read age) (read group)) )  : createStudentList (drop 3 ls )

readInf :: FilePath -> IO [Student]
readInf fname =  readFile fname >>= return . createStudentList . lines   


unionLists::IO[Student] -> IO[Student] -> IO[Student] 				
unionLists s1 s2 =   (++) `liftM` s1 `ap` s2 >>= (return . sort)                                   
                 

main =  getArgs >>= \[n,m] -> unionLists (readInf n) (readInf m)

  
