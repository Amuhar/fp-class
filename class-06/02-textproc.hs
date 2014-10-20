{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Random
import System.Environment
import Data.List
import Data.Char
import System.IO
import System.Directory


countFile :: [String] -> IO ()
countFile args = do
    let fname = head args
    contents <- readFile fname
    print $ countLines contents

countLines:: String -> Int    
countLines contents = foldl (\acc x -> acc+1  )0 $ lines contents 

addLineEnd:: [String] -> IO ()
addLineEnd args = do
    let [str,fname] = args
    appendFile fname str

addLineBegin:: [String] -> IO ()
addLineBegin args = do
   let [str,fname] = args
   contents <- readFile fname
   writeFile "f5.txt" (str++"\n"++contents)
   renameFile "f5.txt" fname
 
    
    
upperLetterFile :: [String] -> IO ()
upperLetterFile  args=  do
    let fname = head args
    contents <- readFile fname
    writeFile "f3.txt" (upperLetter contents)
    renameFile "f3.txt" fname
    contents <- readFile fname
    print contents
    
    
upperLetter:: String -> String
upperLetter contents = (map (\x -> if isLetter x then toUpper x else x ) contents)


mergeFiles::[String] -> IO ()
mergeFiles args = do
    let [fname1,fname2,fname3] = args
    contents1 <- readFile fname1
    contents2 <- readFile fname2
    writeFile fname3 (merge contents1 contents2)
    
merge :: String -> String -> String
merge str1 str2 = concat$ zipWith (\x y -> x++" "++y++"\n") (lines str1) (lines str2)



genFile :: [String] -> IO ()
genFile args = do
    let fname = head args 
    g <- newStdGen
    let (numbLines,g') = randomR (1,50) g
    mapM_ (\x -> x) $ replicate numbLines $ do 
    g' <- newStdGen
    let (randNumber,newGen) = randomR (10,60) g' --генерируется число символов в строке
    appendFile fname  $( take randNumber $ randomRs ('a','z') newGen::[Char]) ++ "\n" 
                
functions:: Int -> [String] -> IO ()
functions numb args= 
    (!!) [countFile, addLineBegin, addLineEnd, upperLetterFile, mergeFiles, genFile] numb args
        

main = do 
    args <-getArgs
    let number = read $ head args 
    functions number $tail args
    

    
    

     
     
     
     
     
     
     
     
     