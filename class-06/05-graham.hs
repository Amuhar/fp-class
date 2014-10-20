{-
   Дописать к реализованному ранее алгоритму Грэхема основную программу, которая принимает
   на вход файл со списком точек и создаёт файл со списком точек, образующих выпуклую оболочку.

   Для отыскания пути к импортируемому модулю следует использовать параметр -i командной строки
   (для ghc и ghci), например:

     $ ghc 05-graham.hs  -o graham -i../class-05/3-Graham_scan/
-}

import GrahamScan
import System.Environment
import Data.List
import Data.Char
import System.IO



createList:: String -> [Point]
createList points =  foldl (\acc x -> (Point (read (head $ tail $ words x)::Double) (read (last $ words x)::Double ) ):acc) [] (lines points)

createString:: [Point] -> String
createString lsPoint = foldl (\acc x -> acc ++ show x ++ "\n" ) "" lsPoint

createFile:: FilePath -> IO()
createFile fname = do
  contents <- readFile fname
  let lsPoint =  createList contents
  let result = graham_scan lsPoint
  writeFile "graham.txt" $createString result
    


main = do
     [fname] <- getArgs
     createFile fname
 
