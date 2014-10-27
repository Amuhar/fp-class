{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:
   1) чтение матрицы из тестового файла;
   2) запись матрицы в текстовый файл;
   3) сумма матриц;
   4) произведение матриц.

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}


import System.Environment
import Data.List
import Control.Monad
import Data.Char
import Data.Array.IArray
import qualified Data.Map as Map


load::  FilePath ->IO (Array (Int,Int) Int)
load fname = do
    content <- readFile fname
    let matrix = foldr(\x acc -> (foldr (\y ac -> (read y ::Int): ac ) [] $ words x):acc ) [] $ lines content
    let request= (listArray ((1,1), (length matrix, length $ head matrix))  $ concat matrix :: Array (Int,Int) Int )
    return request

save request map'  = do
    let [v,_,fname] = tail request
    let map = Map.fromList map'
    let Just arr = (Map.lookup v map )
    let content = foldl (\acc x -> acc ++ (foldl (\ac y -> ac ++ show(arr ! (x,y))++" " ) "" [1..snd$ snd $bounds arr ] )  ++ "\n") ""  [1..fst $ snd $ bounds arr]
    writeFile fname content
 
plus :: Array (Int,Int) Int -> Array (Int,Int) Int -> Array (Int,Int) Int 
plus a b =  array resultBounds [ ((r, c), a!(r, c) + b!(r, c)) |r <- range(lr,ur), c <- range(lc,uc) ]
    where
       ba@((lr,lc),(ur,uc)) = bounds a
       resultBounds
        | ba == bounds b = ba
        | otherwise = error "plus: incompatible bounds"


mult :: Array (Int,Int) Int -> Array (Int,Int) Int -> Array (Int,Int) Int 
mult a b =  array resultBounds [ ((r, c), sum [ a!(r,i)*b!(i,c)|i <- [1..uc]]) |r <- range(lr,ur), c <- range(lc',uc') ]
    where
       ba@((lr,lc),(ur,uc)) = bounds a
       bb@((lr',lc'),(ur',uc')) = bounds b
       resultBounds
        | uc == ur' = ((1,1),(ur,uc'))
        | otherwise = error "mult: incorrect bounds"
        
 
task map request = case head request of
    "LOAD" -> do 
              let [v,_,fname] = tail request
              arr <- (load fname )
              return $(v,arr):map
    "CALC" -> do
            let [v,_,op,a,b] = tail request
            let map' = Map.fromList map
            let Just a' = (Map.lookup a map' ) 
            let Just b' = (Map.lookup b map' ) 
            case op of
             "PLUS" -> do
                       let d = plus a' b'
                       return $(v,d):map
             "MULT" -> do
                       let e = mult a' b'
                       return $ (v,e):map
             
             
               
  
         
readTask [] map requests=  return  (map,requests)                 
readTask content map  requests = do
    if ((head $ words $head content) == "SAVE") 
        then  readTask (tail content) map ((words $head content):requests)
        else do
              map' <- task map (words$ head content)
              readTask (tail content) map' requests           


main = do 
    [fname]<- getArgs
    content <- readFile fname
    (map , requests) <- (readTask (lines content) [] [])
    mapM_ (\x -> save x map) requests
    
    