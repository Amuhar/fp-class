{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}


import System.Environment
import Data.List
import Data.Char
import System.IO
import System.Random

data Point = Point Double Double
    deriving(Show,Ord,Eq)

genFile :: [String] -> IO ()
genFile args = do
    let fname = head args 
    g <- newStdGen
    let (numbLines,g') = randomR (1,50) g
    mapM_ (\x -> x) $ replicate numbLines $ do 
    g' <- newStdGen
    let (x,newGenX) = randomR (-100,100) g'::(Double,StdGen)
    let (y,newGenY) = randomR (-100,100) newGenX::(Double,StdGen)
    appendFile fname  $show (Point x y ) ++ "\n"
    
numbPointQCoordSys args = do
    let fname = head args 
    contents <- readFile fname
    printAnswer $ qCoordSys contents

printAnswer:: [Int] -> IO()
printAnswer listQ = do mapM_ putStrLn $ zipWith (\x y -> "In " ++ x ++ " quadrant " ++ show y ++ " points") ["I","II","II","IV"] listQ 
       
qCoordSys:: String -> [Int]  
qCoordSys contents  =   foldl (\acc x ->  qCoordSysPoint (tail $ words x) acc ) [0,0,0,0]  (lines contents)

qCoordSysPoint:: [String] -> [Int] -> [Int]
qCoordSysPoint point acc 
    |(x>0) && (y>0) = [q1+1,q2,q3,q4]
    |(x<0) && (y>0) = [q1,q2+1,q3,q4]
    |(x<0) && (y<0) = [q1,q2,q3+1,q4]
    |otherwise =  [q1,q2,q3,q4+1]
    where (x,y) = ( read  (head point) ::Double, read (last point)::Double)
          [q1,q2,q3,q4] = acc
          
pointWithMaxDistPrint:: [String] -> IO()          
pointWithMaxDistPrint args = do 
  let fname = head args 
  contents <- readFile fname
  printAnswerDist $ pointWithMaxDist contents
    
printAnswerDist::(Double,String)  -> IO()
printAnswerDist (dist,point) = do putStrLn $ point ++ " with distance " ++ (show dist)                                                 
          
pointWithMaxDist:: String -> (Double,String)        
pointWithMaxDist contents = maximum $ zip (reverse $ foldl (\acc x ->  (distance (tail $ words x) ): acc ) [] ls) $ ls
    where ls = lines contents
    
          
distance::[String] -> Double          
distance point = sqrt $ x^2+y^2
    where (x,y) = ( read  (head point) ::Double, read (last point)::Double)

  
    
functions:: Int -> [String] -> IO ()
functions numb args= 
    (!!) [genFile,numbPointQCoordSys,pointWithMaxDistPrint] numb args
    

main = do
    args <- getArgs
    let number = read $ head args 
    functions number $tail args
    
