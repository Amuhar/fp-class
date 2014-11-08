import System.Environment
import Control.Applicative
import Data.List
import System.Random

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a 
    | a `mod` 3 == 0 = 0
    | a `mod` 3 /= 0 && odd a = a*a
    |otherwise = a*a*a

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n m = last <$> take (n+1) <$> iterate reduce <$> m

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = map (\x -> fst x)

-- возвращает первый уникальных элементов в списке 
toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe  ls =if null l then Nothing else Just (fst $ head l)
    where 
        l = filter ((== 1).snd )$ map (\xs -> (head xs, length xs)) . group . sort $toList ls

toEither :: Integral a => [(a, a)]  -> Either String a
toEither ls= if null l then Left "not found unique elements in a list" else Right (fst $ head l)
    where 
        l = filter ((== 1).snd )$ map (\xs -> (head xs, length xs)) . group . sort $toList ls

-- воспользуйтесь в этой функции случайными числами

toIO :: Integral a => [(a, a)]  -> IO a
toIO ls = do
    g <- newStdGen
    let  (index, g') = randomR (0,length ls -1 ) g::(Int,StdGen)
    let (f,s) = (!!) ls  index
    let ls' = if f > s then [s..f] else [f..s]
    let (index', g) = randomR (0,length ls'-1) g'::(Int,StdGen)
    return $ (!!) ls' index'

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs ls = (head ls, read $last ls)

readData :: FilePath -> IO [(Int, Int)]
readData fname = do
    content <- readFile fname
    return $ map (\s -> let [f',s'] = words s in (read f', read s')) $  lines content

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ toList ps
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
  
  2_1.txt  n = 3

    2 3
    3 5
    4 7
    8 9
    9 10
    2 7
    
    [2,3,4,8,9,2]
    [134217728,0,18014398509481984,0,0,134217728]
    Just 0
    Right 0
    390625
    
 2_2.txt   n = 5
    5 0
    3 2
    5 1
    3 4
    9 0
    9 1
  
    [5,3,5,3,9,9]
    [3273344365508751233,0,3273344365508751233,0,0,0]
    Nothing
    Left "not found unique elements in a list"
    1

  
  
  
  
-}
