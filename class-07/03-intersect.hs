{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и вывести различные числа, встречающиеся
  в каждом из заданных текстовых файлов. Указание: в решении следует воспользоваться множествами.
-}

import System.Environment
import qualified Data.IntSet as Set
import Data.List
import Control.Monad

readNumFile :: (Num a,Read a) =>  FilePath -> IO [a]
readNumFile fname = do
      content <- readFile fname
      let xs = map read  $ concatMap words $ lines content
      return xs
     
solve :: [[Int]] -> (Int, [Int])
solve ls = (length set,set:: [Int])
    where set =  foldl1 (\acc x -> Set.toList $ Set.intersection (Set.fromList x) (Set.fromList acc) ) ls


main = getArgs  >>= mapM readNumFile >>= print.solve
