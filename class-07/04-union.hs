{-
  В параметрах командной строки указаны имена текстовых файлов, содержащих целые числа, разделённые
  пробелами и символами перевода строк. Определить количество и сумму различных чисел, встречающихся
  в заданных текстовых файлах.
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


solve :: [[Int]] -> (Int, Int)
solve ls = (length set,sum set)
    where set = (Set.toList . Set.unions $ foldl (\acc x -> (Set.fromList x ):acc) [] ls)::[Int]


main = getArgs >>= mapM readNumFile >>= print.solve
