import Control.Monad 
import Data.Either
import System.Environment
{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться 
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int

type Pole = (Birds, Birds)


balance = 3

parseInfo:: [String] -> Pole -> Either String Pole
parseInfo act = case head act of
                "L" -> landLeft (read . head . tail $  act) 
                "R" -> landRight (read . head . tail $ act) 
                "B" -> banana 
                "Both" -> landBoth (read. head . tail $act) (read . last $ act)
                "U" -> unlandAll 
                
                
readInf::FilePath -> IO [Pole -> Either String Pole]
readInf fname = readFile fname >>= (return . map  (parseInfo. words) . lines)


result act = foldr (<=<) return act (0,0)


updatePole :: Pole -> Either String Pole
updatePole (l,r) 
    |(l - r) > balance = Left "unbalanced! Left side"
    |(r - l) > balance = Left "unbalanced! Right side"
    |otherwise = Right (l,r)

unlandAll:: Pole -> Either String Pole  
unlandAll _ = Right (0,0)
    
    
landBoth:: Birds -> Birds -> Pole -> Either String Pole
landBoth nl nr (left,right) = updatePole (left + nl, right + nr)
    
    
landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right) = updatePole (left, right + n)

banana :: Pole -> Either String Pole
banana _ = Left "banana on the rope!!"

tests args  = all test [1..8]
  where
    test 1 = (return (0, 0) >>= landLeft 1 >>= landRight 4 
              >>= landLeft (-1) >>= landRight (-2)) == Left "unbalanced! Right side"
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1) == Left "banana on the rope!!"
    test 4 = (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2 >>= landBoth 1 1 >>= landBoth 2 4 ) == Left "unbalanced! Right side"
    test 5 = result args ==  Left "banana on the rope!!"
    test 6 = (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2 >>= landBoth 1 1 >>= unlandAll >>= landLeft 2) ==Right (2,0)
    test 7 = (return (0,0) >>= banana >>= landBoth 2 3 >>= unlandAll) == Left "banana on the rope!!"
    test 8 = (return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2 >>= landBoth 1 1 >>= landBoth 2 4 >>= unlandAll >>= landLeft 1 ) ==Left "unbalanced! Right side"
    test 9 = (return (0,0) >>= landLeft 4 >>= unlandAll ) == Left "unbalanced! Right side"
    test 10 = (return (0,0) >>= landBoth 2 3 >>= landLeft (-1) >>= landBoth 1 2 ) == Right (2,5)
    
main = (head `liftM` getArgs) >>= readInf >>= return . tests

{-
f.txt

    R 2
    L 3
    R -1
    B
    L 1

-}