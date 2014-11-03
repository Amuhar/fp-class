{-
1. Реализуйте тестирующий сценарий следующего вида:
  - формируется список случайных целых чисел;
  - с очередями всех имеющихся типов выполняются следующие операции:
    * в пустую очередь добавляется один элемент из списка случайных чисел;
    * в очередь добавляется два элемента, после чего извлекается один;
    * в очередь добавляется три элемента, после чего извлекается два;
    * в очередь добавляется четыре элемента, после чего извлекается три;
    * и т.д. до некоторого заданного в параметрах командной строки n (количество
      элементов, добавляемых в последний раз);
  - из всех очередей извлекаются оставшиеся элементы и сравниваются между собой 
    (их должно быть в точности n).

2. Напишите третью реализацию очереди, основанную на контейнере Data.Sequence.
   Обновите соответствующим образом тесты.
-}

import AbstractQueue
import qualified Queue as Q
import qualified FastQueue as FQ
import qualified SeqQueue as SQ
import System.Random
import System.IO
import System.Environment

genRandomIntList:: StdGen ->  Int -> [Int] -> IO [Int] 
genRandomIntList _ 0 ls = return ls 
genRandomIntList gen n ls = do 
                          let (rn, ng) = randomR (1,50) gen :: (Int, StdGen)
                          genRandomIntList ng (n - 1) (rn:ls) 
add ::(AbstractQueue q, Num a, Eq a) => q a -> Int -> [a] -> IO(q a)
add q 0 _ = return q                          
add q n ls = do
    gen <-newStdGen
    let (index, newGen) = randomR (1,length ls) gen :: (Int, StdGen)
    let element = (!!) ls index
    add (enqueue q element) (n-1) ls

del ::(AbstractQueue q, Num a, Eq a) => q a -> Int -> q a 
del q 0 = q 
del q m = del (snd $ dequeue q) (m-1)
          
addAndDelElem::(AbstractQueue q, Num a, Eq a) => q a -> Int -> Int ->  [a] ->IO(q a)
addAndDelElem q n m ls 
            | m > n = return q
            |otherwise = do
                q' <- add q m ls
                addAndDelElem (del q' (m-1)) n (m+1) ls

lengthQ:: (AbstractQueue q, Num a, Eq a) => q a -> Int -> Int
lengthQ q c 
            |isEmpty q = c
            |otherwise = lengthQ (snd $ dequeue q) $ c + 1    
            
checkQueue::(AbstractQueue q, Num a, Eq a) => q a -> Int -> [a] -> IO Int
checkQueue q n ls = do
            q' <- addAndDelElem q n 1 ls
            return $lengthQ q' 0

main = do
    [n] <- getArgs
    gen <-newStdGen
    let (len, newGen) = randomR (1,20) gen :: (Int, StdGen)
    ls <- (genRandomIntList newGen len [] )
    let n' = read n
    lenQ <- checkQueue (empty:: Q.Queue Int) n' ls
    lenFQ <- checkQueue (empty:: FQ.Queue Int) n' ls 
    lenSQ <- checkQueue (empty::SQ.Queue Int) n' ls
    print $ lenQ == n' && lenFQ == n' && lenSQ == n'
    
    