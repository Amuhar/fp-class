{- 2. Напишите третью реализацию очереди, основанную на контейнере Data.Sequence.
   Обновите соответствующим образом тесты.
 -}
 
module SeqQueue (Queue, AQ.empty ,AQ.isEmpty, AQ.enqueue,AQ.dequeue) where  

import AbstractQueue as AQ
import Data.Sequence as S

newtype Queue a = QueueImpl (Seq a) deriving (Show)
    

instance AbstractQueue Queue where
  empty = QueueImpl S.empty
  isEmpty (QueueImpl xs) = S.null xs
  enqueue (QueueImpl xs) x = QueueImpl ( (|>) xs x)
  dequeue (QueueImpl xs) = let (y :< ys) = viewl xs in (y, QueueImpl ys)
  