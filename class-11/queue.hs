{- 3. Пользуясь монадой State, реализовать функции для работы с очередью: enqueue и dequeue. -}

import Control.Monad.State
import System.Environment

type Queue = [Int]

enqueue::Int -> State Queue()
enqueue x = get >>= \xs -> put (xs ++[x])
	
dequeue::State Queue Int
dequeue = get >>= \(x:xs) -> put xs >> return x 

	
queueManip::State Queue Int
queueManip = enqueue 7 >>	enqueue 1 >> dequeue >> enqueue 1 >> dequeue
	
main = print $ runState  queueManip [1,2,3,4,5]
  

