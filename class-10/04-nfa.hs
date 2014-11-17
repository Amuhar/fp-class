{-
  Следующие типы задают множество состояний, алфавит и функцию переходов
  недетерминированного конечного автомата (НКА).
-}

import Control.Monad
import Data.List

type Alphabet = [Char]
type State = Integer
type States = [State]
type AcceptingStates = [State]
type InitialState = State
type TransitionFunction = State -> Char -> States
type NFA = (Alphabet, States, InitialState, TransitionFunction, AcceptingStates)

-- пример НКА
--nfa_ex :: NFA
nfa_ex = (['0','1'], [1, 2], 1, tf, [2])
  where
    tf 1 '0' = [1]
    tf 1 '1' = [1, 2]

-- Напишите функцию, определяющую, корректно ли задан НКА
isCorrect :: NFA -> Bool
isCorrect (alph, st, ins, tf, accS) = (not.null $alph) && (not.null $st) && (elem ins st)  && (not.null $accS) && and ( map (not. null. tf ins)  alph)

-- в дальнейшем может пригодиться функция whileM,
-- которая собирает результаты в список, пока истинно
-- заданное условие
whileM :: m Bool -> m a -> m [a]
whileM  = undefined



-- Напишите функцию, определяющую, допускает ли НКА заданное слово 
accept :: NFA -> String -> Bool
accept (alph, st, ins, tf, accS) str =  or .f ins $ str 
            where
                f state "" = if (elem state accS) then return True else return False
                f state str = (tf state $ head str) >>= \st ->  f st ( tail str) 
                
-- Постройте ещё как минимум три примера НКА
--  цепочки заканчиваются последовательностью 00
nfa1 :: NFA
nfa1 = (['0','1'],[0,1,2],0,tf,[2])
    where 
        tf 0 '0' = [0,1]
        tf 0 '1' = [0]
        tf 1 '0' = [2]
        tf 1 '1' = []
        tf 2 '0' = []
        tf 2 '1' = []
    
-- на 4 месте с конца 0
nfa2 :: NFA
nfa2 = (['0','1'], [1,2,3,4,5],1,tf,[5])
    where
        tf 1 '0' = [1,2]
        tf 1 '1' = [1]
        tf 2 '0' = [3]
        tf 2 '1' = [3]
        tf 3 '0' = [4]
        tf 3 '1' = [4]
        tf 4 '0' = [5]
        tf 4 '1' = [5]
        tf 5 '0' = []
        tf 5 '1' = []
-- подпоследовательности 00 и 11
nfa3 :: NFA
nfa3 = (['0','1'], [1,2,3,4],1,tf,[4])
    where
        tf 1 '0' = [1,2]
        tf 1 '1' = [1,3]
        tf 2 '0' = [4]
        tf 2 '1' = []
        tf 3 '0' = []
        tf 3 '1' = [4]
        tf 4 '0' = [4]
        tf 4 '1' = [4]


{-
  Распределите заданные строки в соответствии с распознающими
  их НКА (одна строка может попасть в несколько групп).
-}

classify :: [NFA] -> [String] -> [(NFA, [String])]
classify nfa str = nfa >>= \nfa' ->  return (nfa' ,filter (accept nfa') str ) 
