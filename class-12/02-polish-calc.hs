{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}



import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import System.Environment



type Value = Int
type Var = String
type Stack = [(Var,Value)]

push :: (Var,Value) -> MaybeT (WriterT (Sum Int) (State Stack)) ()
push x = tell (Sum 1) >> get >>= put . (x:)

-- 4 19 1 + * +   => 80 +  => Nothing
-- операции значения => Nothing 

pop :: MaybeT (WriterT (Sum Int) (State Stack)) (Var,Value)
pop = tell (Sum 1) >> get >>= (\ xs ->  guard (not. null $ xs) >> put (tail xs) >> return (head xs))
                       


evalRPN:: [String] -> (Maybe Int, Int)
evalRPN  xs = do 
   let ((value,s) ,state) = (runState (runWriterT (runMaybeT $ mapM step  xs)) []) 
   case value of 
        Nothing ->  (Nothing , getSum s)
        p       -> (if (length state > 1) then (Nothing , getSum s) else (Just (snd . head $ state), getSum s)) -- 7 8 9 6 + - => state = [7,7] возвращаю Nothing
  where
    step "+" = processTops (+)
    step "*" = processTops (*)
    step "-" = processTops (-)
    step  n  = let [vr,vl] = words n in push ( vr, read vl)
    processTops op = do 
        a <- pop
        b <- pop
        let c = snd a `op` snd b
        push (fst b ,c)
    
    
transformParams :: [String] -> [String]              
transformParams [] = []                  
transformParams xs = if (not ( elem (head xs ) ["+","-","*"]  )) then
                        (head xs ++ " " ++ (head. tail $ xs)) : (transformParams (tail . tail $ xs))
                     else (head xs):(transformParams (tail xs)) 
                     

params :: ReaderT [String]  IO(Maybe Int,Int)
params = do
    n <-  ask
    let res = evalRPN (transformParams n)
    return res
    
main = do 
        p <- getArgs
        runReaderT params p 