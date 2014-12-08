import Parser
import SimpleParsers
import ParseNumbers
import Data.Char
import Control.Applicative
{- Напишите парсер для вещественных чисел. -}

          
float :: Parser Float
float = fr <|> fn
    where 
       fr =  do 
                 n <- integer
                 char '.'
                 m <- natural
                 return $ read (show n ++ "." ++ show m) 
       fn = do
                n <- integer
                return (read (show n) :: Float)
        
{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
  
-}
complex :: Parser (Float, Float)
complex = do
            string "(" 
            r <- float
            char ','
            m <- float
            string ")"
            return (r,m)


{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = do
                string "["
                f []
                where 
                   f ls = do 
                        (r,m) <- complex 
                        (char ';' >> f (ls++[(r,m)] )) <|> (string "]" >> return (ls++[(r,m)] ))    
{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = do
                string "["
                f []
                where 
                   f ls = do
				            (complex >>= \p -> char ';' >> f (ls++[p])) <|> (float >>= \p -> char ';' >> f (ls++[(p,0)])) <|>
							(complex >>= \p -> string "]" >> return (ls++[p] )) <|> (float >>= \p -> string "]" >> return (ls++[(p,0)] ))
                    

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = bracket "[" "]" $ (sepBy (complex <|> (float >>= (\ f -> return (f,0)) )) (symbol ",")) 


