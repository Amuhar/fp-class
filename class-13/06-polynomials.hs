{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative
import Data.Char
import Control.Monad
{-
   Определите тип для многочлена с вещественными коэффициентами.
-}
type CoefficientsDegree = [(Float,Int)]
data Poly = Poly  CoefficientsDegree deriving (Show)


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
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}
-- для многочленов , записанных в форме cn*x^n + cn-1*x^(n-1)+..+c0 
coefficient = do 
        space
        c <- getc
        case c of 
         t| t == 'x' -> token((char '^' >> natural >>= \n -> return (1.0,n)  ) <|> return (1.0,1))
          | t == '-' -> token((char 'x' >> (char '^' >> natural >>= \n -> return (-1.0,n)  ) <|> return (-1.0,1)) <|>
                        (float >>= \cf -> (char 'x' >> (char '^' >> natural >>= \n -> return (-cf, n) )<|> return (-cf,1))
                        <|> return (-cf,0)))
          | t == '+' -> token ((char 'x' >> (char '^' >> natural >>= \n -> return (1.0,n)  ) <|> return (1.0,1)) <|>
                        (float >>= \cf -> (char 'x' >> (char '^' >> natural >>= \n -> return (cf, n) )<|> return (cf,1))
                        <|> return (cf,0)))
          | isDigit t -> token (char 'x' >> (char '^' >> natural >>= \n -> return (read (t:""), n) )<|> return (read (t:""),1)) 
                          


poly :: Parser Poly
poly = Poly `liftM` many1 coefficient
   
      

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}
divmod :: Poly -> Poly -> (Poly, Poly)
divmod = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = undefined

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = undefined

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = undefined
