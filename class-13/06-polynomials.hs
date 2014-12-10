{-# LANGUAGE EmptyDataDecls #-}

import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Data.Char
import Control.Monad
import Data.List
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

                          


readxInDeg = symbol "x" >> optional (1.0,1) (symbol "^" >> natural >>= \n -> return (1.0,n))


summand = do
          token( readxInDeg  <|>  (coefficient  >>= \cf -> (readxInDeg >>= \(c,d) -> return (cf*c,d))  <|> return (cf,0)))

coefficient = float <|> (char '+' >> float) <|> (char '-' >> return (-1))       
 
poly :: Parser Poly
poly = Poly `liftM` (many1 summand)
  

      

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}

fromPoly :: Poly -> [(Float,Int)]
fromPoly (Poly ls) = ls

polyDeg :: CoefficientsDegree -> Int
polyDeg ls = length ls -1

divmod :: Poly -> Poly -> (Poly, Poly)
divmod p1 p2 = (\(x,y)-> (Poly x, Poly y) ) $ f (fromPoly p1) (fromPoly p2) []
    where 
         f r p q | oldDeg  <0 = (reverse q, r)
                  | otherwise = f diff p ((oldCoef, oldDeg):q) 
             where 
                oldCoef = (fst.head $ r) / (fst. head $ p )
                oldDeg = (snd.head $ r) - (snd.head $ p)
                mp2 = map (\(c,d) -> (c*oldCoef, oldDeg + d)) p
                diff = diffPoly r mp2

modifPoly p  =reverse $ foldl (\acc (x,y) -> case (x,y) of 
                                             (x1,y1)| snd x1 == snd y1  -> ((fst x1 - fst y1, snd x1):acc)
                                                    | snd x1 > snd y1 -> (-(fst y1),snd y1):x1:acc
                                                    |otherwise -> x1:(-(fst y1),snd y1):acc    ) [] p
 

dropZeroEl p = filter ((/= 0 ).fst) p
 
addZero p d = p ++  replicate d (0,0)   
            
diffPoly p2 p1 = dropZeroEl $ if diff >0 then (modifPoly $ zip p2 (addZero p1 diff)) else (modifPoly $ zip (addZero p2 (-diff)) p1)
  where          
       diff =  length p2 - length p1

 


{-divmod :: Poly -> Poly -> (Poly, Poly)
divmod p1 p2=  f (fromPoly p1) (fromPoly p2) []
    where 
        f l1 l2 q | degDiff l1 l2 <0 = (q,l1) --l1 - остаток , q - частное
                  | otherwise = f  ? l2 
            
            where
                    pr = ( oldK l1 l2,degDiff l1 l2)
                    q' =  (reverse$ :q)
                    oldK l1 l2 = (fst.head $ l1) - (fst.head $ l2)
                    degDiff m1 m2 = (polyDeg m1) - (polyDeg m2)
                    pl2q' = map (\(c,d) -> (c*(fst pr),d+(snd pr))  ) l2
                    diff l1 pl2q' = zipWith' (-) l1 pl2q' 
        
zipWith' p1 p2 = f pn1 pn2 []
    where 
        diff = length p1 - length p2
        pn1 = if diff >0 then pn1 else (p1++(replicate diff 0))
        pn2 = if diff > 0 then p2 else (p2++(replicate diff 0))
        f (x:xs) (y:ys) p = case x of
                             x == y -> f xs ys p1
                             snd x > snd y -> f xs ys (s)-}



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
