import Control.Applicative
import System.Environment
{-
  Пользуясь возможностями аппликативных функторов, определите функцию, 
  вычисляющую наибольший из результатов двух вычислений (значений в некотором
  контексте), принимаемых в качестве параметров (для результатов имеется
  экземпляр класса типов Ord).
-}

maxApp2 :: ( Ord a, Applicative f) => f a -> f a -> f a
maxApp2 f1 f2 =  max <$> f1 <*> f2 

{- Реализуйте аналогичную функцию в случае трёх заданных значений в контексте. -}

maxApp3 :: (Ord a, Applicative f) => f a -> f a -> f a -> f a
maxApp3 f1 f2 f3=  max <$> f1 <*> maxApp2 f2 f3

{- Реализуйте аналогичную функцию в случае списка значений в контексте. -}

maxApp :: (Ord a, Applicative f) => [f a] -> f a
maxApp ls = foldl1 (\a x -> max <$> a <*> x) ls 

{-
  Продемонстрируйте использование написанных функций для аппликативных функторов Maybe,
  список (для каждого из двух экземпляров), Either String и IO.
-}

main = do
    print $ maxApp2 (Just 2) (Just 3) 
    print $ maxApp3 (Just 5) (Just 2) (Just 3) 
    print $ maxApp [Just 5, Nothing, Just 7, Just (-3)]
    a <- maxApp2 getLine getLine 
    b <- maxApp3 getLine getLine getLine
    c <- maxApp [getLine, getLine ,getLine]
    print a 
    print b
    print c
    print $ maxApp2 [1,2,3] [7,8] 
    print $ maxApp3 [1,2,3] [7,8] [9]
    print $ maxApp [[1,2,3] ,[7,8], [9]]
    print $ getZipList$ maxApp2 (ZipList[1,2,3]) (ZipList[7,8]) 
    print $ getZipList$ maxApp3 (ZipList[1,2,3]) (ZipList[7,8]) (ZipList [9])
    print $ getZipList. maxApp. map ZipList $ [[1,2,3] ,[7,8], [9]]
    let (Right l) = maxApp2 (Right "maxLine") (Right "Line") in print l
    let  (Left l ) = maxApp3 (Right "maxLine") (Left "Line") (Right "MaxLine") in print l
    let  (Left l ) = maxApp [Right "maxLine" , Left "Line" , Right "MaxLine"] in print l
    
 {-
 Результаты :
 
Just 3
Just 5
Nothing
1
2
4
5
6
7
8
0
"2"
"6"
"8"
[7,8,7,8,7,8]
[9,9,9,9,9,9]
[9,9,9,9,9,9]
[7,8]
[9]
[9]
"maxLine"
"Line"
"Line"
  
  
 -}  
    

{- (необязательно)
  Ясно ли вам, что вы реализовали нечто, похожее на моноид на аппликативных функторах?
  Можете ли вы выразить это в коде? Необходимо ли добавлять какие-нибудь ограничения?
-}
