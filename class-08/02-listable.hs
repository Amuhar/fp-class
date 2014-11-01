{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

import Data.List

   
class Listable a where
    toList :: a -> [a]
    fromList :: [a] -> a
{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer a - любое целое число разбивается на список цифр.
-}
instance Listable String where
    toList a = words a
    fromList a = concat a

instance Listable Integer where
    toList a =  foldr (\x acc -> read (x:"") : acc) [] $ show a
    fromList a = read $ foldr (\x acc-> show x ++ acc) "" a
	
