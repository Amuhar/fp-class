import Data.List
import Data.Char
import Numeric

{-
Явная рекурсия в решениях хотя и допускается, но не приветствуется. Старайтесь обходиться стандартными
функциями, используя при этом создание функций «на лету». Пытайтесь максимально упростить уже написанные
решения, применяя подходящие функции из модуля Data.List и любых других модулей. Перед выполнением заданий
изучите примеры из лекции по функциям высшего порядка. 
-}

{-
 1. Простейшие задачи на применение функций map и filter.
 1.1 Преобразовать данный список целых чисел следующим образом:
  a) увеличить все его элементы в два раза;
  b) увеличить все его элементы с четными значениями в два раза;
  с) обнулить все его элементы с нечетными значениями;
  d) удалить из него элементы, большие заданного числа k;
  e) отфильтровать его, оставив в списке только отрицательные числа;
  f) удалить из него все положительные чётные числа.
-}

f11a :: Integral a => [a] -> [a]
f11a xs = map (*2) xs

f11b :: Integral a => [a] -> [a]
f11b xs = f11a $ filter even xs

f11c :: Integral a => [a] -> [a]
f11c xs = map (\n -> if odd n then 0 else n ) xs

f11d :: (Ord a,Integral a) => a-> [a] -> [a]
f11d k xs= filter (<= k)  xs

f11e :: Integral a => [a] -> [a]
f11e xs = filter (<0) xs

f11f :: Integral a => [a]->[a]
f11f xs = filter (\n -> (n <=0) ||(odd n)) xs
{-
 1.2 Дан список декартовых координат точек на плоскости (пар вещественных чисел).
     Преобразовать его следующим образом:
  a) отфильтровать список так, чтобы в нём остались точки из заданной координатной четверти;
  b) преобразовать декартовы координаты в полярные.
-}
type Point = (Double, Double)
f12a :: [Point]->Int -> [Point]
f12a xs q 
	|q == 1 = filter (\(x,y) -> (x>0) && (y>0) ) xs
	|q == 2 =  filter (\(x,y) -> (x<0) && (y>0) ) xs
	|q == 3 =  filter (\(x,y) -> (x<0) && (y<0) ) xs
	|otherwise =  filter (\(x,y) -> (x>0) && (y<0)) xs

f12b :: [Point] -> [Point]
f12b xs = map (\(x,y)-> if sqrt(x^2+y^2) == 0 then (x,y) else (sqrt(x^2+y^2),asin$ y/sqrt(x^2+y^2))) xs
		



{-
 1.3 Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
-}


f13a :: [String] -> [String]
f13a lstr = map (\str-> map (\s-> toUpper s) str) lstr 

f13b :: [String] -> Int -> [String]
f13b lstr len =  filter (\str -> length str == len ) lstr

f13c :: [String] -> Char -> [String]
f13c lstr c = filter (\str -> head str == c) lstr 

{-
2. Формирование числовых последовательностей (iterate).
 a) Список натуральных чисел, начиная с 0.
 b) Список чётных чисел.
 c) Список элементов последовательности: a0=1, an=(1+an-1)/2.
 d) Список символов английского алфавита.
 e) Список строк, представляющих n-значные двоичные числа.
-}

nats :: [Integer]
nats = iterate (+1) 0

evens ::[Integer] 
evens = iterate (+2) 0

lseq ::Int -> [Double]
lseq n = take n $ iterate ((/2).(+1)) 1

alph:: [Char]
alph = take 26 $ iterate (chr .(+1) . ord) 'a'

binNumb:: Int -> [String]
binNumb n = map (\d -> showIntAtBase 2 intToDigit d ""  ) $ take (2^n) $ iterate (+1) 0  

{-
3. Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу: цифры — не цифры — ...
  b) Дан список пар вещественных чисел (координат точек на плоскости). Сгруппировать подряд идущие
     координаты точек, лежащие в одной координатной четверти.
  c) Дан список и ненулевое натуральное число n. Разбить список на подсписки длиной n каждый.
     Последний подсписок может содержать менее n элементов.
  d) Дан список и ненулевые натуральные числа n и m. Разбить список на перекрывающиеся подсписки
     длиной n элементов со сдвигом относительно предыдущего подсписка на m элементов.
  e) Дан список. Определить длину самого длинного подсписка, содержащего подряд идущие одинаковые элементы.
-}

f3a :: [Char] ->[String]
f3a w = groupBy (\x y -> (isDigit x && isDigit y)||(isLetter x && isLetter y)) w


xy :: Point->Int 
xy (x,y) 
	|x>0 && y>0 = 1
	|x<0 && y>0 = 2
	|x<0 && y<0 = 3 
	|otherwise =  4
	
f3b ::[Point] ->[[Point]]
f3b w = groupBy (\x y-> xy x == xy y ) w 

f3c :: [a] ->Int -> [[a]]
f3c list n= map (take n) $ takeWhile (not.null) $iterate (drop n) list

f3d :: [a] -> Int -> Int -> [[a]]
f3d xs n m = map (take n) $ takeWhile (not.null) $iterate (drop m) xs

-- Должно быть True
test_f3d = f3d [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

f3e :: Eq a =>[a] -> Int
f3e xs = maximum $ map (\x -> length x) $ groupBy (==) xs

{-
4. Разные задачи.
 a) Дан текст в виде строки символов, содержащий среди прочего числовые данные. Посчитать количество
    всех упоминающихся в тексте чисел.
 b) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату, в указанном промежутке
    (например: все чётные от 1 до 106).
 c) Дана строка текста и число n. Сформировать список, содержащий n самых часто используемых
    в строке символов.
 d) Дан список чисел. Сформировать список локальных максимумов исходного списка. Локальным максимумом
    называется элемент, больший своих соседей.
 e) Дан список. Продублировать все его элементы.
-}

f4a :: String -> Int
f4a s = length $ filter (\x-> isDigit x) s

f4b :: Num a => a -> Int -> a
f4b f l = sum $ take l $ map fst $ iterate (\(x,y) -> (y,x+y) ) (f,f+1)


f4c :: String -> Int-> [Char]
f4c s n =   take n $ ls
	where 
		  ls = map (\x-> snd$ last x) (takeWhile (not.null) $ iterate (\x->take (length x - (fst $ last x ))x) sort)
		  sort = sortBy compare ( map (\y -> (length $ filter (\x-> x==y) s,y) ) s)

f4d ::Ord a => [a] ->[a]
f4d xs
	|null xs = error "empty list"
	|(head xs) > (head $ tail xs) = head xs :lsf
	|otherwise = lsf
	where
		lsf = (map (\(x,y,z)-> y) $ filter (\(x,y,z)-> x<y && y> z) lst )++ (if last xs > (!!) xs (length xs - 2) then [last xs] else [] ) 
		lst = map (\(x:s)-> (x,head s, last s)) ls
		ls = map (take 3) $ take  (length xs-2) $ iterate (drop 1) xs
		  
		
		  
		  
f4e ::[a]->[a]
f4e ls = concat $ map (\x->[x,x]) ls
