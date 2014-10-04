import Data.Char
{-
  Все задачи в этом задании должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться тремя различными тестовыми примерами, которые при запуске
  возвращают True, например:

  f = undefined -- решение 
  f_test1 = f undefined == undefined -- тест 1
  f_test2 = f undefined == undefined -- тест 2
  f_test3 = f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти сумму и произведение элементов списка вещественных чисел.
  с) Найти среднее арифметическое элементов списка вещественных чисел (функцией length пользоваться нельзя,
     решение должно выполняться в один проход).
  d) Найти минимальный элемент списка.
  e) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным параметром
     функции должно быть значение, возвращаемое по умолчанию).
-}
f1a:: [Int] -> Int
f1a xs = foldl (\c x -> c+ if even x then  x else 0) 0 xs

f1a_test1 = f1a [1,2,3,4,5,6,8,10] == 30
f1a_test2 = f1a [0,7,8,9,5,0,2] == 10
f1a_test3 = f1a [5,7,9] == 0

f1b:: [Double] ->(Double,Double)
f1b xs = foldl (\(s,p) x -> (s+x, p*x)) (0,1) xs

f1b_test1 = f1b [2,3,5,7] == (17,210)
f1b_test2 = f1b [0,9,7,1,0] == (17,0)
f1b_test3 =  f1b [6.5,1,4,2] == (13.5,52)

f1c:: [Double] ->Double
f1c xs =  (\(x,y)-> x/y) $ foldl (\(am,c) x -> (am+x, c+1)) (0,0) xs

f1c_test1 = f1c [2,3,5,10] == 5
f1c_test2 = f1c [0,9,7,1,1] == 3.6
f1c_test3 =  f1c [6.5,1,4,2,0] == 2.7

f1d :: Ord a =>  [a] -> a
f1d xs = foldl1 min xs

f1d_test1 = f1d [0,3,5,6] == 0
f1d_test2 = f1d [-2,-3,0,5,6] == -3
f1d_test3 = f1d [3.0,9,5] == 3.0

f1e:: Integral a => [a] -> a -> a
f1e xs m = if (  []== lm ) then m else (foldr1  min lm) 
	where lm =  foldr (\x acc -> if odd  x then x :acc else acc ) [] xs

f1e_test1 = f1e [2,4,6,8] (-1) == -1
f1e_test2 = f1e [5,7,8,9,0,3] (-1) == 3
f1e_test3 = f1e [8,0,2,4,11,2] (-1) == 11
	
{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий каждый второй элемент исходного.
  b) Сформировать список, содержащий первые n элементов исходного.
  c) Сформировать список, содержащий последние n элементов исходного.
  d) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  e) Сформировать список, содержащий все локальные минимумы исходного списка.
  --f) Дана строка, содержащая слова, разделённые одним или несколькими пробелами. Сформировать
     список слов этой строки. 
  g) Разбить список на непересекающиеся подсписки длиной n элементов.
  h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n). 
  k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.
  l) Повторить каждый элемент списка заданное количество раз.
  m) Удалить из списка повторяющиеся подряд идущие элементы.
  n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}
length' ::Num a => [a1] -> a
length' xs =foldr (\x acc -> acc+1) 0 xs

reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [ ] xs

zip' :: [a] ->[b] -> [(a,b)]
zip' xs ys = reverse' $ foldl (\acc x-> if length' acc /= length' ys then 
									(x ,  head $ f2c ys $ length' ys - length' acc):acc else acc ) [] xs
									
map':: (a->b) -> [a] -> [b]
map' f xs = foldr ((:).f) [] xs 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs  = foldr (\x acc -> if p x then x : acc else acc) [ ] xs
								
								
f2a :: [a] -> [a]
f2a xs = foldr (\(x,y) acc-> if even y then x:acc else acc) [] $ zip' xs $ [1,2..length xs]

f2a_test1 = f2a [0,9,8,7,1] == [9,7]
f2a_test2 = f2a [8,9,0,1,7,6] == [9,1,6]
f2a_test3 = f2a [1] == []
		
f2b :: (Ord a, Num a) => a -> [a1] -> [a1]
f2b n xs = 	reverse' $ 	foldl (\acc x-> if (foldr (\x acc -> acc+1) 0 $ x:acc ) <= n then x:acc else acc) [] xs 

f2b_test1 = null $ f2b 1 []  
f2b_test2 = f2b 5 [6,7,8,9,0,1]  == [6,7,8,9,0]
f2b_test3 = f2b 3 [7,8]  == [7,8]


f2c :: (Ord a, Num a) =>  [a1] -> a -> [a1]
f2c xs n = foldr (\x acc -> if (foldr (\x acc -> acc+1) 0 $ x:acc ) <= n then x:acc else acc) [] xs 

f2c_test1 = f2c [6,9,0,1,2] 3 == [0,1,2]
f2c_test2 = null $ f2c [] 5
f2c_test3 =  f2c [3,4,5,6] 6 == [3,4,5,6]

f2d:: Ord a => [a] -> [a]
f2d xs = 	 foldr (\(x,y) acc -> if (y>x) then y: acc else acc) [ ]   $zip' xs $tail xs


f2d_test1 = f2d [1,2,3,4,6] == [2,3,4,6]
f2d_test2 = f2d [2,3,0,1,5,6] == [3,1,5,6]
f2d_test3 = f2d [1]  == []

zip3':: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' xs ys zs= reverse' $ foldl (\acc x-> if length' acc /= length ys && length' acc /= length zs then 
			(x ,  head $ f2c ys $ length' ys - length' acc, head $ f2c zs $ length' zs - length' acc):acc else acc ) [] xs
f2e:: Ord a => [a] -> [a]
f2e (x:xs) = if x < head xs then x: f else f
	where 
	    ls = head $ tail $ reverse' xs
	    f = ( foldr (\(x,y,z) acc  -> if (y < z && y< x) then y:acc else  acc   ) [] $zip3' (x:xs) xs $ tail xs) ++ 
			(if last xs < ls then [last xs] else [])
		 
f2e_test1 = f2e [3,0,1,4,2,5,7,1,2] == [0,2,1]
f2e_test2 = f2e [2,4,1,3,5,6] == [2,1]
f2e_test3 = f2e [2,4,1,3,5,6,3] == [2,1,3]	



                       --(c: (f2f (f2c xs (length xs -(length c ) -1))   ):[]) 
--f2f :: [Char] -> [Char] 					   
--f2f [] = []      
--f2f xs =  foldl1 (\c x -> if " " == x then  (c:((f2f (f2c xs 3 ) ):[]))  else x++c )  (f2f' xs)

--f2f' xs = reverse' $ foldl (\c x -> [x]:c) [] xs

--remSpacInHead xs = if " " == head xs then remSpacInHead (tail xs) else xs


--g) Разбить список на непересекающиеся подсписки длиной n элементов. 

f2g :: [a] -> Int-> [[a]]
f2g [] n  = []
f2g xs n = (f2b n xs):(if length xs - n >= 0 then (f2g (f2c xs $length xs - n) n ) else [])

f2g_test1 = f2g [0,1,4,5,7,8,9,0,1] 3 == [[0,1,4],[5,7,8],[9,0,1]]
f2g_test2 = f2g [2,1,3,4,5,6,7] 3 == [[2,1,3],[4,5,6],[7]]
f2g_test3 = f2g [2,3,4] 5 == [[2,3,4]]

--h) Разбить список на подсписки длиной n элементов с перекрывающейся частью в k элементов (k < n). 

f2h :: [a] -> Int-> Int-> [[a]]
f2h [] n k  = []
f2h xs n k= (f2b n xs):(if length xs - n +k >= 0  then (f2h (f2c xs (length xs - n +k )) n k) else [])

f2h_test1 = f2h [1,2,3,4,5,6,7,8,9,0] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,0],[9,0]]
f2h_test2 = f2h [4,5,6,7,8] 3 1 == [[4,5,6],[6,7,8],[8]]
f2h_test3 = f2h [1,2,3,4] 5 1 == [[1,2,3,4]]

--k) Сформировать список, содержащий все начальные элементы списка, удовлетворяющие заданному предикату.

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = foldr (\ x c -> if p x then x:c else []) [] xs

f2k_test1 = takeWhile' (< 3) [1,2,3,4,1,2,3,4] == [1,2]
f2k_test2 = takeWhile'(< 0) [1,2,3] == []
f2k_test3 = takeWhile' odd [2,3,6,7,8,0] == []


-- l) Повторить каждый элемент списка заданное количество раз.

f2l :: Int -> [a] ->[a]
f2l k = foldr (\x c -> f x c k) [] 
		where f y c k = foldr (\x c -> y:c) c [1..k]
		
f2l_test1 = f2l 5 [1,2,3,4] == [1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4]
f2l_test2 = null $ f2l 4 [] 
f2l_test3 = f2l 3 [0,3,3] == [0,0,0,3,3,3,3,3,3]
		
 --m) Удалить из списка повторяющиеся подряд идущие элементы.
 
 f2m:: [a]->[a]
 f2m = foldl

 --n) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
 --    заданной функции двух аргументов к соответствующим элементам исходных списков.
	
zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' f xs ys = reverse' $ foldl (\acc x-> if length' acc /= length' ys then ( f x $ head $ f2c ys $ length' ys - length' acc):acc else acc ) [] xs
	
f2n_test1 = zipWith' (+) [2,4,6,7,8] [5,0,1,2,3] == zipWith (+) [2,4,6,7,8] [5,0,1,2,3] 
f2n_test2 = zipWith' max [0,10,7,8] [9,0,1,2] ==  zipWith max [0,10,7,8] [9,0,1,2] 
f2n_test3 = zipWith' (\x y -> (x,y)) [0,10,7,8] [9,0,1,2] ==  zipWith (\x y -> (x,y)) [0,10,7,8] [9,0,1,2]

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать список типа [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  с) Сформировать список из первых n чисел Фибоначчи.
  d) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
  e) Проверить, является ли заданное целое число простым.
-}
f3a:: (Num a,Enum a) => a -> a -> a
f3a a b = foldr (\x acc-> acc+x) 0 [a..b]

f3a_test1 = f3a 1 100 == 5050
f3a_test2 = f3a 0 10 == 55
f3a_test3 = f3a 0 1000 == 500500

sum' :: Num a => [a] -> a
sum' xs = foldr (\x acc -> x+acc) 0 xs

f3b:: Int-> Int -> Int 
f3b a b = sum' $ scanl (\acc x -> x*acc ) (fact a) [a+1..b]  
	
fact y 
		|y == 0 = 1
	    |otherwise =   foldr (\acc x -> x*acc) 1 [1..y]
			 
f3b_test1 = f3b 3 9 == 409110
f3b_test2 = f3b 0 5 == 154 
f3b_test3 = f3b 1 1000 == ( sum $ map (\x -> product [1..x] ) [1..1000])

 -- ??take' для бесконечного списка 
f3c n = take n fib
fib = 0:scanl (+) 1  fib 

f3c_test1 = f3c 7 == [0, 1, 1, 2, 3, 5, 8]
f3c_test2 = f3c 22 == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946]
f3c_test3 = f3c 5 == [0, 1, 1, 2, 3]

taylorSin :: (Floating a, Enum a) => a -> a -> a
taylorSin x n= sum' $ zipWith' (/)
    (map' (\k -> (x)**(2*k + 1) * (-1)**k) [0..n-1])
    (scanl (\a k -> a * k * (k - 1)) 1 [3,5..2*n-1]) 

taylorSin_test1 = abs(taylorSin 5 10 - sin 5 ) < exp(-5) 	
taylorSin_test2 = abs(taylorSin pi 10 - sin pi ) < exp(-5) 		 
taylorSin_test3 = abs(taylorSin (pi/2) 10 - sin (pi/2) ) < exp(-5) 


sqrt' :: Integral a => Float ->a
sqrt' n = ceiling(sqrt n)


isPrime n =   foldl (\acc x ->  ((round(n) `mod` x)  /= 0) && acc  ) True  [2..sqrt' n]

isPrime_test1 = isPrime 5 == True 
isPrime_test2 = isPrime 14 == False 
isPrime_test3 = isPrime 29 == True

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 3) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}



{-
 5. Пусть числовые матрицы представлены списками строк. Реализовать следующие функции:
  1) транспонирование матрицы;
  2) сумма двух матриц;
  3) произведение двух матриц.
-}


repEmpL:: [a] -> [[t]]
repEmpL xs = foldr (\x acc -> []:acc ) [] xs 



f51 :: [[a]] -> [[a]]
f51 xs =  foldr (zipWith' (\x y -> x:y ) ) (repEmpL $ head xs) xs   

f51_test1 = f51 [[1,2,3],[4,5,6],[7,8,9]] == [[1,4,7],[2,5,8],[3,6,9]]
f51_test2 = f51 [[1,2,3]] == [[1],[2],[3]]
f51_test3 = f51 [[1],[2],[3]] == [[1,2,3]]

f52:: Num a =>  [[a]] ->[[a]] -> [[a]]
f52 xs ys = foldr (\(x,y) acc -> (zipWith' (+) x y ): acc ) [] $ zip' xs ys

f52_test1 = f52 [[1,2,3],[4,5,6],[8,9,0]] [[1,0,0],[0,1,0],[0,0,1]] == [[2,2,3],[4,6,6],[8,9,1]]
f52_test2 = f52 [[1,2,3]] [[4,5,6]]== [[5,7,9]]
f52_test3 = f52 [[2],[3],[4]] [[7],[8],[0]] == [[9],[11],[4]]

f53 :: Num a => [[a]] ->[[a]] ->[[a]]
f53 xs ys =f51 $ foldr (\y acc -> (foldr (\x ac ->    (foldr (\z c-> c+z) 0 (zipWith' (*) x y) ):  ac) [] xs ):acc ) [] (f51 ys)

f53_test1 = (f53 [[1,2,3],[4,5,6],[8,9,0]] [[1,0,0],[0,1,0],[0,0,1]]) ==  [[1,2,3],[4,5,6],[8,9,0]] 
f53_test2 = f53 [[1,2,3], [1,5,0],[7,0,1]] [[1,2,0],[2,3,1],[1,2,3]] ==[[8,14,11],[11,17,5],[8,16,3]]
f53_test3 = f53 [[3,4,5]] [[11],[2],[0]] == [[41]]
f53_test4 = f53 [[2,1,5,6],[1,2,3,0]] [[3,4,5],[1.0,2,4],[5,6,0] ,[2,1,1]] == [[44,46,20],[20,26,13]]

{-
 6. Реализовать левую свёртку, пользуясь правой. Проанализировать поведение собственной реализации
  на бесконечных списках и сравнить его с поведением оригинальной foldl.
-}
