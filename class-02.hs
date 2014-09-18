-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.

sec2hmc :: Int -> (Int,Int,Int)
sec2hmc time = (h,div m' 60,mod m' 60) 
   where h = div time 3600
         m' = mod time 3600

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h*3600+m*60+s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s= hms2sec (h,m,s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hmc x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x2-x1)^2+(y2-y1)^2)

triangle :: Point -> Point-> Point -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3,y3) = (p, s)
  where
	a = distance (x1, y1) (x2, y2)
	b = distance (x2 ,y2) (x3,y3)
	c = distance (x1 ,y1) (x3,y3)
	p = a +b +c
	sp = p/2
	s = sqrt (sp*(sp-a)*(sp-b)*(sp-c))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) = (if even x then 1 else 0 )+ nEven xs 

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = 2*x: doubleElems xs

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) = if odd x then x: fltOdd xs else fltOdd xs

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
deleteNegElem ::  (Ord a, Num a) => [a] -> [a]
deleteNegElem [] = []
deleteNegElem (x:xs) = if x < 0 then deleteNegElem xs else x: deleteNegElem xs
-- б) увеличить элементы с чётными значениями в два раза;
doubleEvenElems :: Integral a => [a] -> [a]
doubleEvenElems [] = []
doubleEvenElems  (x:xs) = (if even x then 2*x else x ): doubleEvenElems xs

-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
swapEvenAndOddPlaces :: Integral a => [a] -> [a]
swapEvenAndOddPlaces [] = []
swapEvenAndOddPlaces [x] = []
swapEvenAndOddPlaces (x:xs) = y : (x:swapEvenAndOddPlaces ys)
						where 
							(y:ys) = xs


-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) =  x+y: combine_plus xs ys

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
combine_pair :: [a] ->[a] -> [(a,a)]
combine_pair [] ys = []
combine_pair xs [] = []
combine_pair (x:xs) (y:ys) = (x,y):combine_pair xs ys
-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
listn1:: Integral a => a -> [a]
listn1 0 = []
listn1 n = n : listn1 (n-1)


-- б) в порядке возрастания.
list1n_count::Integral a => a -> a-> [a]
list1n_count 0 _=[]
list1n_count n c = c: (list1n_count (n-1) (c+1))

list1n:: Integral a => a -> [a]
list1n n = list1n_count n 1



-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insElemBetweenAllElems:: [a] -> a -> [a]
insElemBetweenAllElems [] a = []
insElemBetweenAllElems (x:xs) a = x:a: (insElemBetweenAllElems xs a) 

-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
sublistDiv:: Eq a => [a] -> [a] -> ([a],[a])
sublistDiv  [] ys  = (ys,[])
sublistDiv (x:xs) (y:ys) =  if (x == y) then (sublistDiv xs (x: (y:ys))) else  ((y:ys),(x:xs))
			
			
subList:: Eq a =>  [a]->([a],[a])
subList [] = ([],[])
subList (x:xs) = sublistDiv xs [x]


--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
	
elemN :: [a] -> Int -> a
elemN (x:xs) 0 =   x
elemN (x:xs) n =   if length(x:xs) <= n then error("index too large") else   elemN xs (n-1)

-- б) Eq a => [a] -> a -> Bool
elemIsMemb ::Eq a => [a] -> a -> Bool
elemIsMemb [] a = False
elemIsMemb (x:xs) a = if (x == a) then True else elemIsMemb xs a
-- в) [a] -> Int -> [a]
firstNElems :: [a] -> Int -> [a]
firstNElems (x:xs) 0 = []
firstNElems (x:xs) n = if length(x:xs) < n then error("number too large") else x: firstNElems xs (n-1)

-- г) a -> Int -> [a]
replicate' :: a -> Int -> [a]
replicate' a 0 = []
replicate' a n = a: (replicate' a (n-1))

-- д) [a] -> [a] -> [a]

unionLists:: [a] -> [a] -> [a]
unionLists [] xs = xs
unionLists (x:xs) ys = x:(unionLists xs ys)

-- е) Eq a => [a] -> [[a]]
group_h ::Eq a => [a] ->[a] -> [[a]]
group_h [] ys = ys:[] 
group_h (x:xs) ys = if x == (head ys) then  group_h xs (x:ys) else ys :(group_h xs [x] )

group'::Eq a => [a] -> [[a]]
group' [] = [[]]
group' xs = group_h (tail xs) [head xs]
       

-- ж) [a] -> [(Int, a)]
groupPairNElem_count::[a]->Int -> [(Int, a)]
groupPairNElem_count [] n = []
groupPairNElem_count xs n = (n,head xs): (groupPairNElem_count (tail xs) (n+1))

groupPairNElem :: [a] -> [(Int, a)]
groupPairNElem [] = error "list is empty"
groupPairNElem xs =  groupPairNElem_count xs 0

-- з) Eq a => [a] -> [a]
listUniqElem_h::Eq a => [a]->[a] -> [a]
listUniqElem_h [] _ = [];
listUniqElem_h (x:xs) ys = if elemIsMemb ys x then listUniqElem_h xs ys else x:(listUniqElem_h xs (x:ys))

listUniqElem::Eq a => [a] -> [a]
listUniqElem [] = []
listUniqElem (x:xs) = x:(listUniqElem_h xs [x])
