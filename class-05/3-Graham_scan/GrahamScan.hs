{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where

import Data.List 

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.



data Point = Point Double Double
	deriving(Show,Ord,Eq)
  
{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}


data Direction = VLeft| VRight| VLine
	deriving (Show,Ord,Eq)


{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}



zVecProd ::[Point] -> Double
zVecProd [(Point x1 y1),(Point x2 y2),(Point x3 y3)] = (x2-x1)*(y3-y1) - (y2-y1)*(x3-x1)

										
directCreate:: [Point] -> Direction
directCreate xs
				|zVecProd xs > 0 = VLeft
				|zVecProd xs < 0 = VRight
				|otherwise = VLine
																  
directions :: [Point] -> [Direction]
directions xs  
			|length xs <= 2 = []
			|otherwise = directCreate (take 3 xs ) : (directions $ drop 1 xs)
		


			
{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}
minY :: [Point] -> Point
minY xs = foldl1 (\(Point x1 y1) (Point x y) -> if (y<y1)|| (y== y1 && x < x1) then Point x y else Point x1 y1) xs


atan' :: Point -> Point -> Double 
atan' (Point x y) (Point x0 y0 )= if atn < 0 then atn +pi else atn
					               where atn  = atan $ (y-y0)/(x-x0) 
length' :: Point -> Point -> Double								   
length'  (Point x y) (Point x0 y0 ) = 	sqrt((x-x0)^2 + (y-y0)^2)							
	
compare'::	Point -> Point->Point -> Ordering
compare' p1 p2 p0 =  if cmp == EQ then (compare (length' p1 p0) (length' p2 p0)) else cmp
													where cmp = compare (atan' p1 p0 ) (atan' p2 p0) 

sortStPoint:: [Point] -> Point -> [Point]													
sortStPoint xs  p0 =   sortBy (\p1 p2 -> compare' p1 p2 p0 ) xs

sortPoint ::[Point] -> Point -> [Point]
sortPoint xs minY' = sortStPoint  (filter (/= minY') xs) minY' 

--graham_scan_step :: [Point] -> ->[Point]
graham_scan_step pn acc VLeft = acc
graham_scan_step pn acc directions' 
								| length acc <= 2 = tail acc
								|otherwise = graham_scan_step pn (tail acc) (head $ directions [ head $ tail $ tail acc,head $ tail acc, pn])


graham_scan :: [Point] ->  [Point]
graham_scan xs = foldl (\acc x ->  reverse $ (x:( graham_scan_step x (reverse acc) $ head$ directions [head $ tail (reverse acc) ,head $ (reverse acc) , x]))   ) [minY',head $ sortPoint xs minY'] $ tail $ sortPoint xs minY'
							where minY' = minY xs
		
{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

graham_scan_test1 = graham_scan [Point 6 5, Point 9 4,Point 11 5.2,Point 4 7,Point 4 1,Point 1 4 ] ==[Point 4 1 ,Point 11 5.2, Point 4 7,Point 1 4]

graham_scan_test2 = graham_scan  [Point 6 5, Point 9 4,Point 11 5.2,Point 3 3, Point 5 4, Point 5 5, Point 6 3,Point 4 7,Point 4 1,Point 1 4 ] == [Point 4 1 ,Point 11 5.2, Point 4 7,Point 1 4]

graham_scan_test3 = graham_scan  [Point 1 7 ,Point 6 5, Point 9 4,Point 11 5.2,Point 3 3, Point 5 4, Point 5 5, Point 6 3,Point 4 7,Point 4 1,Point 1 4 ] == [Point 4 1 ,Point 11 5.2, Point 4 7,Point 1 7,Point 1 4]