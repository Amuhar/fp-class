import System.Environment
import Data.Monoid
import Data.Maybe
import Data.List
{-
  Некоторый датчик генерирует по пять сигналов в сутки, часть из которых
  не доходит до базовой станции. Полученные от датчика сведения представлены
  текстовым файлом, содержащим по одному целому числу в каждом строке. Если
  сигнал не был получен, вместо числа в файле записывается прочерк (символ '-').
-}

type SensorValue = Maybe Int
type SensorData = [SensorValue]

{- Напишите функцию, которая преобразует прочитанную из файла строку в список
   значений, полученных от датчика. -}

getData :: String -> SensorData
getData = (map (\x -> if x == "-" then Nothing else Just $ read x) . lines)

{- Напишите функцию, группирующую данные по суткам. -}

dataByDay :: SensorData -> [SensorData]
dataByDay [] = []
dataByDay data' =  take 5 data' :(dataByDay $drop 5 data')

{-
  Посчитайте минимальное значение среди показаний датчика,
  полученных:
  а) первыми в течение суток;
  б) последними в течение суток.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов First и Last,
  при этом должна быть написана одна функция, отвечающая на вопрос а) или б)
  в зависимости от значения логического параметра.
-}

minData1 :: Bool -> [SensorData] -> Int
minData1 flag data' 
  |flag  = ( fromJust.minimum . map (\x -> getFirst. mconcat . map First $ x) ) $filter (not.all isNothing)  data' 
  |otherwise =  ( fromJust.minimum . map (\x -> getLast . mconcat . map Last $ x) ) $filter (not.all isNothing)  data'

{-
  Посчитайте минимальное значение среди данных,
  полученных:
  а) как суммы всех показаний датчика за каждые сутки;
  б) как произведения всех показаний датчика за каждые сутки.
  Если в некоторые сутки показания не были получены ни разу,
  такие сутки должны игнорироваться.

  Указание: в решении следует пользоваться возможностями моноидов Sum, Product
  и Maybe a, где a — моноид, при этом должна быть написана одна функция, отвечающая
  на вопрос а) или б) в зависимости от значения логического параметра.
-}
forSum::[[Maybe a]] -> [[Maybe (Sum a)]]
forSum data' =  filter (not.null ) $map (foldl (\acc x->  if isNothing x then acc else (Just . Sum . fromJust $ x ) : acc) []  ) data'

forProduct::[[Maybe a]] -> [[Maybe (Product a)]]
forProduct data'= filter (not.null) $map (foldl (\acc x->  if isNothing x then acc else (Just . Product . fromJust $ x ) : acc) [] ) data'


minData2 :: Bool -> [SensorData] -> Int
minData2 flag data' 
  |flag  =  minimum . map (getSum. fromJust. mconcat ) $forSum data' 
  |otherwise =  minimum . map (getProduct. fromJust. mconcat ) $forProduct data'
  

{- Попробуйте объединить две предыдущие функции в одну. -}

data SensorTask = NeedFirst | NeedLast | NeedSum | NeedProduct deriving(Eq)

minData :: SensorTask -> [SensorData] -> Int
minData st data'
    |st == NeedFirst = minData1 True data'
    |st == NeedLast = minData1 False data'
    |st == NeedSum = minData2 True data'
    |otherwise = minData2 False data'


{-
  Пользуясь моноидами All, Any и любыми другими, выясните следующую информацию:
  1) количество суток, за которые не было получено ни одного показания;
  2) количество суток, показания за которые получены полностью;
  3) количество суток, за которые было получено хотя бы одно показание;
  4) количество суток, сумма показаний за которые превосходит заданное число;
  5) количество суток, произведение показаний за которые превосходит заданное число;
  6) количество суток, первое показание за которые превосходит заданное число;
  7) количество суток, последнее показание за которые превосходит заданное число.

  Постарайтесь ответить на все вопросы, написав одну функцию.
-}
forPred::[[Maybe a]] -> [[Bool]]
forPred = map (map (not . isNothing ))


statisticData:: [Int] ->[SensorData] -> Int
statisticData args data' = let n:number = args in 
                            case n of
                            1 ->  length . filter (==False) $ map (getAny . mconcat . map Any) $forPred data'
                            2 ->  length . filter (==True) $ map (getAll . mconcat. map All) $ forPred data'
                            3 ->  length . filter (==True) $ map (getAny . mconcat. map Any) $ forPred data'
                            4 ->  length . filter (> head number) $ map (getSum. fromJust. mconcat ) $forSum data' 
                            5 ->  length . filter (> head number) $ map (getProduct. fromJust. mconcat ) $forProduct data' 
                            6 ->  length . filter (\x -> (not . isNothing $ x) && fromJust x> head number) $ map (getFirst. mconcat . map First ) data' 
                            7 ->  length . filter (\x -> (not . isNothing $ x) && fromJust x> head number) $ map (getLast. mconcat . map Last )  data' 


main = do
  fname:args <- getArgs
  sData <- getData `fmap` readFile fname
  let n = if length args == 1 then [read. head $ args] else [read. head $ args, read. last $ args]
  let data' = dataByDay sData
  print $ minData NeedFirst data'
  print $ minData NeedLast data'
  print $ minData NeedSum data'
  print $ minData NeedProduct data'  
  print $ statisticData  n data'
