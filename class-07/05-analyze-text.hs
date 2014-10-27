{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}



import qualified Data.Set as Set
import System.Environment
import Data.List
import Control.Monad
import Data.Char
import Data.List.Split
import Data.Text as T

pronouns = do
    content <- readFile "pronouns.txt"
    let pronounsList =  Data.List.concatMap T.words $T.lines $pack content
    return $pack "no one" :pack "each other": pack "one another":pronounsList
    
prepositions = do
    content <- readFile "prepositions.txt"
    let prepositionsList =  Data.List.concatMap T.words $T.lines $pack content
    return prepositionsList
	   
f4 content = mapM_ (\x -> x content) [f41,f42]

--properNounMidSent::String -> IO()
--properNounMidSent content = do
		

--f43::String -> IO()
--f43 content = do
--	pr <- prepositions
--	p <- pronouns
	
	

f42::String -> IO()
f42 content = do
    p <- prepositions
    putStrLn $ "number of prepositions "++ (show $ Data.List.length $ Data.List.filter (\x -> elem x p  || elem (pack $ (Data.Char.toLower $ T.head x):(unpack $ T.tail x)) p)  allWordsInText)
    where   allWordsInText = Data.List.filter (\x -> unpack x /= "") $ T.split (\x ->   not (isLetter x) ) $ pack content

f41::String -> IO()
f41 content = do
    p <- pronouns
    putStrLn $ "number of pronouns "++ (show $ Data.List.length $ Data.List.filter (\x -> elem x p || elem (pack $ (Data.Char.toLower $ T.head x):(unpack $ T.tail x)) p )  allWordsInText)
    where   allWordsInText = Data.List.filter (\x -> unpack x /= "") $ T.split (\x ->   not (isLetter x) ) $ pack content
    
    
f3  content = mapM_ (\x -> x content) [f31,f32,f33]

f33::String ->IO()
f33 content =  print $ Data.List.maximum $ Data.List.foldl (\acc x -> (Data.List.length $ Data.List.filter (\y ->  y == x) allTrigrams,x): acc ) [] trigrams
    where allWordsInText = Data.List.filter (\x -> Data.List.length x >= 3) $ splitWhen (\x ->   not (isLetter x) ) content
          allTrigrams = Data.List.concat $ Data.List.map (\x -> f x ) allWordsInText
          f (h:ls) = Data.List.map (\(x,y,z) -> x:y:z:[]) $zip3 (h:ls) ls $Data.List.tail ls
          trigrams = Set.toList $ Set.fromList  allTrigrams


    
    
f32::String ->IO()
f32 content =  print $ Data.List.maximum $ Data.List.foldl (\acc x -> (Data.List.length $ Data.List.filter (\y ->  y == x) allBigrams,x): acc ) [] bigrams
    where allWordsInText = Data.List.filter (\x -> Data.List.length x >= 2) $ splitWhen (\x ->   not (isLetter x) ) content
          allBigrams = Data.List.concat $ Data.List.map (\x -> f x ) allWordsInText
          f ls = Data.List.map (\(x,y) -> x:y:[]) $ Data.List.zip ls $Data.List.tail ls
          bigrams = Set.toList $ Set.fromList  allBigrams
    
    
f31::String -> IO()
f31 content =  print $ Data.List.maximum $ Data.List.foldl (\acc x -> (Data.List.length $ Data.List.filter (\y ->  y == x) onlySymbols,x): acc ) [] symbols
    where onlySymbols = Data.List.filter (\x -> not (isSpace x)) content
          symbols = Set.toList $ Set.fromList onlySymbols
          
          
f2PrintAnswer::  [(Int,Text)] -> IO()
f2PrintAnswer ls = do
    mapM_ print $ do Data.List.take 50 $Data.List.reverse$ sort ls 
    

f2:: String -> IO()
f2 content =  f2PrintAnswer$  Data.List.foldl (\acc x -> (Data.List.length $ Data.List.filter (\y ->  y == x) allWordsInText,x): acc ) [] wordsInText
    where allWordsInText = Data.List.filter (\x -> unpack x /= "") $ T.split (\x ->   not (isLetter x) ) $ pack content
          wordsInText = Set.toList $ Set.fromList allWordsInText
                                


f1PrintAnswer::  [(Int,Char)] -> IO()
f1PrintAnswer ls = do
    mapM_ print $ do Data.List.foldl (\acc x -> if snd x /= snd fMax  && fst x == fst fMax then snd x : acc else acc ) [snd fMax] ls 
    where fMax = Data.List.maximum ls
    

f1:: String -> IO()
f1 content = f1PrintAnswer $ Data.List.foldl (\acc x -> (Data.List.length $ Data.List.filter (\y ->  y == x) punctuationInText,x): acc ) [] punctuationChars
    where punctuationInText = Data.List.filter isPunctuation content
          punctuationChars = Set.toList $ Set.fromList punctuationInText
     


functions:: Int -> String -> IO ()
functions numb args= 
   (!!) [f1,f2,f3,f4] numb args

main = do
    [number,fname] <- getArgs
    content <- readFile fname
    functions (read number) content

