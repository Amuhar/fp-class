{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}


import System.Random
import System.Environment
import Data.List
import Control.Monad(when)
import System.IO


compareNumbers :: String -> String -> String
compareNumbers randNumber number = 
   foldl (\acc (x,y) ->  if x == y then acc++ x:" - bull \n" else (if elem x randNumber then acc ++ x:" - cow\n" else acc ++ "")  ) "" $ zip number randNumber 

game::String -> IO()
game randNumber = do
    putStrLn "input number: "
    numberString <- getLine
    when (not $ null numberString) $ do
        if randNumber == numberString
         then putStrLn "You are right!"
         else putStrLn $(compareNumbers randNumber numberString ++ " input another one")
        game randNumber

genNumb _ 0 _ _	 res = res	
genNumb g n ls m res =
    genNumb g' (n-1) (left++right) (m-1) ((show h):res)       
    where (index, g') = randomR (0,m) g::(Int,StdGen)
          (left,h:right) = splitAt index ls
    
main = do
    gen <- newStdGen
    game $ concat $ genNumb gen 4 [0..9] 9 []
