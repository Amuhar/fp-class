{- 4. Пользуясь средствами монады ST, запрограммировать сортировку массива тремя любыми методами. -} 

import Data.STRef
import Control.Monad.ST
import Control.Monad
import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Data.List

selectSort arr newls numbElem 
    |length newls == numbElem = return arr 
    |otherwise= do
                mx <- getAssocs  arr
                let m = minimumBy (\x y -> compare (snd x) (snd y) ) $ (\\) mx newls  
                let ind = length newls 
                vi <- readArray arr ind
                writeArray arr ind (snd m)
                writeArray arr (fst m) vi
                selectSort arr ((ind , snd m):newls) numbElem 

                
testSelect xs = elems $ runSTArray $ do
    arr <- newListArray (0, length xs - 1) xs
    selectSort arr [] (length xs)
   
 
swapElems i j arr = do
        vi <- readArray arr i
        vj <- readArray arr j
        writeArray arr i vj
        writeArray arr j vi

        
        
bubbleSort xs = elems $ runSTArray $ do
            arr <- newListArray (0, length xs-1) xs
            bSort arr (length xs)
            where 
                bSort arr c  = do
                         t <- bSort' arr [] c
                         xs <- getElems  t
                         if xs == (sort xs) then return t else bSort t c
                bSort' arr newls numbElem 
                    |length newls == numbElem = return arr
                    |otherwise = do
                                mx <- getAssocs  arr
                                let (x:y:xs) = (\\) mx newls
                                let m = if snd x < snd y then x else y
                                when (snd x > snd y) (swapElems (fst x) (fst y) arr)
                                bSort' arr (m:newls) numbElem
  
                
	