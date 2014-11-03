import AbstractSet as AS
import qualified SetIntList as SL
import qualified SetBinaryTree as ST

import Data.List
import System.Random
import System.Environment

test1 s = member (AS.delete (AS.insert (AS.insert s 5) 6) 5 ) 5 == False 



addElem s c k ls 
    |c == k = s
    |otherwise = addElem (AS.insert s $ (!!) ls k) c (k+1) ls



test2 s ls c 
    |c > length ls = count s == length ls
    |otherwise = test2 (addElem s c 0 ls ) ls (c+1)
    
    


genRandomIntList _ 0 _ _ res = res   
genRandomIntList gen n ls m res =
    genRandomIntList g (n-1) (left++right) (m-1) (h:res)       
    where (index, g) = randomR (0,m) gen::(Int,StdGen)
          (left,h:right) = splitAt index ls
          
           

main = do 
       gen <-newStdGen
       let (len, newGen) = randomR (1,20) gen :: (Int, StdGen)
       let ls = genRandomIntList newGen len [1..20] 19 []
       print $ test1 (AS.insert empty 5 :: SL.Set Int) && (test1 (AS.insert empty 5 :: ST.Set Int))
       print $test2 (empty ::SL.Set Int) [1,2,3,4,5] 1 && test2 (empty ::ST.Set Int) [1,2,3,4,5] 1 
       print $test2 (empty ::SL.Set Int) ls 1 && test2 (empty ::ST.Set Int) ls 1       
        
       
       



 




