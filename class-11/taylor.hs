{- 2. Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора и сохраняя каждое слагаемое
в журнал посредством монады Writer. В тексте программы допускается только один вызов функции tell.-}


import Control.Monad.Writer
import System.Environment

fact 0 = 1
fact n = n*fact(n-1)

sin' n k x pr res ls
    | n == k = tell (reverse ls) >> return res
    | k == 0 = sin' n 1 x x x (x:ls)
    | otherwise = return  ((/) (-pr *x**2 ) ((2*k+1)*2*k))  >>= \r -> sin' n (k+1) x r (res + r) (r: ls)


    
    
cos' n k x pr res ls
    | n == k = tell (reverse ls) >> return res
    | k == 0 = cos' n 1 x 1 1 (x:ls)
    | otherwise = return  ((/) (-pr *x**2 ) (2*k*(2*k-1)))  >>= \r -> cos' n (k+1) x r (res + r) (r: ls)
    


tests = all test [1..6]
    where 
        test 1 = abs((fst. runWriter $ sin' 10 0 (pi/2) 0 0 []) - sin (pi/2) ) < 1.0e-10
        test 2 = abs ((fst. runWriter $sin' 10 0 (2*pi/9) 0 0 []) - sin (2*pi/9) ) < 1.0e-10
        test 3 = abs ((fst. runWriter $sin' 10 0 (pi/3) 0 0 []) - sin (pi/3) ) < 1.0e-10
        test 4 = abs((fst. runWriter $cos' 10 0 0 0 0 []) - cos 0 ) < 1.0e-10
        test 5 = abs ((fst. runWriter $cos' 10 0 (2*pi/9) 0 0 []) - cos (2*pi/9) ) < 1.0e-10
        test 6 = abs ((fst. runWriter $cos' 10 0 (pi/4) 0 0 []) - cos (pi/4) ) < 1.0e-10
        

main = return  tests 