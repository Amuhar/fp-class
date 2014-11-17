{- Пользуясь списком как монадой, вычислите пересечение  заданных списков -}
import Data.List 
import Data.Set
import System.Environment

intersect' ::( Eq a ,Ord a )=> [[a]] -> [a]
intersect' ls = (toList . fromList . head $ ls ) >>= \n -> if all (elem n) ls then [n] else []


tests = all test [1..5]
    where 
        test 1 = intersect' [[1,2,3],[4,5,6],[7,8]] == []
        test 2 = intersect' [[1,2,3,3],[2,3,5,7],[9,0,3,3,2]] == [2,3]
        test 3 = intersect' [[1,2,3,5],[1,2,3],[]] == []
        test 4 = intersect' [[1,2,3],[1,2],[1,5,6]] == [1]
        test 5 = intersect' [[9,0,-1,3,-2],[-2,3], [2,0,9,1]] == []


main = return  tests