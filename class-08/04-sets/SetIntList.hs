module SetIntList (Set,member,AS.insert,AS.empty,AS.delete) where 

import AbstractSet as AS
import Data.List

newtype Set a =  Set [a] deriving (Show)
    

instance AbstractSet Set where
    empty = Set []
    member (Set xs) x = elem x xs
    insert (Set xs) x = case elem x xs of 
                        True -> Set xs
                        _ -> Set $ sort (x:xs)
    delete (Set xs) x = Set $ filter (/= x) xs
    count (Set xs) = length xs
    