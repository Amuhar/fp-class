module SetBinaryTree (Set,member,insert,empty) where 

import AbstractSet as AS


data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) 
                     deriving (Show, Eq)



newtype Set a =  Set (BinaryTree a) deriving (Show)
  
insertT EmptyTree x = Node x EmptyTree EmptyTree
insertT (Node a left right) x
        | x == a = Node a left right
        | x < a = Node a (insertT left x) right
        |otherwise = Node a left (insertT right x)
memberT EmptyTree x = False
memberT (Node a left right) x
        | x == a = True
        | x < a = memberT left x
        |otherwise = memberT right x
        
deleteT EmptyTree x = EmptyTree
deleteT (Node a left right) x
        |a == x = deleteT' (Node a left right)
        |a > x = Node a (deleteT left x) right
        |otherwise = Node a left (deleteT right x)


deleteT' (Node a EmptyTree right) = right
deleteT' (Node a left EmptyTree) = left
deleteT' (Node a left right)  =  Node  minR left (deleteT right minR )
        where minR = minElem right
     
minElem (Node a EmptyTree _) = a 
minElem (Node a left _ )  = minElem left 

countT EmptyTree c = c  
countT (Node a left right) c = 1+(countT left 0) + (countT right 0)

instance AbstractSet Set where
    empty = Set (EmptyTree)
    insert (Set bTree) x = Set $ insertT bTree x
    member (Set bTree) x = memberT bTree x
    delete (Set bTree) x = Set$ deleteT bTree x
    count (Set bTree)  = countT bTree 0
    
 
    