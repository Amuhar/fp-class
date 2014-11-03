module AbstractSet where

class AbstractSet a where
  member::(Eq t,Ord t) => a t -> t -> Bool
  insert::(Eq t,Ord t) => a t -> t -> a t
  empty:: a t
  delete:: (Eq t,Ord t) => a t -> t -> a t
  count:: a t -> Int