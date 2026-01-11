data Tree a = Leaf a | Node (Tree a) (Tree a)

class MyFunctor c where
  cmap :: (a -> b) -> c a -> c b

instance MyFunctor [] where
  cmap f [] = []
  cmap f (x : xs) = (f x) : (cmap f xs)

instance MyFunctor Tree where
  cmap f (Leaf x) = Leaf (f x)
  cmap f (Node l r) = Node (cmap f l) (cmap f r)
