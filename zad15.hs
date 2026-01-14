myFoldr f e [] = e
myFoldr f e (x : xs) =
  f x (myFoldr f e xs)

insertionSort :: (a -> a -> Bool) -> [a] -> [a]
insertionSort f xs = myFoldr (insert f) [] xs
  where
    insert _ x [] = [x]
    insert f x (y : ys) =
      if f x y
        then x : y : ys
        else y : insert f x ys
