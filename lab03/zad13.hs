-- e - wartosc poczatkowa
myFoldl f e [] = e
myFoldl f e (x : xs) =
  myFoldl f (f e x) xs

myFoldR f e [] = e
myFoldR f e (x : xs) =
  f x (myFoldR f e xs)
