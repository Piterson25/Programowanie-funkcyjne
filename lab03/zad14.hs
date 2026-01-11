myFoldl f e [] = e
myFoldl f e (x : xs) =
  myFoldl f (f e x) xs

myFoldr f e [] = e
myFoldr f e (x : xs) =
  f x (myFoldr f e xs)

myFlip f x y = f y x

-- a) prod
myProd = myFoldl (*) 1

-- b) length
myLength = myFoldl (\x _ -> x + 1) 0

-- c) and
myAnd = myFoldl (&&) True

-- d) gcd
myGcd = myFoldl gcd 0

-- e) reverse
myReverse = myFoldl (myFlip (:)) []

-- f) delete
myDelete e = myFoldr (\x acc -> if x == e then acc else x : acc) []

-- g) map
myMap f = myFoldr (\x acc -> f x : acc) []

-- h) filter
myFilter p = myFoldr (\x acc -> if p x then x : acc else acc) []

-- i) forall
myForall p = myFoldr (\x acc -> p x && acc) True
