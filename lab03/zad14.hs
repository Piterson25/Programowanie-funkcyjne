myFoldl f e [] = e
myFoldl f e (x:xs) =
    myFoldl f (f e x) xs

myFoldr f e [] = e
myFoldr f e (x:xs) =
    f x (myFoldr f e xs)

myFlip f x y = f y x

-- a) prod
myProd n = myFoldl (*) 1 n

-- b) length
myLength n = myFoldl (\x _ -> x + 1) 0 n

-- c) and
myAnd n = myFoldl (&&) True n

-- d) gcd
myGcd n = myFoldl gcd 0 n

-- e) reverse
myReverse n = myFoldl (myFlip(:)) [] n

-- g) map
myMap f n = myFoldl f n
