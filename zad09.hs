-- a) append l m
append [] l = l
append (x : xs) l = x : (append xs l)

-- b) member x l
member x [] = False
member x (y : ys) =
  if x == y
    then True
    else member x ys

-- c) reverse l
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x : xs) = myReverse xs ++ [x]

-- d) last l
myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

-- e) delete x l
myDelete x [] = []
myDelete x (y : ys) =
  if x == y
    then ys
    else [y] ++ myDelete x ys

-- f) pairing l1 l2
myPairing [] _ = []
myPairing _ [] = []
myPairing (s1 : l1) (s2 : l2) = (s1, s2) : myPairing l1 l2

-- g) split x l
split x [] = ([], [])
split x l = split_help x l [] []
  where
    split_help _ [] l1 l2 = (l1, l2)
    split_help x (y : ys) l1 l2 =
      if x < y
        then split_help x ys l1 (y : l2)
        else
          if x > y
            then split_help x ys (y : l1) l2
            else split_help x ys l1 l2

-- h) map f l
myMap f [] = []
myMap f (x : xs) = (f x) : (myMap f xs)

-- ++ łączy listy
-- () - grupowanie i rozdzielanie, krotki
-- where - zmienne i funkcje lokalne wewnątrz innych
