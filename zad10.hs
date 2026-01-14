-- a)
mySum a b = a + b

map2 f [] [] = []
map2 f (x : xs) (y : ys) = (f x y) : (map2 f xs ys)

-- b)
mySmaller a = a < 3

myFilter p [] = []
myFilter p (x : xs) =
  if p x
    then x : (myFilter p xs)
    else myFilter p xs

-- c)
take_while p [] = []
take_while p (x : xs) =
  if p x
    then x : (take_while p xs)
    else []

-- d)
groups [] = []
groups (x : xs) = groups_help xs [x] []
  where
    groups_help [] curr_acc acc = reverse (curr_acc : acc)
    groups_help (x : xs) (c : cs) acc =
      if x == c
        then groups_help xs (x : (c : cs)) acc
        else groups_help xs [x] ((c : cs) : acc)

groups :: [Int] -> [[Int]]

groups2 [] = []
groups2 (x : xs) = groups_help xs [x]
  where
    groups_help :: [Int] -> [Int] -> [[Int]]
    groups_help [] acc = [acc]
    groups_help (x : xs) (a : ac) =
      if x == a
        then groups_help xs (x : (a : ac))
        else (a : ac) : (groups_help xs [x])
