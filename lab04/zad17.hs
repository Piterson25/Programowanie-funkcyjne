dc :: (p -> Bool) -> (p -> r) -> (p -> [p]) -> ([r] -> r) -> p -> r
dc test end divide combine p =
  if test p
    then end p
    else
      combine
        (map (dc test end divide combine) (divide p))

split :: [a] -> [[a]]
split xs = [take n xs, drop n xs]
  where
    n = length xs `div` 2

myMerge :: (Ord a) => [[a]] -> [a]
myMerge [l1, l2] = combine l1 l2
  where
    combine [] ys = ys
    combine xs [] = xs
    combine (x : xs) (y : ys) =
      if x <= y then x : combine xs (y : ys) else y : combine (x : xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort =
  dc
    (\xs -> length xs <= 1)
    id
    split
    myMerge
