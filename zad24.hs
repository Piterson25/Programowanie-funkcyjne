-- a)
position :: (Eq a) => a -> [a] -> Maybe Int
position _ [] = Nothing
position x (y : ys) =
  if x == y
    then Just 0
    else case position x ys of
      Nothing -> Nothing
      Just n -> Just (n + 1)

-- b)
dropMaybe :: Int -> [a] -> Maybe [a]
dropMaybe n l | n <= 0 = Just l
dropMaybe _ [] = Nothing
dropMaybe n (_ : xs) = dropMaybe (n - 1) xs

-- c)
sumMaybe :: (Num a) => [Maybe a] -> a
sumMaybe [] = 0
sumMaybe (Nothing : xs) = sumMaybe xs
sumMaybe (Just x : xs) = x + sumMaybe xs
