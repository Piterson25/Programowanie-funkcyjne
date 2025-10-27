insertionsort :: (a -> a -> Bool) -> [a] -> [a]
insertionsort _ [] = []
insertionsort f (a:as) = add a (insertionsort f as)
    where 
        add elem [] = [elem] 
        add elem (l:ls) =
            if f elem l then l : add elem ls -- greater elem l
            else elem : l : ls

