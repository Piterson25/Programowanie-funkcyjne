-- a)   +
(+) :: Num a => a -> a -> a
-- b)   + 37
(+ 37) :: Num a => a -> a
-- c)   append
mappend :: Monoid a => a -> a -> a
-- d)   append [1,2]
mappend [1,2] :: Num a => [a] -> [a]
-- e)   map
map :: (a -> b) -> [a] -> [b]
-- f)   map square [1,2,3,4,5]
(map square [1,2,3,4,5]) :: Num b => [b]
-- g)   map square [['a']]
:t (map (\x -> x ++ x) [['a']])
(map (\x -> x ++ x) [['a']]) :: [[Char]]
-- h)   map length [['a']]
(map length [['a']]) :: [Int]
-- i)   foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- j)   foldl (++)
foldl (++) :: Foldable t => [a] -> t [a] -> [a]
-- k)   foldl (++) []
foldl (++) [] :: Foldable t => t [a] -> [a]
-- l)   f 7
f 7 :: Num a => a
-- m)   \f -> f 7
(\f -> f 7) :: Num t1 => (t1 -> t2) -> t2
-- n)   + (f x) (g x)
(+ (f x) (g x)) :: (Num a, Num t, Num (t -> a)) => a -> a
-- o)   f 7 (g 'x')
(f 7 (g 'x')) :: Num (Char -> t) => t
-- p)   \f -> f (g x)
(\f -> f (g 5)) :: (Char -> t) -> t
-- q)   (\f -> f (g x)) square 

