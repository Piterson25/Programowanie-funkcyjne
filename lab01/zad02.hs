gcd n 0 = n
gcd n m = Main.gcd m (n `mod` m)
-- gcd już istnieje w Haskellu

lcm n 0 = n
lcm n m = (n * m) `div` (Main.gcd n m)

