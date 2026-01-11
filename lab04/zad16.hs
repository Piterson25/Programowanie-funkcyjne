--Please define a function iter f n for a one-argument function f and a natural number n. The value of iter f n is a function computing fn.

iter f 0 = id
iter f n = f . iter f (n - 1)

{-
> iter square 2 381
81
> let f = iter square 2 in f 5
625
> iter square 0 7
7
-}

