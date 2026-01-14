mySum term next a b =
  if a > b
    then 0
    else (term a) + mySum term next (next a) b

-- a)
myProduct term next a b =
  if a > b
    then 1.0
    else term a * myProduct term next (next a) b

myProductInt term next a b =
  if a > b
    then 1
    else term a * myProductInt term next (next a) b

myFactorial n = myProduct (\x -> x) (+ 1) 1 n

myPiApprox nmax = 4.0 * myProduct term2 (+ 1) 1 nmax
  where
    term2 n = gora / dol
      where
        gora = (2 * n) * (2 * n)
        dol = (2 n - 1) * (2 n + 1)

-- b)
-- accumulate (+) 0 (\x -> x*x) 2 (+1) 3
accumulate combiner null_value term a next b =
  if a > b
    then null_value
    else combiner (term a) (accumulate combiner null_value term (next a) next b)

mySumA term a next b = accumulate (+) 0 term a next b

myProductA term a next b = accumulate (+) 1 term a next b

-- c)
-- filter_accumulate (\x -> x `mod` 2 == 0) (+) 0 (\x -> x*x) 2 (+1) 3
filter_accumulate pred combiner null_value term a next b =
  if a > b
    then null_value
    else
      if pred a
        then combiner (term a) (filter_accumulate pred combiner null_value term (next a) next b)
        else filter_accumulate pred combiner null_value term (next a) next b
