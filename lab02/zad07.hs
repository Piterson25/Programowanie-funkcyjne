fibR 0 = 0
fibR 1 = 1
fibR n =
  fibR (n - 1) + fibR (n - 2)

fibA n = fibA_help n 0 1
  where
    fibA_help 0 acc acc2 = acc
    fibA_help n acc acc2 = fibA_help (n - 1) acc2 (acc + acc2)
