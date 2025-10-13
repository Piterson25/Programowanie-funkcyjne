-- expR b e
expR b 0 = 1
expR b 1 = b
expR b e =
    if even e then (expR b (e `div` 2)) * (expR b (e `div` 2))
    else e * (expR b (e - 1))

-- expA b e
expA b e = expA_help b e 1
    where expA_help b 0 acc = acc
          expA_help b e acc =
              if even e then expA_help (b^2) (e `div` 2) acc
              else expA_help (b^2) (e `div` 2) (acc * b)

