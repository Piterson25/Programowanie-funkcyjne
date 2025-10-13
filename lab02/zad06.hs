roots a b c = 
    let delta = b^2 - 4 * a * c
    in if delta < 0 then []
    else if delta == 0 then (-b / 2 * a) : []
    else [((-b + sqrt delta) / (2 * a)), ((-b - sqrt delta) / (2 * a))]

