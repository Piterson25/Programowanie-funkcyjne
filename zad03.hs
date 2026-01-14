smaller x y = x < y

greater x y = y < x

equal x y = not (smaller x y) && not (greater x y)

not_equal x y = not (equal x y)

smaller_equal x y = smaller x y || equal x y

greater_equal x y = greater x y || equal x y
