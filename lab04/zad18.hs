-- a) manifest types
type Compl = (Char, [Float])

plus_c :: Compl -> Compl -> Compl
plus_c z1 z2 =
  make_rectangular ((real_part z1) + (real_part z2))
  ((imag_part z1) + (imag_part z2))

times_c :: (Compl, Compl) -> Compl
times_c (z1, z2) =
  make_polar ((magnitude z1) * (magnitude z2))
  ((angle z1) + (angle z2))

make_rectangular :: Float -> Float -> Compl
make_rectangular x y = ('r', [x, y])

make_polar :: Float -> Float -> Compl
make_polar x y = ('p', [x, y])

real_part :: Compl -> Float
real_part ('r', [a, b]) = a
real_part ('p', [a, b]) = a * (cos b)

imag_part :: Compl -> Float
imag_part ('r', [a, b]) = b
imag_part ('p', [a, b]) = a * (sin b)

magnitude :: Compl -> Float
magnitude ('r', [a, b]) = a * a + b * b
magnitude ('p', [a, b]) = a

angle :: Compl -> Float
angle ('r', [a, b]) = (atan b) * a
angle ('p', [a, b]) = b
-- times_c ((make_rectangular 1 2), (make_polar 3 4))

-- b) message passing
mpPlus_c z1 z2 =
  mpMake_rectangular ((mpReal_part z1) + (mpReal_part z2))
  ((mpImag_part z1) + (mpImag_part z2))

mpTimes_c (z1, z2) =
  mpMake_polar ((mpMagnitude z1) * (mpMagnitude z2))
  ((mpAngle z1) + (mpAngle z2))

mpMake_polar a b =
  \m -> (case m of
              "real_part" -> a * cos b
              "imag_part" -> a * sin b
              "angle" -> a
              "magnitude" -> b
              )

mpMake_rectangular a b =
  \m -> (case m of
              "real_part" -> a
              "imag_part" -> a
              "angle" -> (atan b) * a
              "magnitude" -> a * a + b * b
              )
mpReal_part z = z "real_part"
mpImag_part z = z "imag_part"
mpAngle z = z "angle"
mpMagnitude z = z "magnitude"

-- c) angle (+c (make-rectangular -1 3) (make-polar 2 -2))
