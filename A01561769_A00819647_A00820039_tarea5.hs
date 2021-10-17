-- medio
medio :: Float  -> Float -> Float -> Float -> Float
medio 0 0 0 0 = 0
medio a b c d = (menor a b c d + mayor a b c d) / 2

menor :: Float -> Float -> Float -> Float -> Float
menor a b c d
  | a > b = menor b c d a
  | a > c = menor c d a b
  | a > d = menor d a b c
  | otherwise = a

mayor :: Float -> Float -> Float -> Float -> Float
mayor a b c d
  | a < b = mayor b c d a
  | a < c = mayor c d a b
  | a < d = mayor d a b c
  | otherwise = a

main = do
    print "Hola"