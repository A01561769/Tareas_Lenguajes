--  A00820039 Mariano Hurtado de Mendoza Carranza
--  A00819647 Carlos Eduardo Govea Gonzales
--  A01561769 Antonio Torres Carvajal

-- ------------ Programación básica y recursiva SIN LISTAS ------------
--  Función que promedia  los  2  valores  extremos  (el  menor  y
--  el mayor) en los 4 argumentos de la función.
--  a, b, c, d = parametros numericos
medio :: Float  -> Float -> Float -> Float -> Float
medio 0 0 0 0 = 0
medio a b c d = (menor a b c d + mayor a b c d) / 2

--  Función auxiliar que busca el valor menor de 4 valores
--  a, b, c, d = parametros numericos
menor :: Float -> Float -> Float -> Float -> Float
menor a b c d
  | a > b = menor b c d a
  | a > c = menor c d a b
  | a > d = menor d a b c
  | otherwise = a

--  Función auxiliar que busca el valor mayor de 4 valores
--  a, b, c, d = parametros numericos
mayor :: Float -> Float -> Float -> Float -> Float
mayor a b c d
  | a < b = mayor b c d a
  | a < c = mayor c d a b
  | a < d = mayor d a b c
  | otherwise = a

main = do
    print "1. -"
    print (medio 2 1 5 4)
    print (medio 2 2 2 2)
    print "2. -"
