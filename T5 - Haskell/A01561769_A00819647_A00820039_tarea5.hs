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

--  Función recursiva que cuenta la cantidad de números primos
--  que existen en un rango definido por dos argumentos.
--  a = limite inferior del rango
--  b = limite superior del rango
primos :: Float -> Float -> Int
primos a b
    | a > b = 0
    | a == b = 
        if verPrim a (a - 1) then
            1
        else
            0
    | otherwise =
        if verPrim a (a - 1) then
            1 + primos (a + 1) b
        else
            0 + primos (a + 1) b

--  Función recursiva que verifica si un valor es primo
--  haciendo la division por cada valor previo
--  a = valor a dividir
--  b = divisor
verPrim :: Float -> Float -> Bool
verPrim a b
    | a == 0 = False 
    | b == 1 = True
    | verFloat (a / b) = verPrim a (b - 1)
    | otherwise = False

--  Función para verificar si un valor es Float
--  x = valor a verificar
verFloat :: Float -> Bool
verFloat x = x /= fromInteger (round x)





main = do
    print "1. - Medio"
    print (medio 2 1 5 4)
    print (medio 2 2 2 2)
    print "2. - Primos"
    print (primos 1 10)
    print (primos 5 11)
    print (primos 8 10)
    print "3. - Mayores"
