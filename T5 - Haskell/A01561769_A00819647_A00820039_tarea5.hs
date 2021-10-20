import System.Directory.Internal.Prelude (Num(fromInteger))
--  A00820039 Mariano Hurtado de Mendoza Carranza
--  A00819647 Carlos Eduardo Govea Gonzales
--  A01561769 Antonio Torres Carvajal

--  ------------ Programación básica y recursiva SIN LISTAS ------------
--  Función que promedia  los  2  valores  extremos  (el  menor  y
--  el mayor) en los 4 argumentos de la función.
medio :: Float  -> Float -> Float -> Float -> Float
medio 0 0 0 0 = 0
medio a b c d = (menor a b c d + mayor a b c d) / 2

--  Función auxiliar que busca el valor menor de 4 valores
menor :: Float -> Float -> Float -> Float -> Float
menor a b c d
  | a > b = menor b c d a
  | a > c = menor c d a b
  | a > d = menor d a b c
  | otherwise = a

--  Función auxiliar que busca el valor mayor de 4 valores
mayor :: Float -> Float -> Float -> Float -> Float
mayor a b c d
  | a < b = mayor b c d a
  | a < c = mayor c d a b
  | a < d = mayor d a b c
  | otherwise = a

--  Función recursiva que cuenta la cantidad de números primos
--  que existen en un rango definido por dos argumentos.
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
verPrim :: Float -> Float -> Bool
verPrim a b
    | a == 0 = False
    | b == 1 = True
    | verFloat (a / b) = verPrim a (b - 1)
    | otherwise = False

--  Función para verificar si un valor es Float
verFloat :: Float -> Bool
verFloat x = x /= fromInteger (round x)

--  --------------- Listas y empatamiento de patrones ---------------
--  Función que compara los elementos de dos listas del mismo
--  tamaño para regresar una lista que indique en cuálde las 2 listas
--  se encuentra el mayor de cada posición.
mayores :: [Int] -> [Int] -> [Int]
mayores [] [] = []
mayores (x:resto1) (y:resto2) =
    if x >= y then
        1:mayores resto1 resto2
    else
        2:mayores resto1 resto2

--  Función que obtiene una lista de 1’s que representa el
--  resultado en unario de multiplicar dos enteros no
--  negativos en unario dados como listas de unos.
multiplica :: [Int] -> [Int] -> [Int]
multiplica lista1 [] = []
multiplica [] lista2 = []
multiplica lista1 lista2 =
    creaResp (length lista1 * length lista2)

creaResp :: Int -> [Int]
creaResp 0 = []
creaResp x = 1:creaResp (x - 1)

--  Función que reciba una lista de cualquier tamaño y un entero
--  no negativo N, y regrese la misma lista a la cual se le han
--  aplicado  N desplazamientos circulares hacia la derecha.

desplaza :: [Int] -> Int -> [Int]
desplaza list 0 = list
desplaza list n =  desplaza (last list : init list) (n-1) 

-- Un Árbol Binario (AB) puede ser representado en Haskell,
-- por medio de la siguiente declaración: 
data AB t = A (AB t) t (AB t) | V deriving (Show)

-- Función  impares  que  dado  un  árbol  binario,  cree  una  
-- lista  con  los valores impares de sus nodos. Recorrer el árbol 
-- en preorden al buscar los valores. 

impares :: AB Int -> [Int]
impares V = []
impares (A l n r) = if odd n then [n] ++ impares l ++ impares r else impares l ++ impares r

-- Función  intercambia  que  dado  un  árbol  binario  intercambie  los 
-- subárboles izquierdos de los nodos con los subárboles derechos.

intercambia :: AB Int -> AB Int
intercambia V = V
intercambia (A l n r) = (A (intercambia r) n (intercambia l))

-- Función c_tabla que dado un número n regresa una lista con 
-- las tablas de multiplicar de n con comprensión de listas
c_tabla :: Int -> [((Int, Int), Int)]
c_tabla n = [((n, y), n * y) | y <-[1..10]]

main = do
    print "1. - Medio"
    print (medio 2 1 5 4)
    print (medio 2 2 2 2)
    
    print "2. - Primos"
    print (primos 1 10)
    print (primos 5 11)
    print (primos 8 10)

    print "3. - Mayores"
    print (mayores [8,5,2,4] [1,2,3,4])
    print (mayores [1,2,3] [2,3,1])

    print "4. - Multiplica"
    print (multiplica [1,1] [1,1,1])
    print (multiplica [1,1,1] [])
    print (multiplica [1,1,1,1] [1,1])

    print "5. - Desplaza"
    print (desplaza [1,2,3] 1)
    print (desplaza [1,2,3] 2)
    print (desplaza [1,2,3] 6)

    print "6. - Impares"
    print(impares (V))
    print(impares (A (A (A V 2 V) 5 (A V 7 V)) 8 (A V 9 (A (A V 11 V) 15 V))))
    print(impares (A (A (A V 3 V) 2 V) 1 (A (A V 5 V) 4 V)))

    print "7. - intercambia"
    print(intercambia V )
    print(intercambia (A (A (A V 2 V) 5 (A V 7 V)) 8 (A V 9 (A (A V 11 V) 15 V))))
    print(intercambia (A (A (A V 3 V) 2 V) 1 (A (A V 5 V) 4 V)))
    
    print "9. - c_tabla"
    print(c_tabla 1)
    print(c_tabla 8)

