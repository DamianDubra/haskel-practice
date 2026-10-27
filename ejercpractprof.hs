--LAZY  

--1. Rangos infinitos
--Definí naturales, pares (con filter) y multiplosDe3, todas de tipo [Integer].

--(take 5 naturales, take 5 pares, take 5 multiplosDe3)
-- ([0,1,2,3,4],[0,2,4,6,8],[0,3,6,9,12])

natural :: [Integer]
natural = [0 ..]

pares :: [Integer]
pares = filter even natural

multiptres :: [Integer]
multiptres = filter (\x -> x `mod` 3 == 0) natural

--Definí miIterate, miRepeat y miCycle con el mismo comportamiento que las del Prelude.

--take 4 (miIterate (+ 3) 0)  -- [0,3,6,9]
--take 3 (miRepeat 'x')       -- "xxx"
--take 7 (miCycle [1, 2, 3])  -- [1,2,3,1,2,3,1]

miIterate :: (a -> a) -> a -> [a]
miIterate func a = a : miIterate func (func a)

miRepeat :: a -> [a]
miRepeat a = a : miRepeat a

miCycle :: [a] -> [a]
miCycle xs = xs ++ miCycle xs

---------------------------------------------------------------------------

--ORDEN SUPERIOR Y LAMBDA

{-1. Aplicar dos veces
aplicarDosVeces :: (a -> a) -> a -> a

aplicarDosVeces (\x -> x * 3) 2  -- 18
aplicarDosVeces (++ "!") "hola"  -- "hola!!"-}

aplicarDosVeces :: (a -> a) -> a -> a
aplicarDosVeces func a = func (func a)

{-2. Reimplementar map y filter
Escribí miMap :: (a -> b) -> [a] -> [b] y miFilter :: (a -> Bool) -> [a] -> [a] con recursión, sin usar las del Prelude.

miMap (+ 1) [1, 2, 3]   -- [2,3,4]
miFilter odd [1 .. 10]  -- [1,3,5,7,9]-}

miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap func (x:xs) = func x : miMap func xs

miFilter :: (a -> Bool) ->  [a] -> [a]
miFilter _ [] = []
miFilter func (x:xs)
    |func x == True = x: miFilter func xs
    |otherwise = miFilter func xs

{-3. Composición
sumaCuadradosPares :: [Int] -> Int: suma de los cuadrados de los números pares.

sumaCuadradosPares [1 .. 6]  -- 56   (4 + 16 + 36)-}

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares [] = 0
sumaCuadradosPares (x:xs) = if (even x ) then x*x + (sumaCuadradosPares xs) else (sumaCuadradosPares xs)
-- se puede hacer con guardas , con sum 

{-4. Componer una lista de funciones
componer :: [a -> a] -> (a -> a). La lista [f, g, h] tiene que comportarse como f . g . h. Pista: foldr e id.

componer [(+ 1), (* 2)] 5  -- 11
componer [(* 2), (+ 1)] 5  -- 12-}

componer :: [a -> a] -> (a -> a)
componer [] = id
componer (x:xs) = x . componer xs
{- OPCION 2
componer :: [a -> a] -> (a -> a)
componer = foldr (.) id-}

{-6. Contar los que cumplen
contarSi :: (a -> Bool) -> [a] -> Int

contarSi even [1 .. 10]     -- 5
contarSi (> 'm') "haskell"  -- 1-}

contarSi :: (a -> Bool) -> [a] -> Int
contarSi _ [] = 0
contarSi func (x:xs) 
    |func x == True = (contarSi func xs) +1
    | otherwise = contarSi func xs

{-7. Lista de funciones, un argumento
aplicarATodos :: [a -> b] -> a -> [b]. Escribila dos veces: aplicarATodos con una lambda y aplicarATodos' usando la aplicacion parcial de "$" ($ x).

aplicarATodos [(+ 1), (* 2), (^ 2)] 5  -- [6,10,25]-}

aplicarATodos :: [a -> b] -> a -> [b]
aplicarATodos [] _ = []
aplicarATodos xs a = map (\x-> x a) xs

aplicarATodos' :: [a -> b] -> a -> [b]
aplicarATodos' [] _ = []
aplicarATodos' xs a = map ($ a) xs

{-8. Producto escalar
productoEscalar :: [Double] -> [Double] -> Double, usando zipWith.

productoEscalar [1, 2, 3] [4, 5, 6]  -- 32.0-}
productoEscalar :: [Double] -> [Double] -> Double
productoEscalar _ [] = 0.0
productoEscalar [] _ = 0.0
productoEscalar xs1 xs2 = sum (zipWith (*) xs1 xs2)
--zipwith toma una funcion y la e¿aplica entre los elementos de una lista , [primer elemento xs1 * primer elemento xs2 , segundo elemento xs1 * segundo elemento xs2] 



---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- PATTER MACHINE


