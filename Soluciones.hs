module Soluciones where

import Control.Exception (SomeException, evaluate, try)

{-
================================================================================
  SOLUCIONES — GUÍA DE PRÁCTICA PARA EL PARCIAL — PARADIGMA FUNCIONAL (Haskell)
================================================================================

  Cómo usarla:
    1. Abrí la consola en esta carpeta y corré:   ghci Soluciones.hs
    2. Reemplazá cada `undefined` por tu solución.
    3. Probá un nivel con:     probar nivel1      (o nivel2, ..., nivel7)
       Probá todo con:         probarTodo
       Recargá después de editar con:  :r

  Cada caso de prueba sale como:
    OK         -> anda
    FALLA      -> devuelve otra cosa
    PENDIENTE  -> todavía tiene `undefined` (o tira error)

  Ojo: si en el nivel 6 o 7 una prueba se queda colgada, probablemente
  estás recorriendo una lista infinita entera. Cortá con Ctrl+C.

  Los niveles van de menor a mayor dificultad. Los marcados con (*)
  son desafíos: si salen, vas sobrada.
================================================================================
-}

--------------------------------------------------------------------------------
-- NIVEL 0 — Calentamiento: encontrá el error
--------------------------------------------------------------------------------
{-
  Estas líneas salen de tu práctica. Ninguna compila (o hace algo raro).
  Antes de mirar soluciones, explicá por qué y cómo se arregla.
  En el parcial escrito, este tipo de error te puede costar el ejercicio.

  a) agregaralfinal x [xs] = x ++ xs
  b) pares xs = filter (\x -> x ´mod´ 2 == 0) xs
  c) hayPar xs = any (\x -> x 'mod' 2 == 0) xs
  d) perimetro (circulo radio) = 2 * 3.14 * radio
     primetro (rectangulo b a) = 2 * (b + a)
  e) class Describible where
         description :: a -> String
  f) instance Eq Estacion where
         Verano == Verano True
  g) multiplosDeTres = [3,6,...]
     take 10 multiplosDeTres          -- escrito suelto en el archivo
  h) todosPositivos [] = error "lista vacia"
     todosPositivos xs = all (\x -> x > 0) xs
     -- ¿Qué devuelve `all` con lista vacía? ¿Hace falta ese caso?
  i) mayoresADiez xs = filter (\x -> x > 10 == True) xs   -- ¿qué sobra?
  j) Definir `data Color = ...` dos veces en el mismo archivo.
-}
{- RESPUESTAS NIVEL 0

  a) Dos problemas: el patrón [xs] sólo matchea listas de UN elemento, y
     (++) concatena dos listas, pero x es un elemento.
       agregarAlFinal :: a -> [a] -> [a]
       agregarAlFinal x xs = xs ++ [x]

  b) y c) Para usar una función de 2 parámetros como operador infijo se usan
     BACKTICKS (acento grave): x `mod` 2. Ni ´ (acento agudo) ni ' (comilla
     simple, que es para Char).

  d) Los constructores van con MAYÚSCULA (Circulo, Rectangulo); en minúscula
     Haskell los toma como variables. Además "primetro" es otra función
     (typo), así que perimetro queda sin el caso Rectangulo.

  e) Falta la variable de tipo de la clase:  class Describible a where

  f) Falta el `=`:  Verano == Verano = True   (y al final  _ == _ = False)

  g) El rango infinito es con DOS puntos: [3,6..]. Y en un .hs sólo puede
     haber definiciones: `take 10 multiplosDeTres` se escribe en ghci, o
     dentro de una definición:  primeros10 = take 10 multiplosDeTres

  h) all p [] == True (no hay ningún elemento que NO cumpla). Ese caso
     sobra y además rompe un resultado que tendría que ser True.
     Mejor todavía, sin parámetro:  todosPositivos = all (> 0)

  i) `x > 10 == True` es igual a `x > 10`. Comparar un Bool con True
     nunca hace falta:  mayoresADiez = filter (> 10)

  j) Error "Multiple declarations": cada tipo (y cada constructor) se puede
     definir una sola vez por módulo.
-}

--------------------------------------------------------------------------------
-- NIVEL 1 — Recursión sobre listas
--   Resolvelos con pattern matching y recursión, SIN usar maximum, reverse,
--   elem, concat, etc. (sí podés usar filter donde se indica).
--------------------------------------------------------------------------------

-- 1.1 El mayor elemento. Con lista vacía, error.
maximo :: [Int] -> Int
maximo [] = error "lista vacia"
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

-- 1.2 Dar vuelta una lista.
reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- 1.3 ¿Está el elemento en la lista?  (fijate la restricción Eq a)
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- 1.4 Saca SÓLO la primera aparición del elemento.
--     sacarUna 2 [1,2,3,2] == [1,3,2]
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna _ [] = []
sacarUna e (x:xs)
  | e == x    = xs
  | otherwise = x : sacarUna e xs

-- 1.5 Elimina repetidos conservando la primera aparición. Podés usar filter.
--     sinRepetidos [1,2,1,3,2] == [1,2,3]
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

-- 1.6 Intercala dos listas. Si una se termina, sigue con la otra.
--     intercalar [1,3,5] [2,4] == [1,2,3,4,5]
intercalar :: [a] -> [a] -> [a]
intercalar [] ys = ys
intercalar xs [] = xs
intercalar (x:xs) (y:ys) = x : y : intercalar xs ys

-- 1.7 Aplanar una lista de listas.  aplanar [[1,2],[],[3]] == [1,2,3]
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

-- 1.8 Posiciones (desde 0) donde aparece el elemento.
--     posicionesDe 'a' "banana" == [1,3,5]
--     Pista: una función auxiliar que lleve el índice actual.
posicionesDe :: Eq a => a -> [a] -> [Int]
posicionesDe e = desdeIndice 0
  where
    desdeIndice _ [] = []
    desdeIndice i (x:xs)
      | x == e    = i : desdeIndice (i + 1) xs
      | otherwise = desdeIndice (i + 1) xs

-- 1.9 (*) Partir en grupos de n.  partirEn 2 [1..5] == [[1,2],[3,4],[5]]
--     Acá sí podés usar take y drop.
partirEn :: Int -> [a] -> [[a]]
partirEn _ [] = []
partirEn n xs = take n xs : partirEn n (drop n xs)

nivel1 :: [(String, Bool)]
nivel1 =
  [ ("maximo [3,9,2]",               maximo [3,9,2] == 9)
  , ("maximo [-5]",                  maximo [-5] == -5)
  , ("reverso [1,2,3]",              reverso [1,2,3 :: Int] == [3,2,1])
  , ("reverso []",                   reverso ([] :: [Int]) == [])
  , ("pertenece 3 [1,2,3]",          pertenece 3 [1,2,3 :: Int])
  , ("pertenece 'z' \"hola\"",       not (pertenece 'z' "hola"))
  , ("sacarUna 2 [1,2,3,2]",         sacarUna 2 [1,2,3,2 :: Int] == [1,3,2])
  , ("sacarUna 9 [1,2]",             sacarUna 9 [1,2 :: Int] == [1,2])
  , ("sinRepetidos [1,2,1,3,2]",     sinRepetidos [1,2,1,3,2 :: Int] == [1,2,3])
  , ("intercalar [1,3,5] [2,4]",     intercalar [1,3,5] [2,4 :: Int] == [1,2,3,4,5])
  , ("intercalar \"ac\" \"bdef\"",   intercalar "ac" "bdef" == "abcdef")
  , ("aplanar [[1,2],[],[3]]",       aplanar [[1,2],[],[3 :: Int]] == [1,2,3])
  , ("posicionesDe 'a' \"banana\"",  posicionesDe 'a' "banana" == [1,3,5])
  , ("posicionesDe 'x' \"banana\"",  posicionesDe 'x' "banana" == [])
  , ("partirEn 2 [1..5]",            partirEn 2 [1..5 :: Int] == [[1,2],[3,4],[5]])
  ]

--------------------------------------------------------------------------------
-- NIVEL 2 — Orden superior, composición y aplicación parcial
--------------------------------------------------------------------------------

-- 2.1 Tu propio filter, con recursión.
miFilter :: (a -> Bool) -> [a] -> [a]
miFilter _ [] = []
miFilter p (x:xs)
  | p x       = x : miFilter p xs
  | otherwise = miFilter p xs

-- 2.2 Cuántos elementos cumplen. Escribila en UNA línea, con composición (.)
--     cuantosCumplen even [1,2,3,4] == 2
cuantosCumplen :: (a -> Bool) -> [a] -> Int
cuantosCumplen p = length . filter p

-- 2.3 Aplicar una función n veces.  aplicarNVeces 3 (*2) 1 == 8
aplicarNVeces :: Int -> (a -> a) -> a -> a
aplicarNVeces 0 _ x = x
aplicarNVeces n f x = aplicarNVeces (n - 1) f (f x)

-- 2.4 Aplicar una lista de funciones EN ORDEN, de izquierda a derecha.
--     componerTodas [(+1), (*2)] 5 == 12      -- primero +1 -> 6, después *2 -> 12
--     componerTodas [] 5 == 5
componerTodas :: [a -> a] -> a -> a
componerTodas [] = id
componerTodas (f:fs) = componerTodas fs . f

-- 2.5 El elemento que maximiza un criterio. Si hay empate, el primero.
--     mejorSegun length ["hola","a","chau!"] == "chau!"
--     mejorSegun negate [3,1,2] == 1
--     Esta la vas a reusar en el nivel 7.
mejorSegun :: Ord b => (a -> b) -> [a] -> a
mejorSegun _ [] = error "lista vacia"
mejorSegun _ [x] = x
mejorSegun f (x:xs)
  | f x >= f mejorResto = x
  | otherwise           = mejorResto
  where mejorResto = mejorSegun f xs

-- 2.6 Point-free (sin nombrar la lista): suma de los cuadrados de los pares.
--     sumaDeCuadradosDePares [1,2,3,4] == 20
sumaDeCuadradosDePares :: [Int] -> Int
sumaDeCuadradosDePares = sum . map (^ 2) . filter even

-- 2.7 Point-free: las longitudes de las palabras que tienen más de n letras.
--     largosMayoresA 3 ["sol","casa","a","perro"] == [4,5]
largosMayoresA :: Int -> [String] -> [Int]
largosMayoresA n = filter (> n) . map length

{-
  2.8 Teórico (sin ghci primero, después chequeá con :t):
      ¿Cuál es el tipo de cada expresión?
      a) cuantosCumplen even
      b) map (*2) . filter odd
      c) misterio f g = filter f . map g        -- tipo de misterio
      d) flip take
      e) ¿Por qué `cuantosCumplen p = length . filter p` funciona aunque
         "le falte" la lista? ¿Qué es la aplicación parcial?
-}
{- RESPUESTAS 2.8
  a) Integral a => [a] -> Int
  b) Integral a => [a] -> [a]
  c) misterio :: (b -> Bool) -> (a -> b) -> [a] -> [b]
     (primero map g transforma a's en b's, después filter f filtra b's)
  d) [a] -> Int -> [a]
  e) `filter p` ya es una función [a] -> [a] (le pasamos sólo 1 de sus 2
     parámetros: aplicación parcial). Componerla con length da otra función
     [a] -> Int. Definir cuantosCumplen p = (algo de tipo [a] -> Int) es
     exactamente lo que pide la firma (a -> Bool) -> [a] -> Int.
-}

nivel2 :: [(String, Bool)]
nivel2 =
  [ ("miFilter even [1..6]",                   miFilter even [1..6 :: Int] == [2,4,6])
  , ("miFilter (>9) [1,2]",                    miFilter (> 9) [1,2 :: Int] == [])
  , ("cuantosCumplen even [1,2,3,4]",          cuantosCumplen even [1,2,3,4 :: Int] == 2)
  , ("cuantosCumplen (=='a') \"banana\"",      cuantosCumplen (== 'a') "banana" == 3)
  , ("aplicarNVeces 3 (*2) 1",                 aplicarNVeces 3 (* 2) (1 :: Int) == 8)
  , ("aplicarNVeces 0 (+1) 7",                 aplicarNVeces 0 (+ 1) (7 :: Int) == 7)
  , ("componerTodas [(+1),(*2)] 5",            componerTodas [(+ 1), (* 2)] (5 :: Int) == 12)
  , ("componerTodas [] 5",                     componerTodas [] (5 :: Int) == 5)
  , ("componerTodas [reverse, take 2] \"hola\"", componerTodas [reverse, take 2] "hola" == "al")
  , ("mejorSegun length [...]",                mejorSegun length ["hola","a","chau!"] == "chau!")
  , ("mejorSegun negate [3,1,2]",              mejorSegun negate [3,1,2 :: Int] == 1)
  , ("mejorSegun empate -> el primero",        mejorSegun length ["ab","cd"] == "ab")
  , ("sumaDeCuadradosDePares [1,2,3,4]",       sumaDeCuadradosDePares [1,2,3,4] == 20)
  , ("largosMayoresA 3 [...]",                 largosMayoresA 3 ["sol","casa","a","perro"] == [4,5])
  ]

--------------------------------------------------------------------------------
-- NIVEL 3 — Folds
--   foldr f semilla lista   /   foldl f semilla lista
--   Resolvé cada uno con un fold, sin recursión explícita.
--------------------------------------------------------------------------------

-- 3.1
sumaFold :: [Int] -> Int
sumaFold = foldr (+) 0

-- 3.2 Pista: el elemento no te importa, sólo contás.
longitudFold :: [a] -> Int
longitudFold = foldr (\_ n -> n + 1) 0

-- 3.3 Usá foldr1 (no tiene semilla: arranca con un elemento de la lista).
maximoFold :: [Int] -> Int
maximoFold = foldr1 max

-- 3.4 Con foldl. Pista: el acumulador es la lista ya dada vuelta.
reversoFold :: [a] -> [a]
reversoFold = foldl (\acc x -> x : acc) []

-- 3.5 Lo mismo que componerTodas (2.4), pero con foldl.
--     Pista: la semilla es el valor inicial, y en cada paso aplicás una función.
componerConFold :: [a -> a] -> a -> a
componerConFold fs x = foldl (\acc f -> f acc) x fs

-- 3.6 cuantosCumplen, pero con foldr.
cuantosCumplenFold :: (a -> Bool) -> [a] -> Int
cuantosCumplenFold p = foldr (\x n -> if p x then n + 1 else n) 0

{-
  3.7 Teórico: resolvé A MANO, paso a paso, y después verificá en ghci.
      a) foldr (-) 0 [1,2,3]
      b) foldl (-) 0 [1,2,3]
      c) ¿Por qué dan distinto? ¿Con (+) darían distinto?
-}
{- RESPUESTAS 3.7
  a) foldr asocia a la derecha:  1 - (2 - (3 - 0)) = 1 - (2 - 3) = 1 - (-1) = 2
  b) foldl asocia a la izquierda: ((0 - 1) - 2) - 3 = -6
  c) La resta no es asociativa, así que importa cómo se agrupa. Con (+),
     que es asociativa y 0 es neutro, dan lo mismo.
-}

nivel3 :: [(String, Bool)]
nivel3 =
  [ ("sumaFold [1,2,3]",                 sumaFold [1,2,3] == 6)
  , ("sumaFold []",                      sumaFold [] == 0)
  , ("longitudFold \"hola\"",            longitudFold "hola" == 4)
  , ("maximoFold [3,9,2]",               maximoFold [3,9,2] == 9)
  , ("reversoFold [1,2,3]",              reversoFold [1,2,3 :: Int] == [3,2,1])
  , ("componerConFold [(+1),(*2)] 5",    componerConFold [(+ 1), (* 2)] (5 :: Int) == 12)
  , ("cuantosCumplenFold odd [1..5]",    cuantosCumplenFold odd [1..5 :: Int] == 3)
  ]

--------------------------------------------------------------------------------
-- NIVEL 4 — Árboles (ahora polimórficos: guardan cualquier tipo `a`)
--------------------------------------------------------------------------------

data Arbol a = Vacio
             | Nodo (Arbol a) a (Arbol a)     -- izquierdo, valor, derecho
  deriving (Show, Eq)

hoja :: a -> Arbol a
hoja v = Nodo Vacio v Vacio

--        5
--      /   \
--     3     8
--    /     / \
--   1     7   9
arbolEjemplo :: Arbol Int
arbolEjemplo = Nodo (Nodo (hoja 1) 3 Vacio) 5 (Nodo (hoja 7) 8 (hoja 9))

-- Dado (lo usan las pruebas):
inorder :: Arbol a -> [a]
inorder Vacio = []
inorder (Nodo i v d) = inorder i ++ [v] ++ inorder d

-- 4.1 Cantidad de nodos.
tamanio :: Arbol a -> Int
tamanio Vacio = 0
tamanio (Nodo i _ d) = 1 + tamanio i + tamanio d

-- 4.2 Altura: cantidad de niveles (Vacio tiene altura 0, una hoja 1).
altura :: Arbol a -> Int
altura Vacio = 0
altura (Nodo i _ d) = 1 + max (altura i) (altura d)

-- 4.3 Espejo: intercambia izquierdo y derecho en todos los niveles.
espejo :: Arbol a -> Arbol a
espejo Vacio = Vacio
espejo (Nodo i v d) = Nodo (espejo d) v (espejo i)

-- 4.4 map para árboles, ahora puede cambiar el tipo (a -> b).
mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol _ Vacio = Vacio
mapArbol f (Nodo i v d) = Nodo (mapArbol f i) (f v) (mapArbol f d)

-- 4.5 Valores de las hojas (nodos sin hijos), de izquierda a derecha.
--     hojas arbolEjemplo == [1,7,9]
hojas :: Arbol a -> [a]
hojas Vacio = []
hojas (Nodo Vacio v Vacio) = [v]
hojas (Nodo i _ d) = hojas i ++ hojas d

-- 4.6 Árbol binario de búsqueda (ABB): a la izquierda los menores,
--     a la derecha los mayores. Insertá respetando eso (si ya está, no cambia).
insertarABB :: Ord a => a -> Arbol a -> Arbol a
insertarABB x Vacio = hoja x
insertarABB x arbol@(Nodo i v d)
  | x < v     = Nodo (insertarABB x i) v d
  | x > v     = Nodo i v (insertarABB x d)
  | otherwise = arbol

-- 4.7 Armar un ABB insertando los elementos de la lista (con un fold).
--     inorder (desdeLista [5,3,8,1]) == [1,3,5,8]
desdeLista :: Ord a => [a] -> Arbol a
desdeLista = foldl (flip insertarABB) Vacio

-- 4.8 ¿Es un ABB? Pista: pensá qué tiene de especial su inorder.
esABB :: Ord a => Arbol a -> Bool
esABB = estaOrdenada . inorder
  where
    estaOrdenada xs = and (zipWith (<) xs (drop 1 xs))

-- 4.9 (*) El fold de los árboles: reemplaza Vacio por `semilla` y cada Nodo
--     por la función f (que recibe el resultado izquierdo, el valor y el
--     resultado derecho). Después definí sumaArbol usando foldArbol.
foldArbol :: (b -> a -> b -> b) -> b -> Arbol a -> b
foldArbol _ semilla Vacio = semilla
foldArbol f semilla (Nodo i v d) = f (foldArbol f semilla i) v (foldArbol f semilla d)

sumaArbol :: Arbol Int -> Int
sumaArbol = foldArbol (\i v d -> i + v + d) 0

nivel4 :: [(String, Bool)]
nivel4 =
  [ ("tamanio arbolEjemplo",            tamanio arbolEjemplo == 6)
  , ("tamanio Vacio",                   tamanio (Vacio :: Arbol Int) == 0)
  , ("altura arbolEjemplo",             altura arbolEjemplo == 3)
  , ("inorder (espejo arbolEjemplo)",   inorder (espejo arbolEjemplo) == [9,8,7,5,3,1])
  , ("mapArbol show arbolEjemplo",      inorder (mapArbol show arbolEjemplo) == ["1","3","5","7","8","9"])
  , ("hojas arbolEjemplo",              hojas arbolEjemplo == [1,7,9])
  , ("insertarABB 4 arbolEjemplo",      inorder (insertarABB 4 arbolEjemplo) == [1,3,4,5,7,8,9])
  , ("insertarABB repetido",            insertarABB 5 arbolEjemplo == arbolEjemplo)
  , ("desdeLista [5,3,8,1]",            inorder (desdeLista [5,3,8,1 :: Int]) == [1,3,5,8])
  , ("esABB arbolEjemplo",              esABB arbolEjemplo)
  , ("esABB arbol desordenado",         not (esABB (Nodo (hoja 10) 5 (hoja (1 :: Int)))))
  , ("sumaArbol arbolEjemplo",          sumaArbol arbolEjemplo == 33)
  ]

--------------------------------------------------------------------------------
-- NIVEL 5 — Typeclasses: class, instance, deriving
--------------------------------------------------------------------------------

data Palo = Espada | Basto | Oro | Copa
  deriving (Show, Eq)

-- Ojo: Carta NO deriva Show ni Ord. Las vas a implementar vos.
data Carta = Carta { numero :: Int, palo :: Palo }
  deriving Eq

-- 5.1 Valor de cada palo para desempatar: Espada 4, Basto 3, Oro 2, Copa 1.
valorPalo :: Palo -> Int
valorPalo Espada = 4
valorPalo Basto  = 3
valorPalo Oro    = 2
valorPalo Copa   = 1

-- 5.2 show (Carta 1 Espada) == "1 de Espada"
instance Show Carta where
  show (Carta n p) = show n ++ " de " ++ show p

-- 5.3 Una carta es mayor si tiene mayor número; si empatan, gana el palo
--     de mayor valorPalo. Alcanza con definir `compare` (o `<=`).
--     Pista: las tuplas ya saben compararse.
instance Ord Carta where
  compare c1 c2 = compare (clave c1) (clave c2)
    where clave c = (numero c, valorPalo (palo c))

-- 5.4 Una typeclass propia.
class Puntuable a where
  puntos :: a -> Int

-- Una carta vale su número, salvo 10, 11 y 12 que valen 0 (como en el envido).
instance Puntuable Carta where
  puntos (Carta n _)
    | n >= 10   = 0
    | otherwise = n

-- True vale 1, False vale 0.
instance Puntuable Bool where
  puntos True  = 1
  puntos False = 0

-- 5.5 (*) Una lista de cosas Puntuables es Puntuable: suma los puntos.
--     Fijate la restricción en la instancia: sólo vale si `a` es Puntuable.
instance Puntuable a => Puntuable [a] where
  puntos = sum . map puntos

-- 5.6 Ahora que Carta es Ord, esto sale gratis. ¿Por qué?
mejorCarta :: [Carta] -> Carta
mejorCarta = maximum

{-
  5.7 Teórico:
      a) ¿Qué diferencia hay entre `deriving (Show)` y escribir `instance Show`?
         ¿Cuándo conviene cada uno?
      b) ¿Por qué `maximum` funciona con [Carta] recién después de la
         instancia de Ord? Mirá su tipo con :t maximum
      c) ¿Cuál es el tipo de `puntos`? ¿Y de `puntos [True, False, True]`?
      d) Si Carta derivara Ord en vez de implementarla, ¿compilaría?
         (Pista: ¿Palo es Ord?)
-}
{- RESPUESTAS 5.7
  a) deriving genera la implementación automática "obvia" (Show muestra el
     constructor y sus campos tal cual; Eq compara campo por campo; Ord usa
     el orden en que escribiste los constructores). `instance` la escribís
     vos: conviene cuando el comportamiento automático no es el que querés
     (acá: "1 de Espada", o el orden por valorPalo).
  b) maximum :: (Foldable t, Ord a) => t a -> a. Exige Ord a: sin instancia
     de Ord para Carta no hay forma de saber cuál es mayor.
  c) puntos :: Puntuable a => a -> Int.  puntos [True,False,True] :: Int, y
     vale 2 (usa la instancia de listas, que usa la de Bool).
  d) No compila: derivar Ord en Carta necesita que todos sus campos sean Ord,
     y Palo sólo deriva Show y Eq. Habría que agregarle Ord a Palo, y en ese
     caso el orden sería Espada < Basto < Oro < Copa (el orden de escritura),
     que NO es el que pide el enunciado.
-}

nivel5 :: [(String, Bool)]
nivel5 =
  [ ("valorPalo Basto",                     valorPalo Basto == 3)
  , ("show (Carta 1 Espada)",               show (Carta 1 Espada) == "1 de Espada")
  , ("show [Carta 7 Oro]",                  show [Carta 7 Oro] == "[7 de Oro]")
  , ("Carta 3 Copa > Carta 2 Espada",       Carta 3 Copa > Carta 2 Espada)
  , ("Carta 7 Oro < Carta 7 Espada",        Carta 7 Oro < Carta 7 Espada)
  , ("puntos (Carta 7 Oro)",                puntos (Carta 7 Oro) == 7)
  , ("puntos (Carta 11 Copa)",              puntos (Carta 11 Copa) == 0)
  , ("puntos True",                         puntos True == 1)
  , ("puntos [Carta 7 Oro, 6 Oro, 12 Basto]", puntos [Carta 7 Oro, Carta 6 Oro, Carta 12 Basto] == 13)
  , ("puntos [True,False,True]",            puntos [True, False, True] == 2)
  , ("mejorCarta [...]",                    mejorCarta [Carta 4 Copa, Carta 12 Basto, Carta 12 Oro] == Carta 12 Basto)
  ]

--------------------------------------------------------------------------------
-- NIVEL 6 — Evaluación diferida y listas infinitas
--------------------------------------------------------------------------------

-- 6.1 Todos los pares naturales: 0, 2, 4, ...
pares :: [Int]
pares = [0, 2 ..]

-- 6.2 1, n, n^2, n^3, ...   Usá `iterate` (mirá su tipo con :t iterate).
potenciasDe :: Int -> [Int]
potenciasDe n = iterate (* n) 1

-- 6.3 ¿Es primo? (los menores a 2 no lo son)
esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\d -> n `mod` d /= 0) (takeWhile (\d -> d * d <= n) [2 ..])

-- 6.4 La lista infinita de primos.
primos :: [Int]
primos = filter esPrimo [2 ..]

-- 6.5 El primer primo estrictamente mayor a n. Una línea.
primerPrimoMayorA :: Int -> Int
primerPrimoMayorA n = head (filter (> n) primos)

-- 6.6 Secuencia de Collatz: si n es par -> n/2, si es impar -> 3n+1.
--     Termina cuando llega a 1 (el 1 se incluye).
--     collatz 6 == [6,3,10,5,16,8,4,2,1]
--     Pista: iterate + takeWhile.
siguienteCollatz :: Int -> Int
siguienteCollatz n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

collatz :: Int -> [Int]
collatz n = takeWhile (/= 1) (iterate siguienteCollatz n) ++ [1]

-- 6.7 (*) Fibonacci infinito: 0, 1, 1, 2, 3, 5, 8, ...
--     Pista: se puede definir en función de sí misma con zipWith.
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (drop 1 fibonacci)

{-
  6.8 Teórico: ¿Qué pasa con cada una? ¿Devuelve algo, se cuelga, o
      muestra algunos elementos y después se cuelga? Pensalo antes de probar.
      a) take 3 (map (*2) [1..])
      b) length [1..]
      c) head (filter (> 100) [1..])
      d) filter (< 5) [1..]
      e) any even [1..]
      f) all even [1..]
      g) all odd [1,3..]
      h) fst (1, undefined)
      i) length [undefined, undefined]
-}
{- RESPUESTAS 6.8
  a) [2,4,6]. map es perezoso: sólo calcula los 3 que pide take.
  b) Se cuelga: para contar tiene que llegar al final, y no hay final.
  c) 101. filter va generando y head se conforma con el primero.
  d) Muestra [1,2,3,4 y se cuelga: filter no "sabe" que no vienen más
     menores a 5, sigue buscando para siempre. (takeWhile (< 5) sí corta.)
  e) True: el 2 alcanza.
  f) False, y al toque: el 1 ya no es par, all corta en el primer fallo.
  g) Se cuelga: todos son impares, nunca encuentra un contraejemplo.
  h) 1. El segundo componente nunca se evalúa.
  i) 2. length cuenta las celdas de la lista, no evalúa los elementos.
-}

nivel6 :: [(String, Bool)]
nivel6 =
  [ ("take 4 pares",                 take 4 pares == [0,2,4,6])
  , ("take 5 (potenciasDe 2)",       take 5 (potenciasDe 2) == [1,2,4,8,16])
  , ("esPrimo 1 / 2 / 9 / 13",       map esPrimo [1,2,9,13] == [False,True,False,True])
  , ("take 6 primos",                take 6 primos == [2,3,5,7,11,13])
  , ("primerPrimoMayorA 20",         primerPrimoMayorA 20 == 23)
  , ("primerPrimoMayorA 23",         primerPrimoMayorA 23 == 29)
  , ("collatz 6",                    collatz 6 == [6,3,10,5,16,8,4,2,1])
  , ("collatz 1",                    collatz 1 == [1])
  , ("take 8 fibonacci",             take 8 fibonacci == [0,1,1,2,3,5,8,13])
  ]

--------------------------------------------------------------------------------
-- NIVEL 7 — INTEGRADOR estilo parcial: "La Mazmorra"
--------------------------------------------------------------------------------
{-
  Modelamos aventureros que atraviesan mazmorras. Una mazmorra es una
  secuencia de EVENTOS, y cada evento es una FUNCIÓN que transforma al
  aventurero. (Este "las cosas que pasan son funciones" es MUY de parcial.)

  Pista de sintaxis: con records podés crear una copia modificada así:
      a { oro = oro a + 10 }
-}

data Aventurero = Aventurero
  { nombreA    :: String
  , vida       :: Int
  , oro        :: Int
  , inventario :: [String]
  } deriving (Show, Eq)

type Evento = Aventurero -> Aventurero

cande, tomi, lu :: Aventurero
cande = Aventurero "Cande" 100 50 ["pocion", "espada"]
tomi  = Aventurero "Tomi"   30 200 []
lu    = Aventurero "Lu"     60 10 ["pocion", "pocion"]

-- 7.1 Eventos
-- a) Encuentra un cofre con n monedas.
encontrarCofre :: Int -> Evento
encontrarCofre n a = a { oro = oro a + n }

-- b) Pierde esa vida, pero nunca queda por debajo de 0.
trampa :: Int -> Evento
trampa danio a = a { vida = max 0 (vida a - danio) }

-- c) Si le alcanza el oro, lo paga y agrega el item AL PRINCIPIO del
--    inventario. Si no le alcanza, queda igual.
comprar :: String -> Int -> Evento
comprar item precio a
  | oro a >= precio = a { oro = oro a - precio, inventario = item : inventario a }
  | otherwise       = a

-- d) Si tiene una "pocion", la usa: +30 de vida (tope 100) y se saca UNA
--    pocion del inventario (usá sacarUna del nivel 1). Si no tiene, igual.
tomarPocion :: Evento
tomarPocion a
  | pertenece "pocion" (inventario a) =
      a { vida = min 100 (vida a + 30), inventario = sacarUna "pocion" (inventario a) }
  | otherwise = a

-- 7.2
estaVivo :: Aventurero -> Bool
estaVivo a = vida a > 0

-- 7.3 Aplica los eventos en orden. IMPORTANTE: si en algún momento el
--     aventurero muere, los eventos que siguen ya no lo afectan (y no hace
--     falta seguir mirándolos). ¿Te sirve componerTodas? ¿Por qué no?
recorrer :: [Evento] -> Evento
recorrer [] a = a
recorrer (e:es) a
  | estaVivo a = recorrer es (e a)
  | otherwise  = a

-- 7.4
sobrevive :: [Evento] -> Aventurero -> Bool
sobrevive es = estaVivo . recorrer es

data Mazmorra = Mazmorra
  { nombreM    :: String
  , eventos    :: [Evento]
  , dificultad :: Int
  }

-- 7.5 Mazmorra no puede derivar Show (¿por qué?). Implementalo a mano:
--     show cuevaDelDragon == "Mazmorra Cueva del Dragon (dificultad 8)"
instance Show Mazmorra where
  show m = "Mazmorra " ++ nombreM m ++ " (dificultad " ++ show (dificultad m) ++ ")"

cuevaDelDragon, mercado, abismo :: Mazmorra
cuevaDelDragon = Mazmorra "Cueva del Dragon" [trampa 40, encontrarCofre 100, tomarPocion, trampa 50] 8
mercado        = Mazmorra "Mercado" [comprar "pocion" 30, comprar "mapa" 100, encontrarCofre 20] 1
abismo         = Mazmorra "El Abismo" (cycle [trampa 10, encontrarCofre 5]) 99   -- ¡infinita!

-- 7.6 Nombres de los que salen vivos.
quienesSobreviven :: Mazmorra -> [Aventurero] -> [String]
quienesSobreviven m = map nombreA . filter (sobrevive (eventos m))

-- 7.7 De los que sobreviven, el que sale con MÁS ORO. Devolvé al aventurero
--     tal como ENTRÓ. Reusá mejorSegun (2.5) y componé.
mejorAventureroPara :: Mazmorra -> [Aventurero] -> Aventurero
mejorAventureroPara m =
  mejorSegun (oro . recorrer (eventos m)) . filter (sobrevive (eventos m))

-- 7.8 Oro con el que queda después de los primeros n eventos.
--     Tiene que funcionar con el abismo (infinito).
oroTrasNEventos :: Int -> Mazmorra -> Aventurero -> Int
oroTrasNEventos n m = oro . recorrer (take n (eventos m))

{-
  7.9 Teórico (esto ES pregunta de parcial):
      a) ¿Por qué Mazmorra no puede derivar Show ni Eq?
      b) ¿Termina `sobrevive (eventos abismo) cande`? Justificá usando
         evaluación diferida.
      c) Si hubieras definido recorrer como `recorrer = componerTodas`
         (o con foldl), ¿terminaría (b)? ¿Por qué?
      d) ¿Termina `length (eventos abismo)`? ¿Y `show abismo`? ¿Qué pasaría
         si el show mostrara la cantidad de eventos?
      e) ¿Dónde usaste aplicación parcial en este nivel? Nombrá dos lugares.
-}
{- RESPUESTAS 7.9
  a) Tiene un campo de tipo [Aventurero -> Aventurero], y las funciones no son
     Show ni Eq (no hay una forma general de mostrarlas ni de saber si dos
     funciones son iguales).
  b) Sí: la vida baja 10 cada dos eventos; cuando llega a 0, recorrer deja de
     pedir eventos y devuelve. Como la lista se genera a demanda (lazy), el
     resto del cycle nunca se construye. Resultado: False.
  c) No: componerTodas / foldl necesitan recorrer TODA la lista de funciones
     antes de devolver algo, y la lista es infinita. Se cuelga. Lo que salva
     a recorrer es el corte explícito cuando muere.
  d) length no termina (lista infinita). show abismo sí, porque sólo usa el
     nombre y la dificultad. Si mostrara `length (eventos m)`, show colgaría
     con el abismo.
  e) Por ejemplo: `trampa 40` y `encontrarCofre 100` son eventos porque les
     falta el aventurero; `filter (sobrevive (eventos m))`; `sobrevive es`
     definida sin nombrar al aventurero; `oro . recorrer (...)`.
-}

nivel7 :: [(String, Bool)]
nivel7 =
  [ ("encontrarCofre 10 lu",               oro (encontrarCofre 10 lu) == 20)
  , ("trampa 50 tomi (no baja de 0)",      vida (trampa 50 tomi) == 0)
  , ("comprar \"mapa\" 100 tomi",          comprar "mapa" 100 tomi == tomi { oro = 100, inventario = ["mapa"] })
  , ("comprar sin plata",                  comprar "mapa" 100 lu == lu)
  , ("tomarPocion lu",                     tomarPocion lu == lu { vida = 90, inventario = ["pocion"] })
  , ("tomarPocion cande (tope 100)",       vida (tomarPocion cande) == 100)
  , ("tomarPocion tomi (no tiene)",        tomarPocion tomi == tomi)
  , ("recorrer cueva con cande",           recorrer (eventos cuevaDelDragon) cande == cande { vida = 40, oro = 150, inventario = ["espada"] })
  , ("muerto no sigue (tomi en cueva)",    oro (recorrer (eventos cuevaDelDragon) tomi) == 200)
  , ("show cuevaDelDragon",                show cuevaDelDragon == "Mazmorra Cueva del Dragon (dificultad 8)")
  , ("quienesSobreviven cueva",            quienesSobreviven cuevaDelDragon [cande, tomi, lu] == ["Cande"])
  , ("inventario de tomi tras mercado",    inventario (recorrer (eventos mercado) tomi) == ["mapa", "pocion"])
  , ("mejorAventureroPara mercado",        mejorAventureroPara mercado [cande, tomi, lu] == tomi)
  , ("oroTrasNEventos 4 abismo tomi",      oroTrasNEventos 4 abismo tomi == 210)
  , ("sobrevive al abismo? (termina)",     not (sobrevive (eventos abismo) cande))
  , ("show abismo",                        show abismo == "Mazmorra El Abismo (dificultad 99)")
  ]

--------------------------------------------------------------------------------
-- Corredor de pruebas (no hace falta tocarlo)
--------------------------------------------------------------------------------

probar :: [(String, Bool)] -> IO ()
probar casos = do
  resultados <- mapM correr casos
  let ok = length (filter id resultados)
  putStrLn ("\n  " ++ show ok ++ "/" ++ show (length resultados) ++ " casos OK\n")
  where
    correr (nombre, condicion) = do
      r <- try (evaluate condicion) :: IO (Either SomeException Bool)
      case r of
        Right True  -> putStrLn ("  OK         " ++ nombre) >> return True
        Right False -> putStrLn ("  FALLA      " ++ nombre) >> return False
        Left _      -> putStrLn ("  PENDIENTE  " ++ nombre) >> return False

probarTodo :: IO ()
probarTodo = mapM_ conTitulo
  [ ("NIVEL 1", nivel1), ("NIVEL 2", nivel2), ("NIVEL 3", nivel3)
  , ("NIVEL 4", nivel4), ("NIVEL 5", nivel5), ("NIVEL 6", nivel6)
  , ("NIVEL 7", nivel7) ]
  where
    conTitulo (titulo, casos) = putStrLn ("== " ++ titulo) >> probar casos
