import Distribution.Simple.Utils (xargs, warn)

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


{-1. Literales
describirNumero :: Int -> String: "cero" para 0, "uno" para 1, "muchos" para el resto.

map describirNumero [0, 1, 7]  -- ["cero","uno","muchos"]-}

describirNumero :: Int -> String
describirNumero a
    |a==0 = "Cero"
    |a==1 = "Uno"
    |otherwise = "muchos"

{-2. Tuplas
primero :: (a, b, c) -> a
intercambiar :: (a, b) -> (b, a)
primero (1, "a", True)  -- 1
intercambiar (1, "a")   -- ("a",1)-}

primero :: (a, b, c) -> a
primero (x,_,_) = x

intercambiar :: (a, b) -> (b, a)
intercambiar (x,y) = (y,x)

{-3. Listas
cabeza :: [a] -> Maybe a
segundo :: [a] -> Maybe a (el segundo elemento, si existe)
tieneExactamenteDos :: [a] -> Bool, sin usar length
(cabeza "", cabeza "abc")         -- (Nothing,Just 'a')
(segundo [1], segundo [1, 2, 3])  -- (Nothing,Just 2)
tieneExactamenteDos [1, 2]        -- True-}

cabeza :: [a] -> Maybe a
cabeza []= Nothing
cabeza (x:xs)= Just x
-- maybe puede o no tener nada, solo salva errores de ejerccicion, tiene el formato Nothing | Just a, si esta vacia va el nothing y si tiene algo dice simplemente una a

segundo :: [a] -> Maybe a
segundo [] = Nothing
segundo [_]= Nothing
segundo (x:xs:xss) = Just xs

tieneExactamenteDos :: [a] -> Bool
tieneExactamenteDos xs
    |length(xs) == 2 = True
    |otherwise =False

tieneExactamenteDos' :: [a] -> Bool
tieneExactamenteDos' [_, _] = True
tieneExactamenteDos' _ = False

{-4. Maybe
desdeMaybe :: a -> Maybe a -> a: devuelve el valor, o el default si es Nothing.
sumarMaybes :: Maybe Int -> Maybe Int -> Maybe Int: Just de la suma si ambos son Just, si no Nothing.
desdeMaybe 0 (Just 5)          -- 5
sumarMaybes (Just 1) (Just 2)  -- Just 3
sumarMaybes (Just 1) Nothing   -- Nothing-}

desdeMaybe :: a -> Maybe a -> a
desdeMaybe a Nothing = a
desdeMaybe _ (Just b) = b

sumarMaybes :: Maybe Int -> Maybe Int -> Maybe Int
sumarMaybes Nothing Nothing = Nothing
sumarMaybes (Just _) Nothing = Nothing  -- por el enunciado, si no puede devolver el jus y algo
sumarMaybes Nothing (Just _) = Nothing
sumarMaybes (Just a) (Just b) = Just(a+b)

{-5. Guardas
clasificarNota :: Int -> String: "invalida" fuera de 0..10, "desaprobado" si es menor a 4, "aprobado" si es menor a 7, "promocionado" en otro caso.

map clasificarNota [2, 5, 9, 11]
-- ["desaprobado","aprobado","promocionado","invalida"]-}

clasificarNota :: Int -> String
clasificarNota nota
    |nota <0 = "invalida"
    |nota > 10 = "invalida"
    |nota < 4 = "desaprobado"
    |nota < 7 = "aprobado"
    |otherwise = "promocion"

{-6. Records
Dado data Punto = Punto { px :: Double, py :: Double }:

esOrigen :: Punto -> Bool, matcheando por posición: (Punto 0 0).
estaEnEjeY :: Punto -> Bool, matcheando por nombre de campo: (Punto { px = 0 }).
cuadrante :: Punto -> Int: 1 a 4, o 0 si está sobre algún eje.
map cuadrante [Punto 1 1, Punto (-1) 1, Punto (-1) (-1), Punto 1 (-1), Punto 0 3]
-- [1,2,3,4,0]-}

data Punto = Punto { px :: Double, py :: Double }


esOrigen :: Punto -> Bool
esOrigen (Punto px py) = if px == 0 && py == 0 then True else False

esOrigen' :: Punto -> Bool
esOrigen' (Punto x y) = x == 0 && y == 0-- opcion sin el if

estaEnEjeY :: Punto -> Bool
estaEnEjeY (Punto px py) = px == 0 && py /= 0 -- no verifica por nombre de campo pero es valido

estaEnEjeY' :: Punto -> Bool
estaEnEjeY' (Punto { px = 0 }) = True -- caso 1 que en x sea 0 entonces esta sobre y en cualquier valor
estaEnEjeY' _ = False -- caso 2 cualquier que no este en x 0 es invalido

cuadrante :: Punto -> Int
cuadrante (Punto px py)
    |px == 0 || py==0 = 0
    |px >0 && py >0 = 1
    |px<0 && py>0 =2
    |px<0 && py<0 =3
    |otherwise = 4


-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Polimorfismo paramétrico y ad-hoc (clases de tipo)

{-1. Funciones paramétricas
identidad :: a -> a
duplicar :: a -> (a, a)
aplicarPar :: (a -> b) -> (a, a) -> (b, b)
(identidad 'a', duplicar True, aplicarPar (+ 1) (1, 2))  -- ('a',(True,True),(2,3))-}

identidad :: a -> a
identidad a = a

duplicar :: a -> (a, a)
duplicar a = (a,a)

aplicarPar :: (a -> b) -> (a, a) -> (b, b)
aplicarPar func (a,b) = (func a, func b)

{-2. Tipos paramétricos
Definí data Par a b = Par a b y data Caja a = Caja a (derivando Show) y escribí:

intercambiarPar :: Par a b -> Par b a
mapCaja :: (a -> b) -> Caja a -> Caja b
intercambiarPar (Par 1 "a")   -- Par "a" 1
mapCaja length (Caja "hola")  -- Caja 4-}

data Par a b = Par a b

data Caja a = Caja a
    deriving(Show)


intercambiarPar :: Par a b -> Par b a
intercambiarPar (Par a b) = (Par b a)

mapCaja :: (a -> b) -> Caja a -> Caja b
mapCaja func (Caja a) = (Caja (func a))

{-3. ¿Qué restricción hace falta?
Escribí las funciones y poné en la firma la restricción de clase mínima que necesita cada una:

maximoDe :: ??? => [a] -> a
sumarTodos :: ??? => [a] -> a
buscar :: ??? => k -> [(k, v)] -> Maybe v
maximoDe "haskell"               -- 's'
sumarTodos [1.5, 2.5]            -- 4.0
buscar "b" [("a", 1), ("b", 2)]  -- Just 2-}

maximoDe :: Ord a => [a] -> a
maximoDe [x] = x
maximoDe (x:xs) = max x (maximoDe xs)

sumarTodos :: Num a => [a] -> a
sumarTodos [x] = 0
sumarTodos (x:xs) = x + sumarTodos(xs)

buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar _ [] = Nothing
buscar k ((c, v):resto)
    |k == c = Just v 
    |otherwise = buscar k resto

{-4. Instancia de Show a mano
Dado data Punto = Punto Double Double (sin deriving), escribí instance Show Punto para que se muestre como (1.0, 2.5).

deriving (Eq, Ord, Show)-}

data Punto' = Punto' Double Double

instance Show Punto' where
    show (Punto' a b) = "("++show a++", "++show b++")"

{-5. Eq y Ord a mano
Dado el record Persona { nombre :: String, edad :: Int }:

instance Eq Persona: dos personas son iguales si tienen el mismo nombre.
instance Ord Persona: se ordenan por edad. Alcanza con definir compare.
Persona "Ana" 30 == Persona "Ana" 99           -- True
maximum [Persona "Ana" 30, Persona "Beto" 25]  -- Persona {nombre = "Ana", edad = 30}-}

data Persona = Persona { nombre :: String, edad :: Int }
  deriving (Show)

instance Eq Persona where
    (==)  (Persona a b) (Persona x y)= a==x && b==y

instance Ord Persona where
    (Persona _ a) <= (Persona _ b) = a <= b


{-6. Tu propia clase
Definí class Describible a con describir :: a -> String e instancias para Bool, Int y [a] (esta última requiere que a también sea Describible).

describir True           -- "verdadero"
describir (3 :: Int)     -- "el entero 3"
describir [True, False]  -- "lista de 2: verdadero falso"-}

class Describible a where
    palabra :: a -> String

instance Describible Bool where
    palabra (True) = "verdadero"
    palabra (False) = "Falso"

instance Describible Int where
    palabra a = "el entero es" ++ show a

instance Describible a => Describible [a] where  -- el priemro es la restriccion de que cualquer a tiene que ser describible, la segunda el tipo de cosa a describir
    palabra []= []
    palabra (x:xs) = palabra x ++", " ++ palabra xs

{-7. Una clase, varios tipos
Definí class Forma a con area y perimetro (ambas devuelven Double), e instancias para data Circulo = Circulo Double y data Rectangulo = Rectangulo Double Double.

(area (Circulo 1), perimetro (Rectangulo 2 3))  -- (3.141592653589793,10.0)
Para pensar: compará esto con el data Figura de tipos-de-datos.md. ¿Qué es más fácil con cada enfoque: agregar una figura nueva, o agregar una operación nueva?-}

data Figura = Circulo Double | Rectangulo Double Double

class Forma a where
    area :: a -> Double
    perimetro :: a -> Double

instance Forma Figura where
    area (Circulo r) = pi *r*r
    area (Rectangulo b a) = b*a
    perimetro (Circulo r) = 2*pi*r
    perimetro (Rectangulo b a) = 2* (b+a)

{-8. Métodos por defecto y newtype
Definí:

class Monoide a where
  neutro        :: a
  combinar      :: a -> a -> a
  combinarTodos :: [a] -> a   -- con implementación por defecto usando foldr
e instancias para newtype Suma = Suma Int (suma), newtype Producto = Producto Int (producto) y [b] (concatenación). Las instancias no deben redefinir combinarTodos.

combinarTodos [Suma 1, Suma 2, Suma 3]  -- Suma 6
combinarTodos [Producto 2, Producto 5]  -- Producto 10
combinarTodos ["ab", "cd", "e"]         -- "abcde"-}

class Monoide a where
  neutro        :: a
  combinar      :: a -> a -> a
  combinarTodos :: [a] -> a
  combinarTodos xs = foldr combinar neutro xs

newtype Suma = Suma Int deriving (Show)

newtype Producto = Producto Int deriving(Show)

instance Monoide Suma where
    neutro = Suma 0
    combinar (Suma a) (Suma b) = Suma (a + b)

instance Monoide Producto where
    neutro = Producto 1
    combinar (Producto a) (Producto b) = Producto (a*b)

instance Monoide [a] where
    neutro = []
    combinar = (++)