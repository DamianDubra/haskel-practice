ultimo :: [a] -> a

ultimo [x] = x
ultimo (x:xs) = ultimo xs

-------------------------------------------

cantidad :: [a] -> Int

cantidad [] = 0
cantidad (x: xs) = 1 + cantidad xs

-------------------------------------------

suma :: [Int] -> Int

suma [] = 0
suma (x:xs)= x + suma xs

-------------------------------------------

agregaralfinal :: a -> [a] -> [a]

agregaralfinal x [xs] = x ++ xs

------------------------------------------

duplicar :: [Int] -> [Int]

duplicar [] = []
duplicar (x:xs) = [x*2] ++ duplicar xs

-------------------------------------------

duplicar' :: [Int] -> [Int]

duplicar' xs = map(*2) xs

-------------------------------------------

sumarUno :: [Int] -> [Int]

sumarUno xs = map(+1) xs

-------------------------------------------

pares :: [Int] -> [Int]

pares xs = filter (\x -> x ´mod´ 2 == 0 ) xs

-------------------------------------------

mayoresADiez :: [Int] -> [Int]

mayoresADiez xs = filter(\x -> x > 10 == True) xs


-------------------------------------------

hayPar :: [Int] -> Bool

hayPar xs = any (\x -> x 'mod' 2 == 0) xs

-------------------------------------------

todosPositivos ::  [Int] -> Bool

todosPositivos [] = error "lista vacia"
todosPositivos xs = all (\x -> x > 0) xs

-------------------------------------------

perimetro :: FiguraGeometrica -> Float
perimetro (circulo radio) = 2 * 3.14*radio
perimetro (cuadrado lado) =  4*lado
primetro (rectangulo b a) =  2*( b + a)

-------------------------------------------

area :: FiguraGeometrica -> Float 
area (Circulo radio) = 3.14 * radio * radio
area (Cuadrado lado) = lado * lado 
area (Rectangulo b a) = b * a

-------------------------------------------

esFiguraGrande :: FiguraGeometrica -> Bool

esFiguraGrande f = area f > 100

-------------------------------------------

data ArbolBinario 
    = Nodo {valorNodo :: Int} 
    | Rama {
        izq :: ArbolBinario,
        der :: ArbolBinario, 
        valor :: Int
      }

cantidadNodos :: ArbolBinario -> Int

cantidadNodos a = 
