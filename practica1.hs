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