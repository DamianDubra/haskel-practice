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



