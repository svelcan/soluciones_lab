-- 21. Define una función que decida si una palabra puede
--     escribirse reorganizando las letras de otra palabra,
--     menos una letra.
-- |   reordenaMenos1 "software" "estofar" = True
-- |   reordenaMenos1 "haskell" "ellas" = False
--

--Santiago Velázquez Cano
--Ivana Fernanda Rojas Gutiérrez
--Patricio Salvador González Castillo

igual :: Char -> String -> Int
igual a "" = 0
igual a (x:xs) = if a == x 
                 then 1 + (igual a xs) 
                 else igual a xs


nuestrovalor :: String -> String -> Int 
nuestrovalor "" "" = 0
nuestrovalor "" a = 0
nuestrovalor a "" = 0
nuestrovalor (a:xs) (b:ys) = (igual a (b:ys)) + (nuestrovalor xs (b:ys))



reordenaMenos1 :: String->String->Bool
reordenaMenos1 (a:xs) (b:ys)=if (nuestrovalor (a:xs) (b:ys) == (length (a:xs)-1))
                              then True
                              else False
