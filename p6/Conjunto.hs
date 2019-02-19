
module Conjunto where

import Tipos

newtype Conjunto = Conjunto [Variable] deriving Show

vacio :: Conjunto
vacio = Conjunto []

es_vacio :: Conjunto -> Bool
es_vacio (Conjunto xs) = null xs

unitario :: Variable -> Conjunto
unitario x = Conjunto [x]

pertenece :: Conjunto -> Variable -> Bool
pertenece (Conjunto xs) x = elem x xs

anhadir :: Conjunto -> Variable -> Conjunto
anhadir (Conjunto xs) x | elem x xs = Conjunto xs
			| otherwise = Conjunto (x:xs)

union :: Conjunto -> Conjunto -> Conjunto
union conjunto (Conjunto ys) = foldl anhadir conjunto ys

diferencia :: Conjunto -> Conjunto -> Conjunto
diferencia (Conjunto xs) (Conjunto ys) = Conjunto (diff xs ys)

diff :: [Variable] -> [Variable] -> [Variable]
diff [] ys = []
diff (x:xs) ys | x `elem` ys = diff xs ys
	       | otherwise   = x : diff xs ys

contenido :: Conjunto -> Conjunto -> Bool
contenido (Conjunto xs) (Conjunto ys) = and (map (flip elem ys) xs)

igual :: Conjunto -> Conjunto -> Bool
igual c1 c2 = contenido c1 c2 && contenido c2 c1

borrar :: Conjunto -> Variable -> Conjunto
borrar (Conjunto xs) x = Conjunto (filter (/= x) xs)

elementos :: Conjunto -> [Variable]
elementos (Conjunto xs) = xs
