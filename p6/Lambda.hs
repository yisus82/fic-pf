
module Lambda where

import Conjunto
import Tipos
import Lector

libres :: Term -> Conjunto
libres (Var x)     = unitario x
libres (Bool _)    = vacio
libres (Literal _) = vacio
libres (Numero _)  = vacio
libres (Abs var _ term) = diferencia (libres term) (unitario var)
libres (App e1 e2) = union (libres e1) (libres e2)
libres (Cond e1 e2 e3) = union (libres e1) (union (libres e2) (libres e3))
libres (Longitud term) = libres term
libres (Concatenacion term1 term2) = union (libres term1) (libres term2)
libres (Caracter term1 term2) = union (libres term1) (libres term2)
libres (Igual_enteros term1 term2) = union (libres term1) (libres term2)
libres (Igual_cadenas term1 term2) = union (libres term1) (libres term2)
libres (Par term1 term2) = union (libres term1) (libres term2)
libres (Primero term) = libres term
libres (Segundo term) = libres term
libres (OperacionEntera _ term1 term2) = union (libres term1) (libres term2)
libres (Y term) = libres term
libres (LetIn var term1 term2) = union (unitario var) (union (libres term1) (libres term2))
libres (Nulo) = vacio
libres (ConsEntero term1 term2) = union (libres term1) (libres term2)
libres (Cabeza term) = libres term
libres (Cola term) = libres term
libres (EsVacia term) = libres term
libres _ = error ("No estan definidas las variables libres")

fresca :: Conjunto -> Variable
fresca c = head (filter (not . pertenece c) variables) 
    where variables = ["x" ++ show x | x <- [1..]]

-- subst m v n = m[v\n]

subst :: Term -> Variable -> Term -> Term
subst (Var y) x n | x == y = n
		  | otherwise = Var y
subst (Bool v) _ _ = Bool v
subst (Numero y) _ _ = Numero y
subst (Literal c) _ _ = Literal c
subst (Abs var tipo term) x n = 
	if (var == x) then (Abs x tipo term) 
		else if (pertenece (libres n) var)
			then let z = (fresca (libres n)) in (Abs z tipo (subst (subst term var (Var z)) x n))
			else (Abs var tipo (subst term x n))
subst (App e1 e2) x n = App (subst e1 x n) (subst e2 x n)
subst (Cond e1 e2 e3) x n = Cond (subst e1 x n) (subst e2 x n) (subst e3 x n)
subst (Igual_enteros term1 term2) x n = Igual_enteros (subst term1 x n) (subst term2 x n)
subst (Igual_cadenas term1 term2) x n = Igual_cadenas (subst term1 x n) (subst term2 x n)
subst (Par term1 term2) x n = Par (subst term1 x n) (subst term2 x n)
subst (Primero term) x n = Primero (subst term x n)
subst (Segundo term) x n = Segundo (subst term x n)
subst (OperacionEntera op term1 term2) x n = OperacionEntera op (subst term1 x n) (subst term2 x n)
subst (Y term) x n = Y (subst term x n)
subst (LetIn var term1 term2) x n = LetIn var (subst term1 x n) (subst term2 x n)
subst (Nulo) x n = Nulo
subst (ConsEntero term1 term2) x n = ConsEntero (subst term1 x n) (subst term2 x n)
subst (Cabeza term) x n = Cabeza (subst term x n)
subst (Cola term) x n = Cola (subst term x n)
subst (EsVacia term) x n = EsVacia (subst term x n)
subst _ _ _ = error ("No esta definida la sustitucion")

int_of_term :: Term -> Int
int_of_term (Numero a) = a
int_of_term _ = error ("El termino no es un entero")

fn :: Term -> Term
fn (Bool y) = Bool y
fn (Var y) = Var y
fn (Numero y) = Numero y
fn (Literal y) = Literal y
fn (Abs var tipo term) = Abs var tipo term
fn (App term1 term2) 	|((fn term1) == Abs var tipo term) = fn (subst term var term2) 
			|otherwise = error ("No-abstraccion aplicada a termino")
			where (Abs var tipo term) = (fn term1)
fn (Cond e1 e2 e3) 	| ((fn e1) == Bool V) = fn e2
			| ((fn e1) == Bool F) = fn e3
			| otherwise = error ("La condicion no es de tipo Booleano")
fn (Igual_enteros term1 term2)  | ((fn term1) == (fn term2)) = Bool V
				| otherwise = Bool F
fn (Igual_cadenas term1 term2)  | ((fn term1) == (fn term2)) = Bool V
				| otherwise = Bool F
fn (Par term1 term2) = Par (fn term1) (fn term2)
fn (Primero term) 	| ((fn term) == Par a b) = a
			| otherwise = error ("Par no valido")
			where (Par a b) = (fn term)
fn (Segundo term) 	| ((fn term) == Par a b) = b
			| otherwise = error ("Par no valido") 
			where (Par a b) = (fn term)
fn (OperacionEntera op term1 term2) 	| (op == Suma) = Numero (int_of_term(fn term1) + int_of_term(fn term2))
					| (op == Resta) = Numero (int_of_term(fn term1) - int_of_term(fn term2))
					| (op == Multiplicacion) = Numero (int_of_term(fn term1) * int_of_term(fn term2))
					| (op == Division) = Numero (div (int_of_term(fn term1)) (int_of_term(fn term2)))
					| (op == Modulo) = Numero (mod (int_of_term(fn term1)) (int_of_term(fn term2)))
					| (op == Menor) = if (int_of_term(fn term1) < int_of_term(fn term2)) then Bool V else Bool F
fn (Y term) = Literal "Hay que evaluar el punto fijo"
fn (LetIn var term1 term2) = fn (subst term2 var term1)
fn (Nulo) = Nulo
fn (ConsEntero term1 term2) = ConsEntero (fn term1) (fn term2)
fn (Cabeza term) 	| ((fn term) == ConsEntero x y) = x
			| otherwise = error ("Lista no valida")
			where (ConsEntero x y) = (fn term)
fn (Cola term) 		| ((fn term) == ConsEntero x y) = y
			| otherwise = error ("Lista no valida")
			where (ConsEntero x y) = (fn term) 
fn (EsVacia term)	| ((fn term) == Nulo) = Bool V
			| otherwise = Bool F
fn _ = error ("No esta definida la funcion normal")

eval :: String -> Term
eval = fn . leer

