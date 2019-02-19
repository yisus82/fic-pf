open Pretty
open Tipos
open Lambda

type contexto = (variable * tipo);;

let rec sup_tipo contexto x =
	match contexto with
		(y,t)::_ when x = y -> t
		|(_,_)::contexto' -> sup_tipo contexto' x
		|_ -> failwith ("Variable no encontrada en el contexto: " ^ x);;

let rec tipar contexto = function
    CBool _ -> Bool
    |CInt _ -> Int
    |Cadena _ -> String
    |LetIn (x, y, z) -> tipar ((x, (tipar contexto y))::contexto) z
    |Par (a, b) -> Producto ((tipar contexto a), (tipar contexto b))
    |Fst a -> (match (tipar contexto a) with
	Producto(t, _) -> t
	|_ -> failwith ("Par mal construido"))
    |Snd a -> (match (tipar contexto a) with
	Producto(_, t') -> t'
	|_ -> failwith ("Par mal construido"))
    |Nil -> IntList
    |ConsInt (a, b) -> (match (tipar contexto a) with
        Int when (tipar contexto b) = IntList -> IntList
	|_ -> failwith ("Lista de enteros mal construida"))
    |Hd _ -> Int
    |Tl _ -> IntList
    |Null _ -> Bool
    |Y a -> (match (tipar contexto a) with
	Flecha(t, t') -> t'
	|_ -> failwith "Punto fijo mal utilizado")
    |Var x -> sup_tipo contexto x
    |Abs (x, t, e) -> Flecha(t, tipar ((x, t)::contexto) e)
    |App  (e1, e2) -> (match (tipar contexto e1) with
	Flecha(t', t) when (tipar contexto e2) = t' -> t
	|_ -> failwith ("No-abstraccion " ^ Pretty.term e1 ^ " aplicada a: " ^ Pretty.term e2))
    |Cond (e1, e2, e3) when 
	((tipar contexto e1) = Bool) &&
	((tipar contexto e2) = (tipar contexto e3)) -> tipar contexto e3
    |Op1 (op, a) -> (match op with
	Menos when (tipar contexto a) = Int -> Int
	|Lengthc when (tipar contexto a) = String -> Int
        |_ -> failwith "La expresion con operador unario no puede ser tipada") 
    |Op2 (op, a, b) -> (match op with
	Sum when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Int
	|Res when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Int 
	|Mul when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Int 
	|Div when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Int 
	|Mayor when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Bool
	|Igual when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Bool
	|Menor when (((tipar contexto a) = Int) && ((tipar contexto b) = Int)) -> Bool
	|Conc  when ((tipar contexto a) = String) -> String      
	|_ -> failwith "La expresion con operador binario no puede ser tipada")
    |_ -> failwith "No se pudo evaluar";;

let tipo_de e = tipar [] e
