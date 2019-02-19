open Tipos
open Pretty

module C = Conjunto

let rec fv = function
    CBool _ -> C.vacio
    |CInt _ -> C.vacio
    |Cadena _ -> C.vacio
    |LetIn (x, y, z) -> C.union (C.unitario x) (C.union (fv y) (fv z))
    |Par (a, b) -> C.union (fv a) (fv b)
    |Fst a -> fv a
    |Snd a -> fv a
    |Nil -> C.vacio
    |ConsInt (a, b) -> C.union (fv a) (fv b)
    |Hd a -> fv a
    |Tl a -> fv a
    |Null a -> fv a
    |Y a -> fv a
    |Op1 (_, a) -> fv a
    |Op2 (_, a, b) -> C.union (fv a) (fv b)
    |Var x -> C.unitario x
    |Abs(a, _, m) -> C.diferencia(fv m) (C.unitario a)
    |App(m,n) -> C.union (fv m) (fv n)
    |Cond (x, y, z) -> C.union (fv x) (C.union (fv y) (fv z));;

let rec bv = function
    CBool _ -> C.vacio
    |CInt _ -> C.vacio
    |Cadena _ -> C.vacio
    |LetIn (_, y, z) -> C.union (bv y) (bv z)
    |Par (a, b) -> C.union (bv a) (bv b)
    |Fst a -> bv a
    |Snd a -> bv a
    |Nil -> C.vacio
    |ConsInt (a, b) -> C.union (bv a) (bv b)
    |Hd a -> bv a
    |Tl a -> bv a
    |Null a -> bv a
    |Y a -> bv a
    |Op1 (_, a) -> bv a
    |Op2 (_, a, b) -> C.union (bv a) (bv b)
    |Var _ -> C.vacio
    |Abs(a, _, m) -> C.union(bv(m)) (C.unitario a)
    |App(m,n) -> C.union (bv(m)) (bv(n))
    |Cond (e1, e2, e3) -> C.union  (bv e1) (C.union (bv e2) (bv e3));;

let rec subst m x n = match m with 
    CBool b -> CBool b
    |CInt i -> CInt i
    |Cadena c -> Cadena c
    |LetIn (a, b, c) -> LetIn (a, (subst b x n), (subst c x n))
    |Par (a, b) -> Par ((subst a x n), (subst b x n))
    |Fst a -> Fst (subst a x n)
    |Snd a -> Snd (subst a x n)
    |Nil -> Nil
    |ConsInt (a, b) -> ConsInt ((subst a x n), (subst b x n))
    |Hd a -> Hd (subst a x n)
    |Tl a -> Tl (subst a x n)
    |Null a -> Null (subst a x n)
    |Y a -> Y (subst a x n)
    |Var y -> if (y = x) then n else m
    |Abs (y, t, m') -> if (y = x) 
	then Abs(x, t, m') 
	else if (C.pertenece (fv(n)) (y))
		then let z = Fresca.fresca() in Abs(z, t, subst (subst m' y (Var z)) x n)
		else Abs(y, t, subst m' x n)
    |App (m', m'') ->  App(subst m' x n, subst m'' x n)
    |Cond (e1, e2, e3) -> Cond ((subst e1 x n), (subst e2 x n), (subst e3 x n))
    |Op1 (op, a) -> Op1 (op, (subst a x n))
    |Op2 (op, a, b) -> Op2 (op, (subst a x n), (subst b x n));;

let int_of_term = function
    CInt a -> a
    |_ -> failwith "El termino no es un entero";;
    
let cadena_of_term = function 
    Cadena a -> a
    |_ -> failwith "El termino no es una cadena";;

let rec eval = function
    CBool b -> CBool b
    |CInt i -> CInt i
    |Cadena c -> Cadena c
    |LetIn (x, y, z) -> eval (subst z x y)
    |Par (a, b) -> Par ((eval a), (eval b))
    |Fst a -> (match (eval a) with
	Par (a, _) -> a
	|_ -> failwith "Par no valido")
    |Snd a -> (match (eval a) with
	Par (_, b) -> b
	|_ -> failwith "Par no valido")
    |Nil -> Nil
    |ConsInt (a, b) -> ConsInt ((eval a), (eval b))
    |Hd a -> (match (eval a) with
	ConsInt (x, _) -> x
	|_ -> failwith "Lista invalida")
    |Tl a -> (match (eval a) with
	ConsInt (_, y) -> y
	|_ -> failwith "Lista invalida")
    |Null _ -> Cadena "Hay que evaluar el null"
    |Y _ -> Cadena "Hay que evaluar punto fijo"
    |Var _ -> failwith "Expresion no cerrada"
    |Abs (x, t, e) -> Abs (x, t, e)
    |App  (e1, e2) -> (match (eval e1) with
	Abs(x, _, e) -> eval (subst e x e2)
	|_ -> failwith ("No-abstraccion " ^ (Pretty.term e1) ^ " aplicada a " ^ (Pretty.term e2)))
    |Cond (e1, e2, e3) -> (match (eval e1) with
	CBool V -> eval e2
	|CBool F -> eval e3
	|_ -> failwith ("El no-booleano " ^ (Pretty.term e1) ^ " aparece como guardia del condicional"))
    |Op1 (op, a) -> (match op with
	Menos -> CInt (-(int_of_term (eval a)))
	|Lengthc -> CInt (String.length (cadena_of_term (eval a))))
    |Op2 (op, a, b) -> (match op with
	Sum -> CInt ((int_of_term (eval a)) + (int_of_term (eval b)))
	|Res -> CInt ((int_of_term (eval a)) - (int_of_term (eval b))) 
	|Mul -> CInt ((int_of_term (eval a)) * (int_of_term  (eval b)))
	|Div -> CInt ((int_of_term (eval a)) / (int_of_term (eval b))) 
	|Mayor -> if ((int_of_term (eval a)) > (int_of_term (eval b))) then CBool V else CBool F
	|Igual -> if ((int_of_term (eval a)) = (int_of_term (eval b))) then CBool V else CBool F
	|Menor -> if ((int_of_term (eval a)) < (int_of_term (eval b))) then CBool V else CBool F
	|Conc -> Cadena((cadena_of_term (eval a)) ^ (cadena_of_term (eval b))));;

let substituir m x (nombre, valor) = (nombre, subst valor x m)

let rec organizar = function
    [(_, valor)] -> valor
  | (nombre, valor):: hs -> organizar (List.map (substituir valor nombre) hs)
  | [] -> failwith "organizar: lista no puede estar vacia"

let eval_seguro term = 
  let tipo = Tipado.tipo_de term
  and term' = eval term
  in 
    (term',tipo)
