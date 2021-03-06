

open Tipos

module C = Conjunto

let rec fv = function
    Var x     -> C.unitario x
    |Abs(x,m) -> C.diferencia (fv(m)) (C.unitario x)
    |App(m,n) -> C.union (fv(m)) (fv(n))

let rec bv = function
    Var _     -> C.vacio
    |Abs(x,m) -> C.union (bv(m)) (C.unitario x)
    |App(m,n) -> C.union (bv(m)) (bv(n))

(* m[n/x] *)

let rec subst m x n = match m with
    Var y       -> if (y=x) then n
                      else Var y
    |App(m1,m2) -> App(subst m1 x n,subst m2 x n)
    |Abs(y,m)   -> if (y=x) then Abs(x,m)
                      else if (C.pertenece (fv(n)) (y))
                              then let z=Fresca.fresca() in Abs(z,subst
(subst m y (Var z)) x n)
                              else Abs(y,subst m x n);;

let rec hnf = function
    Var x     -> Var x
    |Abs(x,m) -> Abs(x,hnf m)
    |App(m,n) -> match (hnf m) with
                     Abs(x,m') -> hnf(subst m' x n)
                     |h        -> App(h,n);;

let rec whnf = function
    Var x     -> Var x
    |Abs(x,m) -> Abs(x,m)
    |App(m,n) -> match (whnf m) with
                     Abs(x,m') -> whnf(subst m' x n)
                     |h        -> App(h,n);;

let rec nf = function
    Var x     -> Var x
    |Abs(x,m) -> Abs(x,nf m)
    |App(m,n) -> match (hnf m) with
                     Abs(x,m') -> nf(subst m' x n)
                     |h        -> App(nf h,nf n);;

let rec wnf = function
    Var x     -> Var x
    |Abs(x,m) -> Abs(x,wnf m)
    |App(m,n) -> match (whnf m) with
                     Abs(x,m') -> wnf(subst m' x n)
                     |h        -> App(wnf h,wnf n);;

let rec vnf = function
    Var x     -> Var x
    |Abs(x,m) -> Abs(x,vnf m)
    |App(m,n) -> let n' = vnf n
                 in match (whnf m) with
                     Abs(x,m') -> vnf(subst m' x n')
                     |h        -> App(vnf h,n');;

let rec vwnf = function
    Var x     -> Var x
    |Abs(x,m) -> Abs(x,m)
    |App(m,n) -> let n' = vwnf n
                 in match (vwnf m) with
                     Abs(x,m') -> vwnf(subst m' x n')
                     |h        -> App(h,n');;

let substituir m x (nombre, valor) = (nombre, subst valor x m)

let rec organizar = function
    [(_, valor)] -> valor
  | (nombre, valor):: hs -> organizar (List.map (substituir valor nombre) hs)
  | [] -> failwith "organizar: lista no puede estar vacia"

let eval term = nf (organizar term)

let cero = Abs("f",Abs("x", Var "x"));;
let uno = Abs("f",Abs("x", App (Var "f", Var "x")));;

let succ = function
	Abs("f", Abs("x", a)) -> Abs("f", Abs("x", App (Var "f", a)))
	|_ -> failwith "No es un natural de Church";;

let rec term_of_int = function
	0 -> Abs("f",Abs("x", Var "x"))
       |n -> if (n > 0) 
		then (succ (term_of_int (n-1)))
		else failwith "Numero negativo";;

let rec int_of_term2 = function
    Abs(f,Abs(x, Var y)) -> if (x = y) then 0 else failwith "Distinta x"
    |Abs(f,Abs(x, App(Var g, a))) 
	-> if (f = g) 
	then 1 + (int_of_term2 (Abs(f,Abs(x, a))))
	else failwith "Distinta f"
    |_ -> failwith "No es un natural de Church";;

let int_of_term = function
	Abs(f, Abs(x, n)) -> int_of_term2 (Abs(f, Abs(x, n)))
	|_ -> failwith "No es un natural de Church";;
