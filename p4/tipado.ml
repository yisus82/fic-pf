open Tipos

type contexto = (variable * tipo) list;;

let contexto_vacio = [];;

let rec variable_en_tipo x = function
	  Bool -> false
	| Int -> false 
	| IntList -> false
	| String -> false
	| Variable y when x=y -> true
	| Variable _ -> false
	| Flecha (tp1, tp2) -> (variable_en_tipo x tp1) or (variable_en_tipo x tp2)
	| Producto (tp1, tp2) -> (variable_en_tipo x tp1) or (variable_en_tipo x tp2);;
	  
type sustitucion = (variable * tipo) list;;

let sustitucion_vacia = [];;

let rec sust_var sigma x = 
	match sigma with
		  [] -> Variable x
		| (y, t)::_ when x=y -> t
		| _::sigma' -> sust_var sigma' x;;
		
let rec sust_tipo sigma = function
	  Bool -> Bool
	| Int -> Int 
	| IntList -> IntList
	| String -> String
	| Flecha (t1, t2) -> Flecha (sust_tipo sigma t1, sust_tipo sigma t2)
	| Variable x -> sust_var sigma x
	| Producto (t1, t2) -> Producto (sust_tipo sigma t1, sust_tipo sigma t2);;

exception Sust_recursiva;;

let rec ext_sust (y, t) sigma =
	let t' = sust_tipo sigma t
	in if t' = Variable y
		then sigma
		else if variable_en_tipo y t'
				then raise Sust_recursiva
				else (y, t')::List.map
					(fun (xi, ti) -> (xi, sust_tipo [(y, t')] ti))
					sigma;;
					
exception Unificacion;;

let rec unif sigma = function
	  (Bool, Bool) -> sigma
	| (Int, Int) -> sigma
	| (IntList, IntList) -> sigma
	| (String, String) -> sigma
	| (Flecha (t1, t2), Flecha (s1, s2)) -> 
		unif (unif sigma (t1, s1)) (t2, s2)
	| (Producto (t1, t2), Producto (s1, s2)) -> 
		unif (unif sigma (t1, s1)) (t2, s2)
	| (Variable x, t) ->
		(match (sust_var sigma x) with
			  Variable y -> ext_sust (y, t) sigma
			| s -> unif sigma (s, t)  
		)
	| (t, Variable x) -> unif sigma (Variable x, t)
	| _ -> raise Unificacion;;


let rec resolver_sistema_restricciones = function
	  [] -> sustitucion_vacia
	| e::es -> unif (resolver_sistema_restricciones es) e;;

let rec tipo_var contexto var =
    match contexto with
       (v, tipo)::_ when var=v -> tipo
     | _::contexto' -> tipo_var contexto' var
     | _ -> raise (Tipos.NO_LIGADA(var))


let rec tipar contexto = function
	  CBool _ -> (Bool, [])
	| CInt _ -> (Int, [])
	| Op2(op2, t1, t2) -> let (tp1,r1) = tipar contexto t1
					and (tp2,r2) = tipar contexto t2
	    			in (match op2 with 
	    		  		Sum -> (Int,  (tp1,Int) :: (tp2, Int) :: r1 @ r2) 
					| Res -> (Int,  (tp1,Int) :: (tp2, Int) :: r1 @ r2) 
					| Mul -> (Int,  (tp1,Int) :: (tp2, Int) :: r1 @ r2) 
					| Div -> (Int,  (tp1,Int) :: (tp2, Int) :: r1 @ r2)
					| Mayor -> (Bool, (tp1,Int) :: (tp2, Int) :: r1 @ r2)
					| Igual -> (Bool, (tp1,Int) :: (tp2, Int) :: r1 @ r2)
					| Menor -> (Bool, (tp1,Int) :: (tp2, Int) :: r1 @ r2)
					| Conc -> (String, (tp1,String) :: (tp2, String) :: r1 @ r2))
	| Var v -> ((tipo_var contexto v), [])
	| Abs(var, tipo, term) -> let (t,r) = tipar ((var, tipo)::contexto) term
					in (Flecha(tipo,t),r)
	| App(t1, t2) -> let (tp1,r1) = tipar contexto t1
				and (tp2,r2) = tipar contexto t2
				and f = (Variable (Fresca.fresca ()))
	    		in (f, (tp1, Flecha(tp2,f)) :: r1 @ r2) 	
	| Cond(t1, t2, t3) -> let (tp1,r1) = tipar contexto t1
					and (tp2,r2) = tipar contexto t2
					and (tp3,r3) = tipar contexto t3
	    			in (tp2, (tp1,Bool) :: (tp2, tp3) :: r1 @ r2 @ r3) 

	| Cadena _ -> (String, [])
	| Op1(op, tm) ->  let (t,r) = tipar contexto tm
	    			in (match op with 
	    		  		Lengthc -> (Int, (t,String) :: r)
	    				| Menos-> (Int, (t,Int) :: r))
	| LetIn(v, tm1, tm2) -> let (tp1,r1) = tipar contexto tm1
				in let (tp2,r2) = (tipar ((v, tp1)::contexto) tm2)
		in (tp2, r1 @r2)
	| Y tm	-> let (t,r) = tipar contexto tm
				and f = Variable (Fresca.fresca ())
			in (f, (t,Flecha(f,f)) :: r)
	| Par(tm1, tm2)	-> let (tp1,r1) = tipar contexto tm1
					and (tp2,r2) = tipar contexto tm2
					and f = Variable (Fresca.fresca ())
	    			in (Producto(tp1,tp2), r1 @ r2)		
    	| Fst tm -> let (t,r) = tipar contexto tm
    				and f = Variable (Fresca.fresca ())
    			in (f, (t, Producto(f,Variable (Fresca.fresca ()))) :: r)
    	| Snd tm -> let (t,r) = (tipar contexto tm)
    				and f = Variable (Fresca.fresca ())
    			in (f, (t, Producto(Variable (Fresca.fresca ()),f)) :: r)
    	| Nil -> (IntList, [])
    	| Null tm -> let (t,r) = (tipar contexto tm)
    			in (Bool, (t,IntList) :: r)
    	| ConsInt(tm1, tm2) -> let (tp1,r1) = tipar contexto tm1
					and (tp2,r2) = tipar contexto tm2
	    			in (IntList, (tp1,Int) :: (tp2,IntList) :: r1 @ r2)        

   	| Hd tm	-> let (t,r) = tipar contexto tm
			in (Int, (t,IntList) :: r)
	| Tl tm -> let (t,r) = tipar contexto tm
			in (IntList, (t,IntList) :: r)

let tipo_de e = let (t, r) = (tipar [] e)
		in sust_tipo (resolver_sistema_restricciones r) t
  
