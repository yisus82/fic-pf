
type elemento = string
type conjunto = elemento list

let vacio = []

let es_vacio conjunto = conjunto == vacio

let unitario x = [x]

let pertenece conjunto elemento =
   List.exists (function x -> elemento = x) conjunto

let anhadir conjunto elemento = 
  if (pertenece conjunto elemento) then
    conjunto
  else
    elemento :: conjunto

let rec union c1 = function
    [] -> c1
  | x :: xs when pertenece c1 x -> union c1 xs
  | x :: xs                     -> union (x::c1) xs

let rec interseccion c1 = function
    [] -> []
  | x :: xs when pertenece c1 x -> x :: interseccion c1 xs
  | _ :: xs                     -> interseccion c1 xs

let rec diferencia c1 c2 = match c1 with
    [] -> []
  | x :: xs when pertenece c2 x -> diferencia xs c2
  | x :: xs                     -> x :: diferencia xs c2

let rec contenido c1 c2 = match c1 with
    [] -> true
  | x :: xs when pertenece c2 x -> contenido xs c2
  | _                           -> false

let igual c1 c2 = contenido c1 c2 & contenido c2 c1

let rec borrar conjunto elemento = match conjunto with
    []    -> []
  | x::xs when elemento = x -> xs
  | x::xs                      -> x :: borrar xs elemento   

let elementos conjunto = conjunto
 
