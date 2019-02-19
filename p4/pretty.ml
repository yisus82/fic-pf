
open Tipos

let rec tipo = function
    Bool   -> "Bool"
  | Int    -> "Int"
  | IntList -> "IntList"
  | String -> "String"
  | Flecha (m,n) -> "(" ^ tipo m ^ " -> " ^ tipo n ^ ")"
  | Producto (m,n) -> "(" ^ tipo m ^ "," ^ tipo n ^ ")"
  | Variable v -> v

let booleano = function 
    V -> "true"
  | F -> "false" 

let entero = function
    x when x >= 0 -> string_of_int x
  | x             -> "(" ^ string_of_int x ^ ")"

let op1 = function
    Lengthc -> "length"
  | Menos -> "-"

let op2 = function 
    Sum -> "+"
  | Res -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Igual -> "="
  | Mayor -> ">"
  | Menor -> "<"
  | Conc  -> "^"

let rec term = function
    CBool b           -> booleano b
  | CInt e            -> entero e
  | Cadena c          -> "\"" ^ c ^ "\"" 
  | Y e               -> "(Y " ^ term e ^ ")"
  | Par (m,n)         -> "(" ^ term m ^ "," ^ term n ^ ")"
  | Fst m             -> "fst (" ^ term m ^ ")"
  | Snd m             -> "snd (" ^ term m ^ ")"
  | Nil               -> "[]"
  | ConsInt (m,n)     -> "(" ^ term m ^ "::" ^ term n ^ ")"
  | Hd m              -> "(hd " ^ term m ^ ")"
  | Tl m              -> "(tl " ^ term m ^ ")"
  | Null m            -> "(null " ^ term m ^ ")"
  | Op1 (o,m)         -> "(" ^ op1 o ^ " " ^ term m ^ ")"
  | Op2 (o,m,n)       -> "(" ^ term m ^ op2 o ^ term n ^ ")" 
  | Var x             -> x
  | Abs (name,t,l)    -> "(\\" ^ name ^ ":" ^ tipo t ^ "." ^ term l ^ ")"
  | App (t1, t2)      -> "(" ^ term t1 ^ " " ^ term t2 ^ ")"
  | Cond (t1, t2, t3) -> "if " ^ term t1 ^ " then " ^ term t2 ^ 
                                           " else " ^ term t3 
  | LetIn (v,t1,t2) -> "(let " ^ v ^ " = " ^ term t1 ^ " in " ^ term t2 ^ ")"
