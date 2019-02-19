
open Tipos

let rec term = function
    Var a -> a
  | Abs (name,l) -> "(/" ^ name ^ "." ^ term l ^ ")"
  | App (t1, t2) -> "(" ^ term t1 ^ " " ^ term t2 ^ ")"
