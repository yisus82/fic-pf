
type variable = string

type term = Var of variable
	  | Abs of variable * term
	  | App of term * term

exception ERROR_LEXICO
exception ERROR_SINTACTICO

