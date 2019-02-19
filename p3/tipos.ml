
type variable = string

type booleano = F | V 

type tipo = Bool  
          | Int  
	  | IntList
	  | Producto of tipo * tipo
	  | String
          | Flecha of tipo * tipo 
          | Variable of variable

type op1 = Lengthc 
         | Menos 

type op2 = Sum
         | Res
	 | Mul
	 | Div
	 | Mayor
	 | Igual
	 | Menor 
	 | Conc

type term = CBool of booleano
          | CInt of int
	  | Cadena of string
	  | Par of term * term
	  | Fst of term
	  | Snd of term
	  | Nil
	  | Null of term
	  | ConsInt of term * term
	  | Hd of term
	  | Tl of term 
	  | Op1 of op1 * term
	  | Op2 of op2 * term * term
          | Var of variable
	  | Abs of variable * tipo * term
	  | App of term * term
	  | Cond of term * term * term
	  | LetIn of variable * term * term
	  | Y of term
(*	  | Case of term * (term * term) list *)

exception ERROR_LEXICO
exception ERROR_SINTACTICO
exception NO_LIGADA of variable
exception ERROR_DE_TIPOS of string
exception ERROR_DE_EJECUCION of string



