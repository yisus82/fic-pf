
module Tipos where

type Variable = String

data Booleano = F 
              | V
              deriving (Show, Eq)

data Tipo = Booleano
          | Entero
	  | Cadena
	  | ListaEnteros
          | Tipo :-> Tipo
          | Producto Tipo Tipo
          | Variable String
          deriving (Show, Eq)

type Cadena = String

data Operacion = Suma | Resta | Multiplicacion | Division | Modulo | Menor
               deriving (Show, Eq)

data Term = Var Variable
          | Bool Booleano
	  | Literal Cadena
	  | Longitud Term
	  | Concatenacion Term Term
	  | Caracter Term Term
	  | Igual_enteros Term Term
	  | Igual_cadenas Term Term
          | Par Term Term
          | Primero Term
          | Segundo Term
          | Numero Int
          | OperacionEntera Operacion Term Term
	  | Y Term
          | Abs Variable Tipo Term
          | App Term Term
          | Cond Term Term Term
	  | LetIn Variable Term Term
	  | Nulo
	  | ConsEntero Term Term
	  | Cabeza Term
	  | Cola Term
	  | EsVacia Term
	  deriving (Show, Eq)
