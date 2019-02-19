
module Tipado where

import Tipos
import Lector
import Maybe

type Contexto = [(Variable, Tipo)]

anhade :: Variable -> Tipo -> Contexto -> Contexto
anhade v t ctx = (v,t):ctx


tipado :: Term -> Tipo
tipado = tipar []

variable_en_tipo :: Variable -> Tipo -> Bool
variable_en_tipo x Booleano = False
variable_en_tipo x Cadena = False
variable_en_tipo x Entero = False
variable_en_tipo x (ListaEnteros) = False
variable_en_tipo x (Variable y) | x == y    = True
				| otherwise = False
variable_en_tipo x (t1 :-> t2) =  (variable_en_tipo x t1) 
                               || (variable_en_tipo x t2)
variable_en_tipo x (Producto t1 t2) =  (variable_en_tipo x t1) 
				    || (variable_en_tipo x t2)


type Sustitucion = [(Variable, Tipo)]

sustitucion_vacia :: Sustitucion
sustitucion_vacia = []


sust_var :: Sustitucion -> Variable -> Tipo
sust_var [] x = Variable x
sust_var ((y,t):sigma') x | x == y = t
			  | otherwise = sust_var sigma' x


sust_tipo :: Sustitucion -> Tipo -> Tipo
sust_tipo sigma Booleano = Booleano
sust_tipo sigma Entero = Entero
sust_tipo sigma Cadena = Cadena
sust_tipo sigma ListaEnteros = ListaEnteros
sust_tipo sigma (Variable v) = sust_var sigma v
sust_tipo sigma (t1 :-> t2) = (sust_tipo sigma t1) :-> (sust_tipo sigma t2)
sust_tipo sigma (Producto t1 t2) = Producto (sust_tipo sigma t1)
				            (sust_tipo sigma t2)


ext_sust :: Variable -> Tipo -> Sustitucion -> Sustitucion
ext_sust y t sigma | Variable y == t' = sigma
		   | variable_en_tipo y t' =  error "Sustitucion recursiva"
		   | otherwise = (y,t'):(map (\ (xi,ti) -> (xi, sust_tipo [(y,t')] ti)) sigma)
		   where t' = sust_tipo sigma t

unificar :: Sustitucion -> Tipo -> Tipo -> Sustitucion
unificar sigma Booleano Booleano = sigma
unificar sigma Entero Entero = sigma
unificar sigma Cadena Cadena = sigma
unificar sigma ListaEnteros ListaEnteros= sigma
unificar sigma (t1 :-> t2) (s1 :-> s2) = unificar (unificar sigma t1 s1) t2 s2
unificar sigma (Variable x) t = case (sust_var sigma x) of
                                  Variable y -> ext_sust y t sigma
                                  s -> unificar sigma s t
unificar sigma t (Variable x) = unificar sigma (Variable x) t
unificar sigma _ _ = error "Unificacion fallida"

resolver_sistema_restricciones [] = sustitucion_vacia
resolver_sistema_restricciones (e:es) = unificar (resolver_sistema_restricciones es) (fst e) (snd e)

tipar :: Contexto -> Term -> Tipo
tipar ctx (Var v) = fromJust (lookup v ctx)
tipar ctx (Bool _) = Booleano
tipar ctx (Literal _) = Cadena
tipar ctx (Numero _) = Entero
tipar ctx (Longitud t) = 
    case (tipar ctx t) of
	 Cadena -> Entero
	 _      -> error "Longitud the algo que no es una cadena"
tipar ctx (Concatenacion t1 t2) = case (tipar ctx t1, tipar ctx t2) of
    (Cadena, Cadena) -> Cadena
    _                -> error "Concatenacion de algo que no son dos cadenas"
tipar ctx (Caracter n c) = case (tipar ctx n, tipar ctx c) of 
	  (Entero, Cadena) -> Cadena
tipar ctx (Igual_enteros t1 t2)= case (tipar ctx t1, tipar ctx t2) of 
          (Entero, Entero) -> Booleano
	  _ -> error "Igual_enteros aplicado a no enteros"
tipar ctx (Igual_cadenas t1 t2)= case (tipar ctx t1, tipar ctx t2) of 
          (Cadena, Cadena) -> Booleano
	  _ -> error "Igual_cadenas aplicado a no cadenas"
tipar ctx (Par t1 t2) = Producto (tipar ctx t1) (tipar ctx t2)
tipar ctx (Primero t) = case tipar ctx t of
                      Producto t1 _ -> t1
		      _ -> error "Primero de algo que no es un par"
tipar ctx (Segundo t) = case tipar ctx t of
                      Producto _ t2 -> t2
		      _ -> error "Segundo de algo que no es un par"
tipar ctx (OperacionEntera _ n1 n2) = case (tipar ctx n1, tipar ctx n2) of
		(Entero, Entero) -> Entero
		_ -> error "Operacion entera sobre no nomuros"
tipar ctx (Y t) = case tipar ctx t of
          t1 :-> t2 | t1 == t2 -> t1
          _ -> error "tipar ctx de definicion recursiva"
tipar ctx (Abs v tipo t) = tipo :-> tipar (anhade v tipo ctx) t

tipar ctx (App t1 t2) = case tipar ctx t1 of
                      t1' :-> t2' | tipar ctx t2 == t1' -> t2'
                      _ -> error "el primer componente de la aplicacion no es aplicable"
tipar ctx (Cond t1 t2 t3) = case (tipar ctx t1, tipar ctx t2, tipar ctx t3) of
			    (Booleano, t1', t2') | t1' == t2' -> t1'
                            _ -> error "En un tipo del condicional"
tipar ctx (LetIn v t1 t2) = tipar ctx' t2
			       where t1' = tipar ctx t1
				     ctx' = anhade v t1' ctx

tipar ctx (Nulo) = ListaEnteros
tipar ctx (ConsEntero t1 t2) = case (tipar ctx t1, tipar ctx t2) of
			   (Entero, ListaEnteros) -> ListaEnteros
                           _ -> error "Cons invalido"
tipar ctx (Cabeza t) = case (tipar ctx t) of
                       ListaEnteros -> Entero
                       _ -> error "Cabeza de una no lista"
tipar ctx (Cola t) = case (tipar ctx t) of
                       ListaEnteros -> ListaEnteros
                       _ -> error "Cola de una no lista"
tipar ctx (EsVacia t) = case (tipar ctx t) of
                       ListaEnteros -> Booleano
                       _ -> error "EsVacia de una no lista"

