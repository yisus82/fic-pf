
module Lector (leer, leer_fichero) where 

import ParseLib
import Maybe
import Tipos
import IOExts

type Nombre = String
type Definicion = (Nombre, PTerm)
type Diccionario = (Nombre, Term)

data PTerm = PVar Variable
           | Nombre Nombre
           | PBool Booleano
           | PPar PTerm PTerm
           | PPrimero PTerm
           | PSegundo PTerm
           | PNumero Int
           | POperacionEntera Operacion PTerm PTerm
	   | PLiteral String
	   | PLongitud PTerm
	   | PCaracter PTerm PTerm
	   | PConcatenacion PTerm PTerm
	   | PIgual_cadenas PTerm PTerm
	   | PIgual_enteros PTerm PTerm
	   | PY PTerm
           | PAbs Variable Tipo PTerm
           | PApp PTerm PTerm
           | PCond PTerm PTerm PTerm
	   | PLetIn Variable PTerm PTerm
	   | PNulo
	   | PConsEntero PTerm PTerm
	   | PCabeza PTerm
	   | PCola PTerm
	   | PEsVacia PTerm
	   deriving Show

main:: Parser Term
main = do defs <- programa
          return (snd (cambiar vars (sustituir [] defs)))

vars :: [Variable]
vars = ["_X" ++ show i | i <- [1..]]

cambiar :: [Variable] -> Term -> ([Variable], Term)
cambiar vars (Var v) = (vars, Var v)
cambiar vars (Bool b) = (vars, Bool b)
cambiar vars (Literal c) = (vars, Literal c)
cambiar vars (Longitud t) = (vars', Longitud t')
	                    where (vars', t') = cambiar vars t
cambiar vars (Caracter n c) = (vars'', Caracter n' c')
	                      where (vars', n') = cambiar vars n
	                            (vars'', c') = cambiar vars' c
cambiar vars (Concatenacion t1 t2) = (vars'', Concatenacion t1' t2')
	                      where (vars', t1') = cambiar vars t1
	                            (vars'', t2') = cambiar vars' t2
cambiar vars (Igual_enteros t1 t2) = (vars'', Igual_enteros t1' t2')
	                        where (vars', t1') = cambiar vars t1
	                              (vars'', t2') = cambiar vars' t2
cambiar vars (Igual_cadenas t1 t2) = (vars'', Igual_cadenas t1' t2')
	                        where (vars', t1') = cambiar vars t1
	                              (vars'', t2') = cambiar vars' t2
cambiar vars (Igual_cadenas t1 t2) = (vars'', Igual_cadenas t1' t2')
	                        where (vars', t1') = cambiar vars t1
	                              (vars'', t2') = cambiar vars' t2
cambiar vars (Par t1 t2) = (vars'', Par t1' t2')
    where (vars', t1') = cambiar vars t1
	  (vars'', t2') = cambiar vars' t2
cambiar vars (Primero t) = (vars', Primero t')
	                    where (vars', t') = cambiar vars t
cambiar vars (Segundo t) = (vars', Segundo t')
	                    where (vars', t') = cambiar vars t
cambiar vars (Numero n) = (vars, Numero n)
cambiar vars (OperacionEntera op t1 t2) = (vars'', OperacionEntera op t1' t2')
    where (vars', t1') = cambiar vars t1
	  (vars'', t2') = cambiar vars' t2
cambiar vars (Y t) = (vars', Y t')
    where (vars', t') = cambiar vars t
cambiar (v:vars) (Abs n (Variable "_") t) = (vars', Abs n (Variable v) t')
    where (vars', t') = cambiar vars t
cambiar vars (Abs n tipo t) = (vars', Abs n tipo t')
    where (vars', t') = cambiar vars t
cambiar vars (App t1 t2) = (vars'', App t1' t2')
    where (vars', t1') = cambiar vars t1
	  (vars'', t2') = cambiar vars' t2
cambiar vars (Cond t1 t2 t3) = (vars''', Cond t1' t2' t3')
    where (vars', t1') = cambiar vars t1
	  (vars'', t2') = cambiar vars' t2
	  (vars''', t3') = cambiar vars' t3
cambiar vars (Nulo) = (vars, Nulo)
cambiar vars (ConsEntero t1 t2) = (vars'', ConsEntero t1' t2')
    where (vars', t1') = cambiar vars t1
	  (vars'', t2') = cambiar vars' t2
cambiar vars (Cabeza t) = (vars', Cabeza t')
    where (vars', t') = cambiar vars t
cambiar vars (Cola t) = (vars', Cola t')
    where (vars', t') = cambiar vars t
cambiar vars (EsVacia t) = (vars', EsVacia t')
    where (vars', t') = cambiar vars t
cambiar vars (LetIn v t1 t2) = (vars'', LetIn v t1' t2')
    where (vars', t1') = cambiar vars t1
	  (vars'', t2') = cambiar vars' t2

sustituir :: [Diccionario] -> [Definicion] -> Term
sustituir dict []     = snd (head dict)
sustituir dict ((name, term):ds) = sustituir ((name, term'):dict) ds
    where term' = subst term dict

subst :: PTerm -> [Diccionario] -> Term
subst (PVar v)      dict = Var v
subst (Nombre n)    dict = fromJust (lookup n dict)
subst (PNumero n)   dict = Numero n
subst (POperacionEntera op m n) dict = OperacionEntera op (subst m dict) (subst n dict)
subst (PBool b)     dict = Bool b
subst (PPar m n)    dict = Par (subst m dict) (subst n dict)
subst (PPrimero m)  dict = Primero (subst m dict)
subst (PSegundo m)  dict = Segundo (subst m dict)
subst (PLiteral c)  dict = Literal c
subst (PLongitud c) dict = Longitud (subst c dict)
subst (PCaracter n c)  dict = Caracter (subst n dict) (subst c dict)
subst (PIgual_cadenas t1 t2)  dict = Igual_cadenas (subst t1 dict) (subst t2 dict)
subst (PIgual_enteros t1 t2)  dict = Igual_enteros (subst t1 dict) (subst t2 dict)
subst (PConcatenacion t1 t2)  dict = Concatenacion (subst t1 dict) (subst t2 dict)
subst (PY m)        dict = Y (subst m dict)
subst (PAbs v t m)  dict = Abs v t (subst m dict)
subst (PApp m n)    dict = App (subst m dict) (subst n dict)
subst (PCond m n p) dict = Cond (subst m dict) (subst n dict) (subst p dict)
subst (PLetIn v m n) dict = LetIn v (subst m dict) (subst n dict)
subst (PNulo)       dict = Nulo
subst (PConsEntero m n) dict = ConsEntero (subst m dict) (subst n dict)
subst (PCabeza m)   dict = Cabeza (subst m dict)
subst (PCola m)     dict = Cola (subst m dict)
subst (PEsVacia m)  dict = EsVacia (subst m dict)

programa :: Parser [Definicion]
programa = many definicion

definicion :: Parser Definicion
definicion = do nombre' <- nombre_
                symbol "::="
                term' <- term
		symbol ";"
                return (nombre', term')

term :: Parser PTerm
term = repeat2 term' 

term' :: Parser PTerm
term' = bool +++ numero +++ nulo +++ es_vacia +++ cabeza +++ cola 
	+++ literal +++ caracter +++ longitud  +++ rec 
	+++ primero +++ segundo +++ condicional +++ let_in 
	+++ paren_term +++ lambda +++ lambda' +++ nombre +++ var 
	+++ operacion +++ par +++ igual_enteros +++ igual_cadenas 
	+++ concatenacion +++ cons +++ lista

igual_cadenas :: Parser PTerm
igual_cadenas = do symbol "("
		   term1 <- term
		   symbol "=c"
		   term2 <- term
		   symbol ")"
		   return (PIgual_cadenas term1 term2)

concatenacion :: Parser PTerm
concatenacion = do symbol "("
		   term1 <- term
		   symbol "++"
		   term2 <- term
		   symbol ")"
		   return (PConcatenacion term1 term2)


igual_enteros :: Parser PTerm
igual_enteros = do symbol "("
		   term1 <- term
		   symbol "=e"
		   term2 <- term
		   symbol ")"
		   return (PIgual_enteros term1 term2)

literal :: Parser PTerm
literal = do symbol "'"
	     lit <- many alphanum
	     symbol "'"
	     return (PLiteral lit)

caracter :: Parser PTerm
caracter = do symbol "caracter"
	      n <- term'
	      c <- term'
	      return (PCaracter n c)

longitud :: Parser PTerm
longitud = do symbol "longitud"
	      t <- term'
	      return (PLongitud t)

nulo :: Parser PTerm
nulo = do symbol "[]"
	  return PNulo

cons :: Parser PTerm
cons = do symbol "("
	  m <- term
	  symbol "::"
	  n <- term
	  symbol ")"
	  return (PConsEntero m n)

lista :: Parser PTerm
lista = do symbol "["
	   terms <- sepby term (symbol ",")
	   symbol "]"
	   return (haz_lista terms)

haz_lista :: [PTerm] -> PTerm
haz_lista [x] = PConsEntero x PNulo
haz_lista (x:xs) = PConsEntero x (haz_lista xs)

cabeza :: Parser PTerm
cabeza = do symbol "cabeza"
	    t <- term'
	    return (PCabeza t)

cola :: Parser PTerm
cola = do symbol "cola"
	  t <- term'
	  return (PCola t)

es_vacia :: Parser PTerm
es_vacia = do symbol "es_vacia"
	      t <- term'
	      return (PEsVacia t)

rec :: Parser PTerm
rec = do symbol "rec"
	 t <- term'
         return (PY t)

par :: Parser PTerm
par = do symbol "("
         t1 <- term
         symbol ","
         t2 <- term
         symbol ")"
         return (PPar t1 t2)

primero :: Parser PTerm
primero = do symbol "primero"
             t <- term'
             return (PPrimero t)

segundo :: Parser PTerm
segundo = do symbol "segundo"
             t <- term'
             return (PSegundo t)

operacion :: Parser PTerm
operacion = do symbol "("
               t1 <- term
               op <- operador
               t2 <- term
               symbol ")"
               return (POperacionEntera op t1 t2)

operador :: Parser Operacion
operador = suma +++ resta +++ multiplicacion +++ division +++ modulo

suma :: Parser Operacion
suma = do symbol "+"
          return Suma

resta :: Parser Operacion
resta = do symbol "-"
           return Resta

multiplicacion :: Parser Operacion
multiplicacion = do symbol "*"
                    return Multiplicacion

division :: Parser Operacion
division = do symbol "/"
              return Division

modulo :: Parser Operacion
modulo = do symbol "%"
            return Modulo

lambda :: Parser PTerm
lambda = do symbol "fun"
            v <- variable
	    symbol "."
	    t <- term
	    return (PAbs v (Variable "_") t)

lambda' :: Parser PTerm
lambda' = do symbol "fun"
             v <- variable
             symbol ":"
             tipo_var <- tipo
    	     symbol "."
	     t <- term
	     return (PAbs v tipo_var t)


let_in :: Parser PTerm
let_in = do symbol "let"
	    v <- variable
	    symbol ":="
            m <- term'
    	    symbol "in"
	    n <- term
	    return (PLetIn v m n)

tipo :: Parser Tipo
tipo = flecha

flecha :: Parser Tipo
flecha = do ts <- sepby1 tipo' (symbol "->") 
            return (foldl1 (:->) ts)

tipo' :: Parser Tipo
tipo' = booleano +++ entero +++ cadena +++ lista_enteros +++ var_tipo 
	+++ tipo_par +++ producto

producto :: Parser Tipo
producto  = do symbol "("
               t1 <- tipo
               symbol ","
               t2 <- tipo
               symbol ")"
               return (Producto t1 t2)

tipo_par :: Parser Tipo
tipo_par = do symbol "("
              t <- tipo
              symbol ")"
              return t

booleano :: Parser Tipo
booleano = do symbol "Booleano"
              return Booleano

cadena :: Parser Tipo
cadena = do symbol "Cadena"
	    return Cadena

entero :: Parser Tipo
entero = do symbol "Entero"
            return Entero

lista_enteros :: Parser Tipo
lista_enteros = do symbol "ListaEnteros"
		   return ListaEnteros

var_tipo :: Parser Tipo
var_tipo = do v <- variable
              return (Variable v)

paren_term :: Parser PTerm
paren_term = do symbol "(" 
		t <- term
		symbol ")"
                return t

condicional :: Parser PTerm
condicional = do symbol "if"
                 cond <- term'
                 symbol "then"
                 then_term <- term'
                 symbol "else"
                 else_term <- term'
                 return (PCond cond then_term else_term)

nombre :: Parser PTerm
nombre = do n <- nombre_
            return (Nombre n)

var :: Parser PTerm
var = do v <- variable
         return (PVar v)

numero :: Parser PTerm
numero = do n <- natural
            return (PNumero n)

bool :: Parser PTerm
bool = verdadero +++ falso

verdadero :: Parser PTerm
verdadero = do symbol "Verdad"
               return (PBool V)

falso :: Parser PTerm
falso = do symbol "Falso"
           return (PBool F)

variable :: Parser Variable
variable = token minusculas

nombre_ :: Parser String
nombre_ = token mayusculas

repeat2 :: Parser PTerm -> Parser PTerm
repeat2 p = do {x <- p; rest x}
           where
               rest x = do {y <- p; rest (PApp x y)} +++ return x

minusculas :: Parser String
minusculas = do {x <- lower; xs <- many alphanum; return (x:xs)}

mayusculas :: Parser String
mayusculas = do {x <- upper; xs <- many alphanum; return (x:xs)}



leer :: String -> Term
leer = fst . head . papply (parse main)

leer_fichero' :: String -> IO Term
leer_fichero' fichero = do p <- readFile fichero
                           return (leer p)

leer_fichero :: String -> Term
leer_fichero s = unsafePerformIO (leer_fichero' s)

