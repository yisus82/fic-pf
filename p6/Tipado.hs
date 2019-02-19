
module Tipado where

import Tipos
import Lector
import Maybe

type Contexto = [(Variable, Tipo)]

anhade :: Variable -> Tipo -> Contexto -> Contexto
anhade v t ctx = (v,t):ctx


tipado :: Term -> Tipo
tipado = tipar []


tipar :: Contexto -> Term -> Tipo
tipar ctx (Var v) = fromJust (lookup v ctx)
tipar ctx (Bool _) = Booleano

