
module Ejemplos where

import Tipos
import Lector
import Lambda

i  = leer "/x.x"
i' = leer "/y.y"
k  = leer "/x.y"
t  = leer "/x y.x"
f  = leer "/x y.x"
a  = leer "/x. x x"

e1 = leer "/x.x z"
e2 = leer "(/x. a x)((/y. b y) c)"
e3 = leer "(/x.x (/y.y x))"
e4 = leer "(/x.x x) (/x.x x)"
e5 = leer "(/x. x x) y z"
e6 = leer "(/x. a) (/x. x x)"

true' =  "(/ x y . x)"
false' =  "(/ x y . y)"
if'    =  "(/ p x y. p x y)" 

not' = "(/ p. " ++ if' ++ " p " ++ false' ++ true' ++ ")"
and' = "(/ p q. " ++ if' ++ "p q " ++ false' ++ ")"
or' =  "(/ p q. " ++ if' ++ " p " ++ true' ++ " q )"

par = "(/ x y f. f x y)"
primero = "(/ p. p " ++ true' ++ " )"
segundo = "(/ p. p " ++ false' ++ " )"

par' x y = "(" ++ par ++ x ++ y ++ ")"

primero' p = "(" ++ primero ++ p ++ ")"
segundo' p = "(" ++ segundo ++ p ++ ")"

nat_of_int :: Integer -> Term
nat_of_int n = Abs "f" (Abs "x" (nat2int n))

nat2int 0 = Var "x"
nat2int n = App (Var "f") (nat2int (n-1))


int_of_nat :: Term -> Integer
int_of_nat n = int2nat 0 n'
    where (Abs f (Abs x n')) = n

int2nat accum (Var x) = accum
int2nat accum (App (Var f) n ) = int2nat (accum +1) n

suma' = "(/ m n f x. m f (n f x))"
mult' = "(/ m n f x. m (n f) x)"
exp'  = "(/ m n f x. n m f x)"

sum' x y = int_of_nat (fn (App (App (leer suma') nat_x) nat_y))
           where nat_x = nat_of_int x
                 nat_y = nat_of_int y

sum2 x y = whnf (App (App (leer suma') nat_x) nat_y)
           where nat_x = nat_of_int x
                 nat_y = nat_of_int y


mul' x y = int_of_nat (fn (App (App (leer mult') nat_x) nat_y))
           where nat_x = nat_of_int x
                 nat_y = nat_of_int y

suc' = "(/ n f x. f (n f x))"
es_cero = "(/n. n (/ x. "++ false' ++ ") " ++ true' ++ ")"

cero = "(/f x. x)"
uno  =  "(/f x. f x)"
dos  =  "(/f x. f (f x))"

prefn = "(/ f p. "++ par ++ "( f ( " ++ primero ++" p)) ( " ++ primero ++ " p))"
pred' = "(/n f x. " ++ segundo ++ "(n ( "++ prefn ++ " f) ( "++ par ++" x x)))"
sub'  = "(/ m n. n " ++ pred' ++ " m)"

pre' n = int_of_nat (fn (App (leer pred') (nat_of_int n)))

pre2 n =  (App (leer pred') (nat_of_int n))
sub2 n m = int_of_nat (fn (App (App (leer sub') (nat_of_int n)) (nat_of_int m)))


nil = "("++ par ++ true' ++ true' ++")"
cons = "(/ x y. "++ par ++ false' ++ "(" ++ par ++" x y ))"
es_nil = primero
cabeza = "(/z. "++ primero ++ "("++ segundo++" z))"
cola = "(/z. "++ segundo ++ "("++ segundo++" z))"

y' = "(/f.(/x. f (x x))(/x.f (x x)))"

-- factorial
fact = "(" ++ y' ++ "(/g n." ++ if' ++ "(" ++  es_cero ++ "n)"++ uno 
           ++ "(" ++ mult' ++ "n ( g (" ++ pred' ++" n )))))"

fact2 :: Integer -> Integer
fact2 n = int_of_nat (fn (App (leer fact) (nat_of_int n)))

-- append
-- fibonacci
-- lista de ceros

ceros = "("++ y' ++ "(/g." ++ cons ++ cero ++ "g))"


-- ev (cabeza ++ "(" ++ cola ++ "(" ++ cola ++ceros ++ "))")
