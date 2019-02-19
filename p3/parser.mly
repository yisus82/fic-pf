

%{
   open Tipos

   type pterm = 
             PCons of booleano
           | PConsk of int 
	   | PCadena of string
	   | PNil
	   | PNull of pterm
	   | PConsInt of pterm * pterm
	   | PHd of pterm
	   | PTl of pterm
	   | POp1 of op1 * pterm
	   | POp2 of op2 * pterm * pterm
           | PVar of variable
           | PName of variable 
	   | PPar of pterm * pterm
	   | PFst of pterm
	   | PSnd of pterm 
	   | PAbs of variable * tipo * pterm
	   | PApp of pterm * pterm
	   | PCond of pterm * pterm * pterm
	   | PLetIn of variable * pterm * pterm
	   | PY of pterm

   (* Lambda l xs: devuelve la lambda-expresion l 
      encerrada por abstracciones como variables 
      haya en xs.
   *)

   let nombre variables nombre =
     try List.assoc nombre variables with
       Not_found -> failwith ("No existe variable: " ^ nombre)

   let rec cambiar_nombres definiciones = function
       PCons c   -> CBool c
     | PConsk e  -> CInt e
     | PCadena c -> Cadena c
     | PVar v    -> Var v
     | PNil      -> Nil
     | PConsInt(m,n) -> ConsInt(cambiar_nombres definiciones m,
                                cambiar_nombres definiciones n)
     | PHd m        -> Hd (cambiar_nombres definiciones m)
     | PTl m        -> Tl (cambiar_nombres definiciones m)
     | PNull m        -> Null (cambiar_nombres definiciones m)
     | PFst m    -> Fst (cambiar_nombres definiciones m)
     | PSnd m    -> Snd (cambiar_nombres definiciones m)
     | PPar (m,n) -> Par (cambiar_nombres definiciones m,
                            cambiar_nombres definiciones n)
     | POp1(o,m)   -> Op1 (o,cambiar_nombres definiciones m)
     | POp2(o,m,n) -> Op2 (o,cambiar_nombres definiciones m,
                             cambiar_nombres definiciones n)
     | PName n  -> cambiar_nombres definiciones (nombre definiciones n)
     | PAbs (v,tipo,pt) -> Abs (v,tipo,cambiar_nombres definiciones pt)
     | PApp (pt1,pt2) -> App (cambiar_nombres definiciones pt1,
                              cambiar_nombres definiciones pt2)
     | PCond(pt1,pt2,pt3) -> Cond (cambiar_nombres definiciones pt1,
                                   cambiar_nombres definiciones pt2,
                                   cambiar_nombres definiciones pt3)
     | PLetIn(v,pt1,pt2) -> LetIn (v, cambiar_nombres definiciones pt1,
                                      cambiar_nombres definiciones pt2)
     | PY pt             -> Y (cambiar_nombres definiciones pt)

   let termino = function
       [] -> failwith "No hay nada que leer"
     | defs -> 
	 let (_,expr) = List.hd (List.rev defs) in 
	   cambiar_nombres defs expr
     
     
     

%}

%token DOSPUNTOCOMA
%token AND
%token BARRA
%token BOOL
%token <string> CADENA
%token COMA
%token CORCHETES
%token CORDER
%token CORIZQ
%token CUATROPUNTOS
%token DOSPUNTOS
%token DOSIGUAL
%token ELSE
%token FALSE
%token FLECHA
%token FST
%token <int> ENTERO
%token EOF
%token HD
%token IF
%token IGUAL 
%token IN
%token INT
%token INTLIST
%token LENGTHC
%token LET 
%token MAS
%token MAYOR
%token <string> MAYUSCULAS
%token MENOR
%token MENOS
%token <string> MINUSCULAS
%token NULL 
%token PARDER 
%token PARIZQ 
%token POR
%token PUNTO
%token PUNTOCOMA
%token SND
%token STRING
%token THEN 
%token TILDE
%token TL
%token TRUE
%token Y

%nonassoc MAYOR MENOR IGUAL
%right CUATROPUNTOS  
%left TILDE 
%left MAS MENOS           /* lowest precedence */
%left POR BARRA           /* medium precedence */
%nonassoc MENOSU          /* highest precedence */
%left APLICACION

%right FLECHA

%start programa
%type <(Tipos.variable * Tipos.term) list> programa

%start term
%type <Tipos.term> term

%%

programa : EOF              { raise ERROR_SINTACTICO }
	| definiciones EOF  { $1 }
;

definiciones : definicion                { [$1] } 
	| definicion  definiciones   { $1 :: $2 }
;

definicion : LET MINUSCULAS  DOSIGUAL pterm DOSPUNTOCOMA { ($2,$4) }

op1 : LENGTHC { Lengthc }  
;

term : pterm		{ $1 }
;

pterm : aplicacion                                     { $1 }
      | BARRA MINUSCULAS DOSPUNTOS tipo PUNTO pterm   { Abs ($2,$4,$6) }
      | BARRA MINUSCULAS PUNTO pterm   { Abs ($2,Variable (Fresca.fresca()),$4) }
      |	IF pterm THEN pterm ELSE pterm                 { Cond ($2,$4,$6) }
      |	LET MINUSCULAS IGUAL pterm IN pterm            { LetIn($2,$4,$6) }
      | pterm MAS pterm                                { Op2 (Sum,$1,$3) } 
      | pterm MENOS pterm                              { Op2 (Res,$1,$3) } 
      | pterm POR pterm                                { Op2 (Mul,$1,$3) } 
      | pterm BARRA pterm                              { Op2 (Div,$1,$3) }
      | pterm IGUAL pterm                              { Op2 (Igual,$1,$3) }  
      | pterm MAYOR pterm                              { Op2 (Mayor,$1,$3) }  
      | pterm MENOR pterm                              { Op2 (Menor,$1,$3) }  
      | pterm TILDE pterm                              { Op2 (Conc,$1,$3) }  
      |	pterm CUATROPUNTOS pterm                       { ConsInt($1,$3) }
      |	MENOS pterm %prec MENOSU                       { Op1 (Menos,$2 )}
;

aplicacion : simple                          { $1 }
      |	aplicacion simple %prec APLICACION   { App ($1,$2) }

simple :  
        TRUE                                           { CBool V }
      | FALSE                                          { CBool F }
      |	ENTERO                                         { CInt $1 }
      |	CADENA                                         { Cadena $1 }
      |	CORCHETES                                      { Nil }
      |	PARIZQ pterm COMA pterm PARDER                 { Par ($2,$4) }
      | MINUSCULAS                                     { Var $1 }
      | PARIZQ pterm PARDER                            { $2 }
      |	CORCHETES                                      { Nil }
      |	HD simple                                      { Hd $2 }
      |	TL simple                                      { Tl $2 }
      |	NULL simple                                    { Null $2 }
      |	FST simple                                     { Fst $2 }
      |	SND simple                                     { Snd $2 }
      | Y simple                                       { Y $2 }
      | op1 simple                                     { Op1 ($1,$2) }
      |	lista                                          { $1 }
;

lista : CORIZQ CORDER             { Nil }
      |	CORIZQ elementos CORDER   { $2 }

elementos : pterm                 { ConsInt($1,Nil) }
      |	pterm COMA elementos      { ConsInt($1,$3) }

tipo :  BOOL                                           { Bool }
      |	INT                                            { Int } 
      |	INTLIST                                        { IntList }
      |	STRING                                         { String }
      |	PARIZQ tipo COMA tipo PARDER                   { Producto ($2,$4) }
      |	PARIZQ tipo PARDER                             { $2 }
      |	tipo FLECHA tipo                               { Flecha ($1,$3) }
      |	MAYUSCULAS                                     { Variable ($1) }
;	  

%%







