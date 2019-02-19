

%{
   open Tipos

   let rec lambda l = function 
         [] -> l
        | x::xs -> Abs ( x, lambda l xs) 

   (* Lambda l xs: devuelve la lambda-expresion l 
      encerrada por abstracciones como variables 
      haya en xs.
   *)

%}

%token LET
%token EOF
%token LAMBDA 
%token DOSIGUAL
%token DOSPUNTOCOMA
%token <string> MAYUSCULAS
%token <string> MINUSCULAS
%token PARDER 
%token PARIZQ 
%token PUNTO 

%right PUNTO 

%start programa
%type <(Tipos.variable * Tipos.term) list> programa

%%

programa : EOF              { raise ERROR_SINTACTICO }
	| definiciones EOF { $1 }
;

definiciones : definicion                { [$1] }
	| definicion  definiciones   { $1 :: $2 }
;

definicion : LET MINUSCULAS  DOSIGUAL aplicacion DOSPUNTOCOMA { ($2,$4) }

aplicacion : simple                   { $1 }
       | aplicacion simple            { App ($1, $2) } 


simple : MINUSCULAS                                { Var $1 }
       | PARIZQ aplicacion PARDER                  { $2 }
       | LAMBDA lista_variables PUNTO aplicacion   { lambda $4 $2 }


lista_variables: 
        MINUSCULAS                    { [$1] }
      | MINUSCULAS lista_variables    { ($1)::($2) } 

%%







