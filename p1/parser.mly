

%{
   open Tipos

   type pterm = PVar of variable
	   | PAbs of variable * pterm
	   | PApp of pterm * pterm

   let rec lambda l = function 
         [] -> l
        | x::xs -> PAbs ( x, lambda l xs) 

   (* Lambda l xs: devuelve la lambda-expresion l 
      encerrada por abstracciones como variables 
      haya en xs.
   *)


   let rec termino = function
       PVar v -> Var v
     | PAbs (v,pt) -> Abs (v, termino pt)
     | PApp (pt1,pt2) -> App (termino pt1, termino pt2)


%}

%token AND
%token EOF
%token LAMBDA 
%token DOSDOSIGUAL
%token <string> MAYUSCULAS
%token <string> MINUSCULAS
%token PARDER 
%token PARIZQ 
%token PUNTO 

%right PUNTO 

%start termino
%type <Tipos.term> termino

%%

termino : EOF              { raise ERROR_SINTACTICO }
	| aplicacion  EOF      { termino $1 }
;

aplicacion : simple                   { $1 }
       | aplicacion simple            { PApp ($1, $2) } 


simple : MINUSCULAS                                { PVar $1 }
       | PARIZQ aplicacion PARDER                  { $2 }
       | LAMBDA lista_variables PUNTO aplicacion   { lambda $4 $2 }


lista_variables: 
        MINUSCULAS                    { [$1] }
      | MINUSCULAS lista_variables    { ($1)::($2) } 

%%







