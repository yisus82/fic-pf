type token =
    AND
  | EOF
  | LAMBDA
  | DOSDOSIGUAL
  | MAYUSCULAS of (string)
  | MINUSCULAS of (string)
  | PARDER
  | PARIZQ
  | PUNTO

open Parsing
# 4 "parser.mly"
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


(* Line 26, file parser.ml *)
let yytransl_const = [|
  257 (* AND *);
    0 (* EOF *);
  258 (* LAMBDA *);
  259 (* DOSDOSIGUAL *);
  262 (* PARDER *);
  263 (* PARIZQ *);
  264 (* PUNTO *);
    0|]

let yytransl_block = [|
  260 (* MAYUSCULAS *);
  261 (* MINUSCULAS *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\004\000\
\004\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\002\000\001\000\003\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\005\000\000\000\010\000\000\000\
\003\000\000\000\000\000\000\000\002\000\004\000\009\000\000\000\
\006\000\000\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\011\000"

let yysindex = "\005\000\
\001\000\000\000\000\000\000\255\000\000\015\255\000\000\002\000\
\000\000\000\255\001\255\009\255\000\000\000\000\000\000\015\255\
\000\000\015\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000"

let yygindex = "\000\000\
\000\000\250\255\251\255\254\255"

let yytablesize = 266
let yytable = "\012\000\
\003\000\013\000\014\000\007\000\010\000\001\000\014\000\015\000\
\016\000\018\000\004\000\008\000\014\000\005\000\017\000\006\000\
\004\000\000\000\000\000\005\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\004\000\000\000\005\000\005\000\006\000\
\006\000\007\000"

let yycheck = "\006\000\
\000\000\000\000\008\000\000\000\005\001\001\000\012\000\010\000\
\008\001\016\000\002\001\008\001\018\000\005\001\006\001\007\001\
\002\001\255\255\255\255\005\001\255\255\007\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\002\001\255\255\005\001\005\001\007\001\
\007\001\006\001"

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
	Obj.repr((
# 45 "parser.mly"
                             raise ERROR_SINTACTICO ) : Tipos.term))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'aplicacion) in
	Obj.repr((
# 46 "parser.mly"
                          termino _1 ) : Tipos.term))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 49 "parser.mly"
                                        _1 ) : 'aplicacion))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'aplicacion) in
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 50 "parser.mly"
                                        PApp (_1, _2) ) : 'aplicacion))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 53 "parser.mly"
                                                     PVar _1 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 1 : 'aplicacion) in
	Obj.repr((
# 54 "parser.mly"
                                                     _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 2 : 'lista_variables) in
	let _4 = (peek_val parser_env 0 : 'aplicacion) in
	Obj.repr((
# 55 "parser.mly"
                                                     lambda _4 _2 ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 59 "parser.mly"
                                        [_1] ) : 'lista_variables))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : string) in
	let _2 = (peek_val parser_env 0 : 'lista_variables) in
	Obj.repr((
# 60 "parser.mly"
                                        (_1)::(_2) ) : 'lista_variables))
(* Entry termino *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error }
let termino (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : Tipos.term)
(* Line 63, file parser.mly *)







(* Line 203, file parser.ml *)
