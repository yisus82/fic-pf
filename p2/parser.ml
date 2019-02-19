type token =
    LET
  | EOF
  | LAMBDA
  | DOSIGUAL
  | DOSPUNTOCOMA
  | MAYUSCULAS of (string)
  | MINUSCULAS of (string)
  | PARDER
  | PARIZQ
  | PUNTO

open Parsing
# 4 "parser.mly"
   open Tipos

   let rec lambda l = function 
         [] -> l
        | x::xs -> Abs ( x, lambda l xs) 

   (* Lambda l xs: devuelve la lambda-expresion l 
      encerrada por abstracciones como variables 
      haya en xs.
   *)

(* Line 15, file parser.ml *)
let yytransl_const = [|
  257 (* LET *);
    0 (* EOF *);
  258 (* LAMBDA *);
  259 (* DOSIGUAL *);
  260 (* DOSPUNTOCOMA *);
  263 (* PARDER *);
  264 (* PARIZQ *);
  265 (* PUNTO *);
    0|]

let yytransl_block = [|
  261 (* MAYUSCULAS *);
  262 (* MINUSCULAS *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\004\000\004\000\005\000\
\005\000\005\000\006\000\006\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\002\000\005\000\001\000\002\000\001\000\
\003\000\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\013\000\000\000\000\000\000\000\
\002\000\004\000\000\000\000\000\008\000\000\000\000\000\006\000\
\000\000\000\000\000\000\005\000\007\000\012\000\000\000\009\000\
\000\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\015\000\016\000\018\000"

let yysindex = "\005\000\
\001\000\000\000\010\255\000\000\000\000\017\000\017\255\019\255\
\000\000\000\000\013\255\018\255\000\000\013\255\001\255\000\000\
\018\255\020\255\006\255\000\000\000\000\000\000\013\255\000\000\
\013\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\255"

let yygindex = "\000\000\
\000\000\019\000\000\000\244\255\241\255\010\000"

let yytablesize = 258
let yytable = "\021\000\
\004\000\019\000\012\000\021\000\020\000\001\000\013\000\012\000\
\014\000\021\000\025\000\013\000\024\000\014\000\012\000\008\000\
\009\000\003\000\013\000\010\000\014\000\011\000\010\000\017\000\
\003\000\010\000\022\000\000\000\023\000\011\000\000\000\000\000\
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
\000\000\003\000"

let yycheck = "\015\000\
\000\000\014\000\002\001\019\000\004\001\001\000\006\001\002\001\
\008\001\025\000\023\000\006\001\007\001\008\001\002\001\006\001\
\000\000\001\001\006\001\004\001\008\001\003\001\007\001\006\001\
\000\000\007\000\017\000\255\255\009\001\009\001\255\255\255\255\
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
\255\255\001\001"

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
	Obj.repr((
# 35 "parser.mly"
                              raise ERROR_SINTACTICO ) : (Tipos.variable * Tipos.term) list))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'definiciones) in
	Obj.repr((
# 36 "parser.mly"
                      _1 ) : (Tipos.variable * Tipos.term) list))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'definicion) in
	Obj.repr((
# 39 "parser.mly"
                                           [_1] ) : 'definiciones))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'definicion) in
	let _2 = (peek_val parser_env 0 : 'definiciones) in
	Obj.repr((
# 40 "parser.mly"
                                _1 :: _2 ) : 'definiciones))
; (fun parser_env ->
	let _2 = (peek_val parser_env 3 : string) in
	let _4 = (peek_val parser_env 1 : 'aplicacion) in
	Obj.repr((
# 43 "parser.mly"
                                                                (_2,_4) ) : 'definicion))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 45 "parser.mly"
                                        _1 ) : 'aplicacion))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'aplicacion) in
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 46 "parser.mly"
                                        App (_1, _2) ) : 'aplicacion))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 49 "parser.mly"
                                                     Var _1 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 1 : 'aplicacion) in
	Obj.repr((
# 50 "parser.mly"
                                                     _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 2 : 'lista_variables) in
	let _4 = (peek_val parser_env 0 : 'aplicacion) in
	Obj.repr((
# 51 "parser.mly"
                                                     lambda _4 _2 ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 55 "parser.mly"
                                        [_1] ) : 'lista_variables))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : string) in
	let _2 = (peek_val parser_env 0 : 'lista_variables) in
	Obj.repr((
# 56 "parser.mly"
                                        (_1)::(_2) ) : 'lista_variables))
(* Entry programa *)
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
let programa (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : (Tipos.variable * Tipos.term) list)
(* Line 59, file parser.mly *)







(* Line 210, file parser.ml *)