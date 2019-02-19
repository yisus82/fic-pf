type token =
    DOSPUNTOCOMA
  | AND
  | BARRA
  | BOOL
  | CADENA of (string)
  | COMA
  | CORCHETES
  | CORDER
  | CORIZQ
  | CUATROPUNTOS
  | DOSPUNTOS
  | DOSIGUAL
  | ELSE
  | FALSE
  | FLECHA
  | FST
  | ENTERO of (int)
  | EOF
  | HD
  | IF
  | IGUAL
  | IN
  | INT
  | INTLIST
  | LENGTHC
  | LET
  | MAS
  | MAYOR
  | MAYUSCULAS of (string)
  | MENOR
  | MENOS
  | MINUSCULAS of (string)
  | NULL
  | PARDER
  | PARIZQ
  | POR
  | PUNTO
  | PUNTOCOMA
  | SND
  | STRING
  | THEN
  | TILDE
  | TL
  | TRUE
  | Y

open Parsing
# 4 "parser.mly"
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
     
     
     

(* Line 75, file parser.ml *)
let yytransl_const = [|
  257 (* DOSPUNTOCOMA *);
  258 (* AND *);
  259 (* BARRA *);
  260 (* BOOL *);
  262 (* COMA *);
  263 (* CORCHETES *);
  264 (* CORDER *);
  265 (* CORIZQ *);
  266 (* CUATROPUNTOS *);
  267 (* DOSPUNTOS *);
  268 (* DOSIGUAL *);
  269 (* ELSE *);
  270 (* FALSE *);
  271 (* FLECHA *);
  272 (* FST *);
    0 (* EOF *);
  274 (* HD *);
  275 (* IF *);
  276 (* IGUAL *);
  277 (* IN *);
  278 (* INT *);
  279 (* INTLIST *);
  280 (* LENGTHC *);
  281 (* LET *);
  282 (* MAS *);
  283 (* MAYOR *);
  285 (* MENOR *);
  286 (* MENOS *);
  288 (* NULL *);
  289 (* PARDER *);
  290 (* PARIZQ *);
  291 (* POR *);
  292 (* PUNTO *);
  293 (* PUNTOCOMA *);
  294 (* SND *);
  295 (* STRING *);
  296 (* THEN *);
  297 (* TILDE *);
  298 (* TL *);
  299 (* TRUE *);
  300 (* Y *);
    0|]

let yytransl_block = [|
  261 (* CADENA *);
  273 (* ENTERO *);
  284 (* MAYUSCULAS *);
  287 (* MINUSCULAS *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\006\000\002\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\011\000\011\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\002\000\005\000\001\000\001\000\001\000\
\006\000\004\000\006\000\006\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\001\000\002\000\
\001\000\001\000\001\000\001\000\001\000\005\000\001\000\003\000\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\001\000\002\000\003\000\001\000\003\000\001\000\001\000\001\000\
\001\000\005\000\003\000\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\000\000\054\000\000\000\000\000\
\000\000\028\000\029\000\000\000\026\000\000\000\027\000\000\000\
\000\000\006\000\000\000\000\000\031\000\000\000\000\000\000\000\
\000\000\025\000\000\000\055\000\000\000\000\000\000\000\023\000\
\041\000\000\000\002\000\004\000\000\000\042\000\000\000\000\000\
\037\000\034\000\000\000\000\000\022\000\036\000\000\000\038\000\
\035\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\000\024\000\000\000\000\000\000\000\
\000\000\043\000\000\000\000\000\000\000\032\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\
\046\000\047\000\048\000\053\000\000\000\049\000\000\000\000\000\
\045\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\000\000\030\000\000\000\051\000\000\000\000\000\000\000\
\000\000\000\000\050\000"

let yydgoto = "\003\000\
\006\000\028\000\007\000\008\000\039\000\030\000\031\000\087\000\
\032\000\033\000\040\000"

let yysindex = "\069\000\
\007\000\121\255\000\000\000\000\233\254\000\000\011\000\244\254\
\241\254\000\000\000\000\073\255\000\000\194\255\000\000\194\255\
\121\255\000\000\245\254\121\255\000\000\194\255\121\255\194\255\
\194\255\000\000\194\255\000\000\254\000\194\255\194\255\000\000\
\000\000\010\255\000\000\000\000\006\255\000\000\213\255\016\255\
\000\000\000\000\172\000\026\255\000\000\000\000\255\254\000\000\
\000\000\000\000\121\255\121\255\121\255\121\255\121\255\121\255\
\121\255\121\255\121\255\000\000\000\000\121\255\139\255\121\255\
\121\255\000\000\121\255\121\255\121\255\000\000\000\000\033\255\
\033\255\003\255\033\255\033\255\003\255\000\000\009\255\165\255\
\000\000\000\000\000\000\000\000\139\255\000\000\251\254\254\000\
\000\000\201\000\214\000\236\000\000\000\008\255\139\255\121\255\
\121\255\121\255\000\000\139\255\000\000\043\255\254\000\254\000\
\254\000\012\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\048\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\061\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\119\000\
\004\000\037\000\047\000\083\000\073\000\000\000\109\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\120\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\253\254\150\000\156\000\
\160\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\064\000\000\000\254\255\000\000\000\000\240\255\
\016\002\000\000\010\000"

let yytablesize = 559
let yytable = "\029\000\
\008\000\051\000\052\000\017\000\069\000\051\000\004\000\034\000\
\052\000\095\000\035\000\051\000\005\000\100\000\043\000\037\000\
\063\000\045\000\053\000\044\000\047\000\062\000\095\000\066\000\
\054\000\055\000\095\000\056\000\057\000\052\000\096\000\070\000\
\052\000\058\000\054\000\051\000\013\000\058\000\057\000\059\000\
\101\000\064\000\052\000\058\000\107\000\068\000\018\000\003\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\095\000\054\000\080\000\007\000\088\000\057\000\044\000\
\090\000\091\000\092\000\058\000\094\000\001\000\002\000\036\000\
\014\000\059\000\089\000\009\000\000\000\010\000\102\000\011\000\
\038\000\012\000\019\000\106\000\000\000\000\000\013\000\000\000\
\014\000\015\000\016\000\017\000\000\000\103\000\104\000\105\000\
\018\000\019\000\000\000\000\000\000\000\000\000\020\000\021\000\
\022\000\000\000\023\000\000\000\020\000\000\000\024\000\000\000\
\000\000\000\000\025\000\026\000\027\000\000\000\021\000\010\000\
\000\000\000\000\000\000\009\000\000\000\010\000\000\000\011\000\
\000\000\012\000\000\000\000\000\000\000\000\000\013\000\000\000\
\014\000\015\000\016\000\017\000\000\000\000\000\081\000\000\000\
\018\000\019\000\000\000\000\000\000\000\009\000\020\000\021\000\
\022\000\000\000\023\000\011\000\000\000\000\000\024\000\012\000\
\082\000\083\000\025\000\026\000\027\000\093\000\084\000\051\000\
\000\000\000\000\000\000\000\000\085\000\000\000\052\000\000\000\
\000\000\086\000\000\000\000\000\000\000\000\000\000\000\000\000\
\053\000\000\000\000\000\000\000\000\000\000\000\054\000\055\000\
\000\000\056\000\057\000\000\000\000\000\000\000\010\000\058\000\
\011\000\000\000\012\000\000\000\000\000\059\000\000\000\013\000\
\000\000\014\000\015\000\016\000\000\000\000\000\000\000\051\000\
\000\000\018\000\065\000\000\000\000\000\000\000\052\000\000\000\
\021\000\022\000\000\000\023\000\000\000\000\000\000\000\024\000\
\053\000\000\000\000\000\025\000\026\000\027\000\054\000\055\000\
\000\000\056\000\057\000\000\000\000\000\000\000\000\000\058\000\
\000\000\000\000\000\000\000\000\000\000\059\000\000\000\000\000\
\000\000\008\000\000\000\008\000\017\000\000\000\008\000\000\000\
\008\000\017\000\008\000\017\000\000\000\008\000\000\000\000\000\
\017\000\000\000\000\000\000\000\008\000\008\000\000\000\000\000\
\017\000\000\000\008\000\008\000\000\000\008\000\008\000\005\000\
\000\000\008\000\000\000\008\000\017\000\013\000\000\000\000\000\
\008\000\008\000\013\000\017\000\013\000\000\000\013\000\018\000\
\000\000\013\000\000\000\000\000\018\000\000\000\018\000\000\000\
\013\000\013\000\000\000\018\000\000\000\000\000\013\000\013\000\
\000\000\013\000\013\000\018\000\000\000\013\000\000\000\000\000\
\000\000\014\000\000\000\000\000\013\000\013\000\014\000\018\000\
\014\000\000\000\014\000\019\000\000\000\014\000\018\000\000\000\
\019\000\000\000\019\000\000\000\014\000\014\000\000\000\019\000\
\000\000\000\000\014\000\014\000\000\000\014\000\014\000\019\000\
\000\000\014\000\000\000\000\000\000\000\020\000\000\000\000\000\
\014\000\014\000\020\000\019\000\020\000\000\000\020\000\021\000\
\010\000\020\000\019\000\000\000\021\000\010\000\021\000\010\000\
\020\000\020\000\000\000\021\000\010\000\000\000\000\000\020\000\
\000\000\020\000\021\000\021\000\010\000\020\000\000\000\000\000\
\000\000\021\000\000\000\021\000\020\000\020\000\009\000\021\000\
\010\000\000\000\000\000\009\000\011\000\009\000\021\000\010\000\
\012\000\011\000\009\000\011\000\000\000\012\000\000\000\012\000\
\011\000\000\000\009\000\000\000\012\000\000\000\051\000\000\000\
\011\000\000\000\000\000\000\000\012\000\052\000\009\000\000\000\
\000\000\000\000\000\000\000\000\011\000\009\000\000\000\053\000\
\012\000\000\000\000\000\011\000\000\000\054\000\055\000\012\000\
\056\000\057\000\000\000\051\000\000\000\000\000\058\000\000\000\
\000\000\000\000\052\000\067\000\059\000\097\000\000\000\000\000\
\051\000\000\000\000\000\000\000\053\000\000\000\000\000\052\000\
\000\000\000\000\054\000\055\000\000\000\056\000\057\000\000\000\
\000\000\053\000\098\000\058\000\000\000\000\000\051\000\054\000\
\055\000\059\000\056\000\057\000\000\000\052\000\000\000\000\000\
\058\000\000\000\000\000\000\000\000\000\000\000\059\000\053\000\
\051\000\000\000\000\000\000\000\000\000\054\000\055\000\052\000\
\056\000\057\000\000\000\000\000\099\000\000\000\058\000\000\000\
\000\000\053\000\000\000\000\000\059\000\000\000\000\000\054\000\
\055\000\000\000\056\000\057\000\000\000\041\000\000\000\042\000\
\058\000\000\000\000\000\000\000\000\000\046\000\059\000\048\000\
\049\000\000\000\050\000\000\000\000\000\060\000\061\000"

let yycheck = "\002\000\
\000\000\003\001\006\001\000\000\006\001\003\001\000\000\031\001\
\010\001\015\001\000\000\003\001\025\001\006\001\017\000\031\001\
\011\001\020\000\020\001\031\001\023\000\012\001\015\001\008\001\
\026\001\027\001\015\001\029\001\030\001\033\001\036\001\033\001\
\036\001\035\001\026\001\003\001\000\000\035\001\030\001\041\001\
\033\001\036\001\010\001\035\001\033\001\020\001\000\000\000\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\015\001\026\001\062\000\000\000\064\000\030\001\008\001\
\067\000\068\000\069\000\035\001\085\000\001\000\002\000\008\000\
\000\000\041\001\065\000\003\001\255\255\005\001\095\000\007\001\
\008\001\009\001\000\000\100\000\255\255\255\255\014\001\255\255\
\016\001\017\001\018\001\019\001\255\255\096\000\097\000\098\000\
\024\001\025\001\255\255\255\255\255\255\255\255\030\001\031\001\
\032\001\255\255\034\001\255\255\000\000\255\255\038\001\255\255\
\255\255\255\255\042\001\043\001\044\001\255\255\000\000\000\000\
\255\255\255\255\255\255\003\001\255\255\005\001\255\255\007\001\
\255\255\009\001\255\255\255\255\255\255\255\255\014\001\255\255\
\016\001\017\001\018\001\019\001\255\255\255\255\004\001\255\255\
\024\001\025\001\255\255\255\255\255\255\000\000\030\001\031\001\
\032\001\255\255\034\001\000\000\255\255\255\255\038\001\000\000\
\022\001\023\001\042\001\043\001\044\001\001\001\028\001\003\001\
\255\255\255\255\255\255\255\255\034\001\255\255\010\001\255\255\
\255\255\039\001\255\255\255\255\255\255\255\255\255\255\255\255\
\020\001\255\255\255\255\255\255\255\255\255\255\026\001\027\001\
\255\255\029\001\030\001\255\255\255\255\255\255\005\001\035\001\
\007\001\255\255\009\001\255\255\255\255\041\001\255\255\014\001\
\255\255\016\001\017\001\018\001\255\255\255\255\255\255\003\001\
\255\255\024\001\006\001\255\255\255\255\255\255\010\001\255\255\
\031\001\032\001\255\255\034\001\255\255\255\255\255\255\038\001\
\020\001\255\255\255\255\042\001\043\001\044\001\026\001\027\001\
\255\255\029\001\030\001\255\255\255\255\255\255\255\255\035\001\
\255\255\255\255\255\255\255\255\255\255\041\001\255\255\255\255\
\255\255\001\001\255\255\003\001\001\001\255\255\006\001\255\255\
\008\001\006\001\010\001\008\001\255\255\013\001\255\255\255\255\
\013\001\255\255\255\255\255\255\020\001\021\001\255\255\255\255\
\021\001\255\255\026\001\027\001\255\255\029\001\030\001\025\001\
\255\255\033\001\255\255\035\001\033\001\001\001\255\255\255\255\
\040\001\041\001\006\001\040\001\008\001\255\255\010\001\001\001\
\255\255\013\001\255\255\255\255\006\001\255\255\008\001\255\255\
\020\001\021\001\255\255\013\001\255\255\255\255\026\001\027\001\
\255\255\029\001\030\001\021\001\255\255\033\001\255\255\255\255\
\255\255\001\001\255\255\255\255\040\001\041\001\006\001\033\001\
\008\001\255\255\010\001\001\001\255\255\013\001\040\001\255\255\
\006\001\255\255\008\001\255\255\020\001\021\001\255\255\013\001\
\255\255\255\255\026\001\027\001\255\255\029\001\030\001\021\001\
\255\255\033\001\255\255\255\255\255\255\001\001\255\255\255\255\
\040\001\041\001\006\001\033\001\008\001\255\255\010\001\001\001\
\001\001\013\001\040\001\255\255\006\001\006\001\008\001\008\001\
\020\001\021\001\255\255\013\001\013\001\255\255\255\255\027\001\
\255\255\029\001\020\001\021\001\021\001\033\001\255\255\255\255\
\255\255\027\001\255\255\029\001\040\001\041\001\001\001\033\001\
\033\001\255\255\255\255\006\001\001\001\008\001\040\001\040\001\
\001\001\006\001\013\001\008\001\255\255\006\001\255\255\008\001\
\013\001\255\255\021\001\255\255\013\001\255\255\003\001\255\255\
\021\001\255\255\255\255\255\255\021\001\010\001\033\001\255\255\
\255\255\255\255\255\255\255\255\033\001\040\001\255\255\020\001\
\033\001\255\255\255\255\040\001\255\255\026\001\027\001\040\001\
\029\001\030\001\255\255\003\001\255\255\255\255\035\001\255\255\
\255\255\255\255\010\001\040\001\041\001\013\001\255\255\255\255\
\003\001\255\255\255\255\255\255\020\001\255\255\255\255\010\001\
\255\255\255\255\026\001\027\001\255\255\029\001\030\001\255\255\
\255\255\020\001\021\001\035\001\255\255\255\255\003\001\026\001\
\027\001\041\001\029\001\030\001\255\255\010\001\255\255\255\255\
\035\001\255\255\255\255\255\255\255\255\255\255\041\001\020\001\
\003\001\255\255\255\255\255\255\255\255\026\001\027\001\010\001\
\029\001\030\001\255\255\255\255\033\001\255\255\035\001\255\255\
\255\255\020\001\255\255\255\255\041\001\255\255\255\255\026\001\
\027\001\255\255\029\001\030\001\255\255\014\000\255\255\016\000\
\035\001\255\255\255\255\255\255\255\255\022\000\041\001\024\000\
\025\000\255\255\027\000\255\255\255\255\030\000\031\000"

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
	Obj.repr((
# 141 "parser.mly"
                              raise ERROR_SINTACTICO ) : (Tipos.variable * Tipos.term) list))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'definiciones) in
	Obj.repr((
# 142 "parser.mly"
                       _1 ) : (Tipos.variable * Tipos.term) list))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'definicion) in
	Obj.repr((
# 145 "parser.mly"
                                           [_1] ) : 'definiciones))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'definicion) in
	let _2 = (peek_val parser_env 0 : 'definiciones) in
	Obj.repr((
# 146 "parser.mly"
                                _1 :: _2 ) : 'definiciones))
; (fun parser_env ->
	let _2 = (peek_val parser_env 3 : string) in
	let _4 = (peek_val parser_env 1 : 'pterm) in
	Obj.repr((
# 149 "parser.mly"
                                                           (_2,_4) ) : 'definicion))
; (fun parser_env ->
	Obj.repr((
# 151 "parser.mly"
                Lengthc ) : 'op1))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 154 "parser.mly"
                _1 ) : Tipos.term))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'aplicacion) in
	Obj.repr((
# 157 "parser.mly"
                                                         _1 ) : 'pterm))
; (fun parser_env ->
	let _2 = (peek_val parser_env 4 : string) in
	let _4 = (peek_val parser_env 2 : 'tipo) in
	let _6 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 158 "parser.mly"
                                                        Abs (_2,_4,_6) ) : 'pterm))
; (fun parser_env ->
	let _2 = (peek_val parser_env 2 : string) in
	let _4 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 159 "parser.mly"
                                         Abs (_2,Variable (Fresca.fresca()),_4) ) : 'pterm))
; (fun parser_env ->
	let _2 = (peek_val parser_env 4 : 'pterm) in
	let _4 = (peek_val parser_env 2 : 'pterm) in
	let _6 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 160 "parser.mly"
                                                         Cond (_2,_4,_6) ) : 'pterm))
; (fun parser_env ->
	let _2 = (peek_val parser_env 4 : string) in
	let _4 = (peek_val parser_env 2 : 'pterm) in
	let _6 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 161 "parser.mly"
                                                         LetIn(_2,_4,_6) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 162 "parser.mly"
                                                         Op2 (Sum,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 163 "parser.mly"
                                                         Op2 (Res,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 164 "parser.mly"
                                                         Op2 (Mul,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 165 "parser.mly"
                                                         Op2 (Div,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 166 "parser.mly"
                                                         Op2 (Igual,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 167 "parser.mly"
                                                         Op2 (Mayor,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 168 "parser.mly"
                                                         Op2 (Menor,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 169 "parser.mly"
                                                         Op2 (Conc,_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 170 "parser.mly"
                                                         ConsInt(_1,_3) ) : 'pterm))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 171 "parser.mly"
                                                         Op1 (Menos,_2 )) : 'pterm))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 174 "parser.mly"
                                               _1 ) : 'aplicacion))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'aplicacion) in
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 175 "parser.mly"
                                               App (_1,_2) ) : 'aplicacion))
; (fun parser_env ->
	Obj.repr((
# 178 "parser.mly"
                                                         CBool V ) : 'simple))
; (fun parser_env ->
	Obj.repr((
# 179 "parser.mly"
                                                         CBool F ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : int) in
	Obj.repr((
# 180 "parser.mly"
                                                         CInt _1 ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 181 "parser.mly"
                                                         Cadena _1 ) : 'simple))
; (fun parser_env ->
	Obj.repr((
# 182 "parser.mly"
                                                         Nil ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 3 : 'pterm) in
	let _4 = (peek_val parser_env 1 : 'pterm) in
	Obj.repr((
# 183 "parser.mly"
                                                         Par (_2,_4) ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 184 "parser.mly"
                                                         Var _1 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 1 : 'pterm) in
	Obj.repr((
# 185 "parser.mly"
                                                         _2 ) : 'simple))
; (fun parser_env ->
	Obj.repr((
# 186 "parser.mly"
                                                         Nil ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 187 "parser.mly"
                                                         Hd _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 188 "parser.mly"
                                                         Tl _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 189 "parser.mly"
                                                         Null _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 190 "parser.mly"
                                                         Fst _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 191 "parser.mly"
                                                         Snd _2 ) : 'simple))
; (fun parser_env ->
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 192 "parser.mly"
                                                         Y _2 ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 1 : 'op1) in
	let _2 = (peek_val parser_env 0 : 'simple) in
	Obj.repr((
# 193 "parser.mly"
                                                         Op1 (_1,_2) ) : 'simple))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'lista) in
	Obj.repr((
# 194 "parser.mly"
                                                         _1 ) : 'simple))
; (fun parser_env ->
	Obj.repr((
# 197 "parser.mly"
                                    Nil ) : 'lista))
; (fun parser_env ->
	let _2 = (peek_val parser_env 1 : 'elementos) in
	Obj.repr((
# 198 "parser.mly"
                                    _2 ) : 'lista))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : 'pterm) in
	Obj.repr((
# 200 "parser.mly"
                                    ConsInt(_1,Nil) ) : 'elementos))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'pterm) in
	let _3 = (peek_val parser_env 0 : 'elementos) in
	Obj.repr((
# 201 "parser.mly"
                                    ConsInt(_1,_3) ) : 'elementos))
; (fun parser_env ->
	Obj.repr((
# 203 "parser.mly"
                                                         Bool ) : 'tipo))
; (fun parser_env ->
	Obj.repr((
# 204 "parser.mly"
                                                         Int ) : 'tipo))
; (fun parser_env ->
	Obj.repr((
# 205 "parser.mly"
                                                         IntList ) : 'tipo))
; (fun parser_env ->
	Obj.repr((
# 206 "parser.mly"
                                                         String ) : 'tipo))
; (fun parser_env ->
	let _2 = (peek_val parser_env 3 : 'tipo) in
	let _4 = (peek_val parser_env 1 : 'tipo) in
	Obj.repr((
# 207 "parser.mly"
                                                         Producto (_2,_4) ) : 'tipo))
; (fun parser_env ->
	let _2 = (peek_val parser_env 1 : 'tipo) in
	Obj.repr((
# 208 "parser.mly"
                                                         _2 ) : 'tipo))
; (fun parser_env ->
	let _1 = (peek_val parser_env 2 : 'tipo) in
	let _3 = (peek_val parser_env 0 : 'tipo) in
	Obj.repr((
# 209 "parser.mly"
                                                         Flecha (_1,_3) ) : 'tipo))
; (fun parser_env ->
	let _1 = (peek_val parser_env 0 : string) in
	Obj.repr((
# 210 "parser.mly"
                                                         Variable (_1) ) : 'tipo))
(* Entry programa *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
(* Entry term *)
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
let term (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 2 lexfun lexbuf : Tipos.term)
(* Line 214, file parser.mly *)







(* Line 604, file parser.ml *)
