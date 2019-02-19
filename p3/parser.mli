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

val programa :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Tipos.variable * Tipos.term) list
val term :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tipos.term
