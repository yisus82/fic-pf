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

val programa :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Tipos.variable * Tipos.term) list
