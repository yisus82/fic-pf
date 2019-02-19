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

val termino :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tipos.term
