
let term cadena =  
  Parser.termino Lexer.token (Lexing.from_string cadena)
