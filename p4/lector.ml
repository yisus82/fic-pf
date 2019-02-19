
let term cadena =  
  Parser.term Lexer.token (Lexing.from_string cadena)

let string cadena =  
  Parser.programa Lexer.token (Lexing.from_string cadena)

let fichero nombre =
  Parser.programa Lexer.token (Lexing.from_channel (open_in nombre))
