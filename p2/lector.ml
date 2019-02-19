
let term cadena =  
  Parser.programa Lexer.token (Lexing.from_string cadena)


let fichero nombre =
  Parser.programa Lexer.token (Lexing.from_channel (open_in nombre))
