
{
     open Parser

     let cadena c = String.sub c 1 (String.length c -2)
}

rule token = parse
      [' ' '\t' '\n']    { token lexbuf }
  			(* Comment *)
   | '#'[^'\n']*	{ token lexbuf }
   | '.'                { PUNTO }
   | '('                { PARIZQ }
   | ')'                { PARDER }
   | '['                { CORIZQ }
   | ']'                { CORDER }
   | '='                { IGUAL }
   | "->"               { FLECHA }
   | '>'                { MAYOR }
   | '<'                { MENOR }
   | '+'                { MAS }
   | '-'                { MENOS }
   | '*'                { POR }
   | '/'                { BARRA }
   | '^'                { TILDE }
   | ','                { COMA }
   | ';'                { PUNTOCOMA }
   | ":="               { DOSIGUAL }
   | "::"               { CUATROPUNTOS }
   | ";;"               { DOSPUNTOCOMA }
   | ':'                { DOSPUNTOS }
   | "[]"               { CORCHETES }
   | "and"              { AND }
   | "Bool"             { BOOL }
   | "IntList"          { INTLIST }
   | "Int"              { INT }
   | "Y"                { Y }     
   | "String"           { STRING }
   | "else"             { ELSE }
   | "false"            { FALSE }
   | "if"               { IF }
   | "in"               { IN }
   | "let"              { LET }
   | "null"             { NULL }
   | "then"             { THEN }
   | "tl"               { TL }
   | "hd"               { HD }
   | "true"             { TRUE }
   | "fst"              { FST }
   | "snd"              { SND }
   | "lengthc"          { LENGTHC }
   | "\""['A'-'Z''a'-'z''0'-'9''_'' ']* "\""
                        { CADENA (cadena(Lexing.lexeme lexbuf)) }
   | ['0'-'9']+         { ENTERO (int_of_string (Lexing.lexeme lexbuf)) }
   | ['a'-'z']['a'-'z''0'-'9''_']*
                        { MINUSCULAS (Lexing.lexeme lexbuf)}
   | ['A'-'Z']['A'-'Z''0'-'9''_']*
                        { MAYUSCULAS (Lexing.lexeme lexbuf)}
   | eof                { EOF }
   | _                  { raise Tipos.ERROR_LEXICO }

			



