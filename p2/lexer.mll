
{
     open Parser
}

rule token = parse
     [' ' '\t' '\n']    { token lexbuf }
   | '.'                { PUNTO }
   | '/'                { LAMBDA }
   | '('                { PARIZQ }
   | ')'                { PARDER }
   | "let"              { LET }
   | ":="               { DOSIGUAL }
   | ";;"      		{ DOSPUNTOCOMA }
   | ['a'-'z']['a'-'z''0'-'9''_']*
                        { MINUSCULAS (Lexing.lexeme lexbuf)}
   | ['A'-'Z']['A'-'Z''0'-'9''_']*
                        { MAYUSCULAS (Lexing.lexeme lexbuf)}
   | eof                { EOF }
   | _                  { raise Tipos.ERROR_LEXICO }

			



