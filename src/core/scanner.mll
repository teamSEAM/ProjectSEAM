{ open Parser }


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }

| "if"     {IF}
| "else"   {ELSE}
| "return" { RETURN }
| "print" { PRINT }

| "string" { STRING }
| "int"    { INT }
| "float" { FLOAT }

| "entity" { ENTITY }
| "function" { FUNCTION }

| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| '"' [^'"']*'"'as lxm { STRING_LITERAL(lxm) }

| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("Illegal character: " ^ Char.escaped char)) }
