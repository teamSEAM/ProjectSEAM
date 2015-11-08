{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }| '-' { MINUS }
| '*' { TIMES }| '/' { DIVIDE }
| '=' { ASSIGN }| ',' { SEQUENCE }
| '(' { LPAREN } | ')' { RPAREN } (* Punctuation *)
| ';'' { SEMI } | ','' { COMMA }
| "==" { EQ }| "!=" { NEQ } 
| '<' { LT }| "<=" { LEQ } 
| ''>'' { GT }| ">=" { GEQ }
| "else" { ELSE } | "if" { IF } (* Keywords *)
| "return" { RETURN }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z'] as name { VARIABLE(int_of_char name - int_of_char 'a') }
| eof { EOF }
