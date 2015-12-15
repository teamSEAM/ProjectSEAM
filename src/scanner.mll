{ open Parser }


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQUAREBRACE }
| ']'      { RSQUAREBRACE }
| ';'      { SEMI }
| ','      { COMMA }


(* a lot of operators *)
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

(* statements *)
| "if"     {IF}
| "else"   {ELSE}
| "return" { RETURN }
| "print" { PRINT }


(* here's our types *)
| "string" { STRING }
| "int"    { INT }
| "float" { FLOAT }
| "instance" { INSTANCE }


(* large level declarations *)
| "entity" { ENTITY }
| "function" { FUNCTION }

(* literals *)
| ['0'-'9']+ as lxm { INT_LITERAL(int_of_string lxm) }
| '"' [^'"']*'"'as lxm { STRING_LITERAL(lxm) }

(* IDs are all that should remain *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }


| eof { EOF }
| _ as char { raise (Failure("Illegal character: " ^ Char.escaped char)) }
