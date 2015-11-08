(*{ open Parser }

rule token = parse
<<<<<<< HEAD
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }| '-' { MINUS }
| '*' { TIMES }| '/' { DIVIDE }
| '=' { ASSIGN }| ',' { SEQUENCE }
| '(' { LPAREN } | ')' { RPAREN } (* Punctuation *)
| ';' { SEMI } | ',' { COMMA }
| "==" { EQ }| "!=" { NEQ } 
| '<' { LT }| "<=" { LEQ } 
| '>' { GT }| ">=" { GEQ }
| "else" { ELSE } | "if" { IF } (* Keywords *)
=======
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
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
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
>>>>>>> f2d8a2cc8a7eb699e5c75cd9f69e0abd74fbfa54
| "return" { RETURN }
| "int"    { INT }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
<<<<<<< HEAD
*)
{ open Parser }

let letters=['a'-'z' 'A'-'Z']
let digits=  ['0'-'9']

rule token = 
parse [' ' '\t' '\r' '\n'] { token lexbuf }
|'+' {PLUS}				|'-' {MINUS}
|'/' {DIVIDE}			|'*' {TIMES}
|'(' {LPAREN}			|')' {RPAREN}
|'[' {SQLB}       		|']' {SQRB}
|',' {COMMA}			|'.' {DOT}
|':' {COLON}
| ';' { SEMI } 

|'=' {ASSIGN}			|"!=" {NEQ}
|"==" {EQ}			
|'>' {GT}				|"<" {LT}
|">=" {GEQ}				|"<=" {LEQ}
| "else" { ELSE } 		| "if" { IF } 
| "while" { WHILE } 	|"for" { FOR }
| "int" { INT } 		| "float" {FLOAT}

|"entity" {ENTITY}
|"texture" {TEXTURE}
|"function" {FUNCTION}
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }

| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
| eof { EOF } 
