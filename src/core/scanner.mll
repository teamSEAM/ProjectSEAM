(*{ open Parser }

rule token = parse
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
| "return" { RETURN }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z'] as name { VARIABLE(int_of_char name - int_of_char 'a') }
| eof { EOF }
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
