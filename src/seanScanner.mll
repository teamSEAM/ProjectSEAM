{ open Parser }

let letters=['a'-'z']
let digits=  ['0'-'9']
let floating_point=(left+eside)|nodecimal

rule token = 
parse [' '\r' '\n'] { token lexbuf }
|'\t' {TAB}

|'#' {COMMENT}
|'+' {PLUS}				|'-' {MINUS}
|'/' {DIVIDE}			|'*' {MULTIPLY}
|'(' {LBRACKET}			|')' {RBRACKET}
|'[' {SQLB}       		|']' {SQRB}
|',' {COMMA}			|'.' {DOT}
|':' {COLON}


|'=' {ASSIGN}			|'!=' {NOTEQUAL}
|'==' {EQUAL}			
|'>' {GT}				|'<' {LT}
|'>=' {GTE}				|'<=' {LTE}
| "else" { ELSE } 		| "if" { IF } 
| "while" { WHILE } 	|"for" { FOR }
| "int" { INT } 		| "float" {FLOAT}

|"entity" {ENTITY}
|"main" {MAIN}
|"texture" {TEXTURE}
|"function" {FUNCTION}
|
| eof { EOF } 
