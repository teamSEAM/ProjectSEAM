{ open Parser }

(* Generally useful regexes *)
let digit  = ['0'-'9']
let lower  = ['a'-'z']
let upper  = ['A'-'Z']
let letter = (upper | lower)
let minus  = ['-']
let plus   = ['+']
let sign   = (plus | minus)
let exp    = ['e' 'E'] sign? (digit+)

(* Literals *)
let lit_bool   = "true" | "false"
let lit_int    = minus? (digit+)
let lit_string = '"' [^'"']* '"'
let lit_float  = minus? (digit*) ['.']? (digit+) (exp)?
let regex_lit = (lit_bool | lit_int | lit_string | lit_float)

(* Identifiers *)
let regex_id = (letter | '_') ((letter | digit | '_')*)

(* Primitives *)
let type_bool     = "bool"
let type_int      = "int"
let type_string   = "string"
let type_float    = "float"
let type_instance = "instance " regex_id
let type_texture  = "texture"
let regex_type =
  (type_bool | type_int | type_string | type_float |
      type_instance | type_texture)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '#'      { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }
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
| "return" { RETURN }
| "bool"   { BOOL }
| "int"    { INT }
| "float"  { FLOAT }
| "string" { STRING }
| "entity" { ENTITY }
| "func"   { FUNC }
| "texture"{ TEXTURE }
| "spawn"  { SPAWN }
| "kill"   { KILL }
| lit_bool as b   { LIT_BOOL(bool_of_string b) }
| lit_int as i    { LIT_INT(int_of_string i) }
| lit_float as f  { LIT_FLOAT(float_of_string f) }
| lit_string as s { LIT_STRING(s) }
| regex_id as id  { ID(id) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '#' { token lexbuf }
| _    { comment lexbuf }
