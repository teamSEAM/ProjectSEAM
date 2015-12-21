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
let regex_type =
  (type_bool | type_int | type_string | type_float | type_instance)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '#'      { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
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
| "entity" { ENTITY }
| regex_type as t  { TYPE }
| regex_lit as lit { LITERAL(Ast.literal_of_string lit) }
| regex_id  as id  { ID(id) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '#' { token lexbuf }
| _    { comment lexbuf }
