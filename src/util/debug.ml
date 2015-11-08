open Ast

let rec string_of_expr = function
    Binop(e1, op, e2) -> string_of_expr e1 ^ " <Binop> " ^ string_of_expr e2
  | Lit(l) -> "<Lit:" ^ string_of_int l ^ ">"
  | Asn(i, e) ->  i ^ "> = " ^ string_of_expr e
  | Id(i) -> i
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
 