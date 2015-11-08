open Ast

let rec string_of_expr = function
    Binop(e1, op, e2) -> string_of_expr e1 ^ " <Binop> " ^ string_of_expr e2
  | Lit(l) -> "<Lit:" ^ string_of_int l ^ ">"
  | Asn(i, e) ->  i ^ "> = " ^ string_of_expr e
  | Id(i) -> i
