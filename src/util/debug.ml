open Ast

let rec string_of_expr = function
    Binop(e1, op, e2) ->
      string_of_expr e1 ^ " " ^
	(match op with
	  Add -> "+"
	| Sub -> "-"
	| Mul -> "*"
	| Div -> "/") ^ " " ^
	string_of_expr e2
  | Seq(l, r) -> (string_of_expr l) ^ "\n" ^ (string_of_expr r)
  | Lit(l) -> string_of_int l
  | Asn(i, e) -> "<var:" ^ string_of_int i ^ "> = " ^ string_of_expr e
  | Var(i) -> "<var:" ^ string_of_int i ^ ">"
