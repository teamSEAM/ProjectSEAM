open Ast
(*
let var_array = Array.make 26 0

let rec eval = function
    Lit(x) -> x
  | Id(v) -> var_array.(v)
  | Asn(var, expr) ->
    var_array.(var) <- (eval expr);
    var_array.(var)
  | Binop(e1, op, e2) ->
    let v1 = eval e1 and v2 = eval e2 in
    match op with
	  Add -> v1 + v2
    | Sub -> v1 - v2
    | Mul -> v1 * v2
    | Div -> v1 / v2
*)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.token lexbuf in
  print_endline (Debug.string_of_expr expr)
