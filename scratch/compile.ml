open Ast

exception UndeclaredEntity of string
exception UndeclaredVariable of string

type symbol_table = {
  parent : symbol_table option;
  current_entity : edecl;
  variables : vdecl list;
}

type environment = {
  entities : edecl list;
  scope : symbol_table;
}

let find_entity (env : environment) name =
  try List.find (fun e -> e.ename = name) env.entities
  with Not_found -> raise (UndeclaredEntity name)

let rec find_variable (scope : symbol_table) name =
  try List.find (fun (_, n) -> n = name) scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _ -> raise (UndeclaredVariable name)

let add_edecl env edecl = {
  entities = edecl :: env.entities;
  scope = {
    parent = None;
    current_entity = edecl;
    variables = edecl.fields;
  };
}

let add_scope env vdecls = {
  entities = env.entities;
  scope = {
    parent = Some(env.scope);
    current_entity = env.scope.current_entity;
    variables = vdecls;
  };
}

let rec tr_expr env = function
  | Literal(lit) -> string_of_literal lit
  | Id(id) -> string_of_identifier id
  | Binop(e1, o, e2) ->
    (tr_expr env) e1 ^ " " ^ string_of_op o ^ " " ^ (tr_expr env) e2
  | Assign(id, e) -> string_of_identifier id ^ " = " ^ (tr_expr env) e
  | Access(id, e) -> string_of_identifier id ^ "[" ^ (tr_expr env) e ^ "]"
  | Call(id, args) ->
    string_of_identifier id ^
      "(" ^ String.concat ", " (List.map (tr_expr env) args) ^ ")"
  | Noexpr -> ""

let rec tr_stmt env = function
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map (tr_stmt env) stmts) ^ "}\n"
  | Expr(expr) -> (tr_expr env) expr ^ ";\n";
  | Return(expr) -> "return " ^ (tr_expr env) expr ^ ";\n";
  | If(e, s, Block([])) ->
    "if (" ^ (tr_expr env) e ^ ")\n" ^ (tr_stmt env) s
  | If(e, s1, s2) ->
    "if (" ^ (tr_expr env) e ^ ")\n" ^ (tr_stmt env) s1 ^
      "else\n" ^ (tr_stmt env) s2
  | For(e1, e2, e3, s) ->
    "for (" ^ (tr_expr env) e1  ^ " ; " ^ (tr_expr env) e2 ^ " ; " ^
      (tr_expr env) e3  ^ ") " ^ (tr_stmt env) s
  | While(e, s) -> "while (" ^ (tr_expr env) e ^ ") " ^ (tr_stmt env) s

let tr_vdecl (typ, name) =
  string_of_dtype typ ^ " " ^ name ^ ";"

let tr_fdecl env fdecl =
  let env = add_scope env (fdecl.formals @ fdecl.locals) in
  let ename = env.scope.current_entity.ename in
  let mangled_fname = "__" ^ ename ^ "_" ^ fdecl.fname in
  let first_arg = "struct " ^ ename ^ " *this" in
  let rtype = fdecl.rtype in
  string_of_rtype rtype ^ mangled_fname ^
    "(" ^ String.concat ", " (first_arg :: List.map string_of_formal fdecl.formals) ^
    ") {\n" ^ String.concat "\n" (List.map tr_vdecl fdecl.locals) ^ "\n" ^
    String.concat "\n" (List.map (tr_stmt env) fdecl.body)

let tr_edecl (env, output) edecl =
  let env = add_edecl env edecl in
  let ename = edecl.ename in
  let fields = List.map tr_vdecl edecl.fields in
  let methods = List.map (tr_fdecl env) edecl.methods in
  let translated = "struct " ^ ename ^ " {\n" ^
    String.concat "\n" fields ^ "\n}\n\n" ^
    String.concat "\n" methods ^ "\n" in
  (env, translated :: output)

let translate entities =
  let empty_edecl = { ename = ""; fields = []; methods = [] } in
  let empty_env = {
    entities = [];
    scope = { parent = None; current_entity = empty_edecl; variables = [] };
  } in
  let (env, translated) = (List.fold_left tr_edecl (empty_env, []) entities) in
  String.concat "\n" translated

(* let tr_stmt stmt = string_of_stmt stmt *)

(* let tr_vdecl (dtype, name) = *)


(* let tr_fdecl ename fdecl = *)
(*   let first_arg = "struct " ^ ename ^ "* self" *)
(*   and mangled_fname = "__" ^ ename ^ " " ^ fdecl.fname *)
(*   and formals = List.map string_of_formal fdecl.formals *)
(*   and locals = List.map tr_vdecl fdecl.locals *)
(*   in *)
(*   string_of_rtype fdecl.rtype ^ *)
(*     mangled_fname ^ *)
(*     "(" ^ String.concat ", " (first_arg :: formals) ^ ") {\n" ^ *)

(*     "}\n" *)

(* let tr_edecl edecl = *)
(*   "struct " ^ edecl.ename ^ " {\n" ^ *)
(*     String.concat "\n" (List.map string_of_vdecl edecl.fields) ^ "\n}\n\n" ^ *)
(*     String.concat "\n" (List.map (tr_fdecl edecl.name) edecl.methods) ^ "\n" *)

(* let translate entities = *)
(*   String.concat "\n" (List.map tr_edecl entities) *)
