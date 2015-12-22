open Ast
open Boilerplate

exception UndeclaredEntity of string
exception UndeclaredIdentifier of string

type symbol_table = {
  parent : symbol_table option;
  current_entity : edecl;
  variables : vdecl list;
}

type environment = {
  entities : edecl list;
  scope : symbol_table;
}

let rec string_of_scope s =
  "parent: " ^ (match s.parent with
  | None -> ""
  | Some(p) -> string_of_scope p) ^ ")\ncurrent_entity: " ^
    string_of_edecl s.current_entity ^ "\nvariables: " ^
    String.concat "; " (List.map string_of_vdecl s.variables)

let string_of_env env =
  "entities: " ^ String.concat ", " (List.map string_of_edecl env.entities) ^
    "\nscope: " ^ string_of_scope env.scope ^ "\n"

let find_entity (env : environment) name =
  try List.find (fun e -> e.ename = name) env.entities
  with Not_found -> raise (UndeclaredEntity name)

let rec find_variable (scope : symbol_table) name =
  try List.find (fun (_, n) -> n = name) scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _ -> raise (UndeclaredIdentifier name)

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

let in_scope scope name =
  try
    let _ = (List.find (fun (_, n) -> n = name) scope.variables) in
    true
  with Not_found -> false

let rec is_field scope name =
  match scope.parent with
  | None ->
    if (in_scope scope name) then true
    else raise (UndeclaredIdentifier name)
  | Some(parent) ->
    if (in_scope scope name) then false
    else is_field parent name

let pop_scope env =
  match env.scope.parent with
  | Some(new_scope) ->
    {
      entities = env.entities;
      scope = new_scope;
    }
  | None -> raise (Failure "Attempting to pop from empty environment")

let tr_identifier env id =
  (if (is_field env.scope (name_of_identifier id)) then
      "this->" else "") ^ string_of_identifier id

let is_builtin name =
  try let _ = List.find (fun s -> s = name) Lib.modules in true
  with Not_found -> false

let rec tr_expr env = function
  | Literal(lit) -> string_of_literal lit
  | Id(id) -> tr_identifier env id
  | Binop(e1, o, e2) ->
    (tr_expr env) e1 ^ " " ^ string_of_op o ^ " " ^ (tr_expr env) e2
  | Assign(id, e) -> tr_identifier env id ^ " = " ^ (tr_expr env) e
  | Access(id, e) -> tr_identifier env id ^ "[" ^ (tr_expr env) e ^ "]"
  | Spawn(ent) -> ent ^ "_spawn()"
  | Call(id, args) ->
    (match id with
    | Name(n) -> if (n = "load") || (n = "unload")
      then n ^ "(" ^ String.concat ", " (List.map (tr_expr env) args) ^ ")"
      else tr_identifier env id ^ "(" ^
	String.concat ", " (List.map (tr_expr env) args) ^ ")"
    | Member(p, n) ->
      if is_builtin p then "_" ^ p ^ "_" ^ n ^
	"(" ^ String.concat ", " (List.map (tr_expr env) args) ^ ")"
      else tr_identifier env id ^
	"(" ^ String.concat ", " (List.map (tr_expr env) args) ^ ")")
  | Noexpr -> ""

let rec tr_stmt env = function
  | Block(stmts) ->
    "{\n" ^ String.concat "\n" (List.map (tr_stmt env) stmts) ^ "\n}"
  | Expr(expr) -> (tr_expr env) expr ^ ";";
  | Return(expr) -> "return " ^ (tr_expr env) expr ^ ";";
  | If(e, s, Block([])) ->
    "if (" ^ (tr_expr env) e ^ ") " ^ (tr_stmt env) s
  | If(e, s1, s2) ->
    "if (" ^ (tr_expr env) e ^ ") " ^ (tr_stmt env) s1 ^
      " else " ^ (tr_stmt env) s2
  | For(e1, e2, e3, s) ->
    "for (" ^ (tr_expr env) e1  ^ " ; " ^ (tr_expr env) e2 ^ " ; " ^
      (tr_expr env) e3  ^ ") " ^ (tr_stmt env) s
  | While(e, s) -> "while (" ^ (tr_expr env) e ^ ") " ^ (tr_stmt env) s
  | Kill(id) ->
    let iname = name_of_identifier id in
    let (dtype, _) = find_variable env.scope iname in
    let ename = string_of_dtype dtype in
    ename ^ "_kill(" ^ (tr_identifier env id) ^ ")"

let rec tr_formal (typ, name) =
  match typ with
  | Bool -> "int " ^ name
  | Int -> "int " ^ name
  | String -> "char *" ^ name
  | Float -> "float " ^ name
  | Instance(s) -> s ^ " *" ^ name
  | Array(t, size) -> tr_formal(t, name) ^ "[" ^ string_of_int size ^ "]"
  | Texture -> "texture *" ^ name

let tr_vdecl vdecl = (tr_formal vdecl) ^ ";"

let is_stub fname =
  try let _ = List.find (fun stub -> fname = stub)
	Boilerplate.stubs_action in true
  with Not_found -> false

let tr_fdecl env fdecl =
  let env = add_scope env (fdecl.formals @ fdecl.locals) in
  let ename = env.scope.current_entity.ename in
  let mangled_fname = ename ^ "_" ^ fdecl.fname in
  let first_arg = if (is_stub fdecl.fname) then "void *in" else ename ^ " *this" in
  let rtype = fdecl.rtype in
  string_of_rtype rtype ^ " " ^ mangled_fname ^
    "(" ^ String.concat ", " (first_arg :: List.map string_of_formal fdecl.formals) ^
    ") {\n" ^
    (if (is_stub fdecl.fname)
     then ename ^ " *this = (" ^ ename ^ " *)in;\n" else "") ^
    String.concat "\n" (List.map tr_vdecl fdecl.locals) ^ "\n" ^
    String.concat "\n" (List.map (tr_stmt env) fdecl.body) ^ "\n}\n"

let update_stub edecl fdecl =
  try let _ = List.find (fun f -> f.fname = fdecl.fname)
	edecl.methods
      in edecl
  with Not_found -> {
    ename = edecl.ename;
    fields = edecl.fields;
    methods = List.rev (fdecl :: (List.rev edecl.methods));
  }

let tr_edecl (env, output) edecl =
  let stubs = [ {rtype = Void;
		 fname = "step";
		 formals = [];
		 locals = [];
		 body = [];
		};
		{rtype = Void;
		 fname = "start";
		 formals = [];
		 locals = [];
		 body = [];
		};
		{rtype = Void;
		 fname = "stop";
		 formals = [];
		 locals = [];
		 body = [];
		};
		{rtype = Void;
		 fname = "render";
		 formals = [];
		 locals = [];
		 body = [];
		}
	      ]
  in
  let edecl = List.fold_left update_stub edecl stubs in
  let env = add_edecl env edecl in
  let ename = edecl.ename in
  let fields = List.map tr_vdecl edecl.fields in
  let methods = List.map (tr_fdecl env) edecl.methods in
  let translated = "typedef struct " ^ ename ^ " {\n" ^
    String.concat "\n" fields ^ "\n} " ^ ename ^";\n" ^
    String.concat "\n" methods ^ "\n" ^
    (gen_spawn ename) ^ "\n" ^
    (gen_destroy ename) in
  (env, translated :: output)

let translate entities =
  let empty_edecl = { ename = ""; fields = []; methods = [] } in
  let empty_env = {
    entities = [];
    scope = { parent = None; current_entity = empty_edecl; variables = [] };
  } in
  let (env, translated) = (List.fold_left tr_edecl (empty_env, []) entities) in
  String.concat "\n" translated
