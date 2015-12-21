type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type primitive = Bool | Int | String | Float | Instance of string

type literal =
| LitBool of bool
| LitInt of int
| LitString of string
| LitFloat of float

type expr =
| Literal of literal
| Id of string
| Binop of expr * op * expr
| Assign of string * expr
(* | Field of string * string              (\* entity id, field id *\) *)
(* | Method of string * string * expr list (\* entity id, method id, args *\) *)
| Call of string * expr list
| Noexpr

type stmt =
| Block of stmt list
| Expr of expr
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt

type vdecl = primitive * string

type fdecl = {
  fname : string;
  formals : vdecl list;
  locals : vdecl list;
  body : stmt list;
}

type edecl = {
  ename : string;
  fields : vdecl list;
  methods : fdecl list;
}

type program = vdecl list * edecl list * fdecl list

let string_of_op = function
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Equal -> "==" | Neq -> "!="
  | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="

let string_of_literal = function
  | LitBool(b) -> string_of_bool b
  | LitInt(b) -> string_of_int b
  | LitString(s) -> s
  | LitFloat(f) -> string_of_float f

let rec string_of_expr = function
  | Literal(l) -> string_of_literal l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, args) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
  | Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) ->
    "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->
    "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^
      "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
    "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_primitive = function
  | Bool -> "bool"
  | Int -> "int"
  | String -> "string"
  | Float -> "float"
  | Instance(name) -> "instance " ^ name

let string_of_vdecl (t, id) = string_of_primitive t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^
    String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_edecl edecl =
  "entity " ^ edecl.ename ^ "\n{\n" ^
    String.concat "" (List.map string_of_vdecl edecl.fields) ^
    String.concat "" (List.map string_of_fdecl edecl.methods) ^
    "}\n"

let string_of_program (vars, ents, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_edecl ents) ^ "\n" ^
    String.concat "\n" (List.map string_of_fdecl funcs)
