type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq
type dtype = Bool | Int | String | Float | Instance of string | Array of dtype * int
type ret_type = Void | ActingType of dtype

type literal =
| LitBool of bool
| LitInt of int
| LitFloat of float
| LitString of string
| LitArray of literal * int

type identifier =
| Name of string
| Member of identifier * string  (* entity id, member id *)

type expr =
| Literal of literal
| Id of identifier               (* variables and fields *)
| Call of identifier * expr list (* functions and methods *)
| Binop of expr * op * expr
| Assign of identifier * expr
| Access of identifier * expr    (* array access *)
| Noexpr

type stmt =
| Block of stmt list
| Expr of expr
| Return of expr
| If of expr * stmt * stmt
| For of expr * expr * expr * stmt
| While of expr * stmt

type vdecl = dtype * string

type fdecl = {
  rtype : ret_type;
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

type program = edecl list

let string_of_op = function
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
  | Equal -> "==" | Neq -> "!="
  | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="

let rec string_of_dtype = function
  | Bool -> "bool"
  | Int -> "int"
  | String -> "string"
  | Float -> "float"
  | Array(t, size) ->
    string_of_dtype t ^ "[" ^ string_of_int size ^ "]"
  | Instance(name) -> "instance(" ^ name ^ ")"

let string_of_ret_type = function
  | Void -> "function"
  | ActingType(at) -> string_of_dtype at

let rec string_of_literal = function
  | LitBool(b) -> string_of_bool b
  | LitInt(b) -> string_of_int b
  | LitString(s) -> s
  | LitFloat(f) -> string_of_float f
  | LitArray(l, size) ->
    string_of_literal l ^ "[" ^ string_of_int size ^ "]"

let rec string_of_identifier = function
  | Name(name) -> name
  | Member(parent, name) -> string_of_identifier parent ^ "." ^ name

let rec string_of_expr = function
  | Literal(lit) -> string_of_literal lit
  | Id(id) -> string_of_identifier id
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(id, e) -> string_of_identifier id ^ " = " ^ string_of_expr e
  | Access(id, e) -> string_of_identifier id ^ "[" ^ string_of_expr e ^ "]"
  | Call(id, args) ->
    string_of_identifier id ^
      "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
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

let string_of_vdecl (t, id) = string_of_dtype t ^ " " ^ id ^ ";\n"

let string_of_formal (t, id) = string_of_dtype t ^ " " ^ id

let string_of_fdecl fdecl =
  string_of_ret_type fdecl.rtype ^ " " ^ fdecl.fname ^ "(" ^
    String.concat ", " (List.map string_of_formal fdecl.formals) ^ ")\n{\n" ^
    String.concat "" (List.map string_of_vdecl fdecl.locals) ^
    String.concat "" (List.map string_of_stmt fdecl.body) ^
    "}\n"

let string_of_edecl edecl =
  "entity " ^ edecl.ename ^ "\n{\n" ^
    String.concat "" (List.map string_of_vdecl edecl.fields) ^ "\n" ^
    String.concat "" (List.map string_of_fdecl edecl.methods) ^
    "}\n"

let string_of_program entities =
  String.concat "\n" (List.map string_of_edecl entities)
