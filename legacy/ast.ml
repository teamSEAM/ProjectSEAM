(* Defines the operators allowed in a binary operation *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

(* Defines the different types of allowed expressions *)
type expr =
    IntLit of int (* An integer literal *)
  | StrLit of string (* A string literal *)
  | Id of string (* IDs are always strings *)
  | Binop of expr * op * expr (* Binary expressions *)
  | Assign of string * expr (* Assignment is ID + an expression *)
  | Field of string * string (* Entity id and field id *)
  | Method of string * string * expr list (* Entity method call *)
  | Call of string * expr list (* A call has a function name and list of
                                 arguments, which are all expressions *)
  | Noexpr (* A "no expression" is an empty expression -- i.e. 'e' used
              in call_function( the_empty_string ) *)

(* Defines the allowable statements *)
type stmt =
    Block of stmt list (* Statement can be a list of statements *)
  | Expr of expr (* A single expression is a valid statement
                    TODO: Check if this will break in C -- looks valid *)
  | Return of expr (* Return a value from a function *)
  | Print of expr (* prints value of the expression *)
  | If of expr * stmt * stmt

(* Acting type = type including array declarations *)
(* To add: floats, instances, boolean *)
type primitive = Str | Float | Int | Instance
type array_size = NotAnArray | Dynamic | ArraySize of int
type acting_type = primitive * array_size

type ret_type = Void | ActingType of acting_type
type vdecl = acting_type * string

(*
type ret_type = Void | PrimitiveVariable of primitive
type vdecl = primitive * string
*)

type fdecl = {
    vtype: ret_type; (* type of function *)
    fname : string;
    formals : vdecl list;
    locals : vdecl list;
    body : stmt list;
}

type entity_decl = {
        name : string;
        members : vdecl list;
        functions : fdecl list;
}

type toplevel_element =
        | TopLevelFunction of fdecl
        | TopLevelVar of vdecl
        | TopLevelEntity of entity_decl

type program = toplevel_element list

(* Nice. *)
let c_equivalents obj = match obj with
    | Str -> "char **"
    | Float -> "float"
    | Int -> "int"

(*type array_size = NotAnArray | Dynamic | ArraySize of int
type acting_type = primitive * array_size  *)

(* generating the C code for anything involving arrays is so much
more complicated than what I had before for non-arrays, so that will be moved
out of AST. The functions in AST are just for generating convenient strings like
"float" or "int" *)
let c_op obj = match obj with
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Equal -> "=="
    | Neq -> "!="
    | Less -> "<"
    | Leq -> "<="
    | Greater -> ">"
    | Geq -> ">="
