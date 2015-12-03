(* Defines the operators allowed in a binary operation *)
type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

(* Defines the different types of allowed expressions *)
type expr =
    IntLit of int (* An integer literal *)
  | StrLit of string (* A string literal *)
  | Id of string (* IDs are always strings *)
  | Binop of expr * op * expr (* Binary expressions *)
  | Assign of string * expr (* Assignment is ID + an expression *)
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

type primitive = Str | Float | Int (* To add: floats, instances, boolean *)
type ret_type = Void | PrimitiveVariable of primitive
type vdecl = primitive * string

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

