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

type ret_type = Void | Str | Int
type primitive = Str | Int (* To add: floats, instances, boolean *)
type vdecl = primitive * string

type fdecl = {
    vtype: ret_type; (* type of function *)
    fname : string;
    formals : vdecl list;
    locals : vdecl list;
    body : stmt list;
}

type program = fdecl list (* Only function decls for now; add globals/
                                 entities later *)

(*type entity_decl = {
        ename : string;
        members : v_decl list;
        functions : func_decl list;
}

type toplevel_element =
        | Function of func_decl
        | VarDecl of v_decl
        | EntityDecl of entity_decl
*)

(* type program = toplevel_element list *)
