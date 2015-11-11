type operator = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater| Geq 


type primitive_type = Bool | Str | Instance | Int

(* bool is whether this is an array *)
type actual_type = primitive_type * bool

type expr =
  | Binop of expr * operator * expr
  | Lit of int
  | Asn of string * expr (* SEAN *)
  | Id of string (* SEAN *)
  | Call of string * expr list
  | Noexpr (* SEAN *)

type stmt =
  |Expr of expr (* EXPR ; *)
  |Block of stmt list (* { ... } SEAN *)
  |If of expr * stmt * stmt (* SEAN *)


type v_decl = actual_type * string
type func_decl = {
        fname: string;
        return_type: actual_type;
        formals: v_decl list;
        locals: v_decl list;
        body: stmt list;
}

type entity_decl = {
        ename: string;
        members: v_decl list; 
        functions: func_decl list;
}

type toplevel_element = func_decl | v_decl | entity_decl
type program = toplevel_element list





