type operator = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater| Geq 

type expr =
  | Binop of expr * operator * expr
  | Lit of int
  | Asn of string * expr (* SEAN *)
  | Id of string (* SEAN *)
  | Call of string * expr list
  | Noexpr (* SEAN *)

type stmt=
  |Expr of expr (* EXPR ; *)
  |Block of stmt list (* { ... } SEAN *)
  |If of expr * stmt * stmt (* SEAN *)