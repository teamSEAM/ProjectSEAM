type operator = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater| Geq 

type expr =
  | Binop of expr * operator * expr
  | Lit of int
  | Asn of string * expr (* SEAN *)
  | Id of string (* SEAN *)
  | Noexpr (* SEAN *)

type stmt=
  |Block of stmt list (* { ... } SEAN *)
  |If of expr * stmt * stmt (* SEAN *)