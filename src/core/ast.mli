type operator = Add | Sub | Mul | Div | Equal | Neq | Less | Leq | Greater |

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Seq of expr * expr
  | Asn of int * expr

  | Id of string (* SEAN *)

type stmt=
  |Block of stmt list (* { ... } SEAN *)
  |If of expr * stmt * stmt (* SEAN *)