(* Compile to C in here *)

let da_func "" element =
    (* How we deal with fdecls *)

let rec translate functions =
    List.fold_left da_func "" functions
