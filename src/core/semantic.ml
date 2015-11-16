include Ast (* oh my god include this when using types from Ast *)

(*
  * To analyze: 
  *     Return types
  *     Expression type checks 
  *     Undeclared identifiers being used
  *     Unset identifiers being used (to an extent)
*)

let check prog = 


    let c_equivalents obj = match obj with
        | Void -> "void"
        | Str -> "char **" 
        | Int -> "int" in
    let c_primitive obj = match obj with
        | Str -> "char **"
        | Int -> "int" in

    (* We raise exceptions if stuff goes bad *)
    let handle_fdecl list_so_far current_fdecl = 
        
        
        (* handle the type *)
        let ret_type = c_equivalents (current_fdecl.vtype) in     
        let formals = "()" in 
        let statements = 
                let handle_stmt list_so_far current_stmt =
                       " huh " :: list_so_far
                       in

                let tokens = List.fold_left handle_stmt [] current_fdecl.body in
                String.concat " " tokens in

        let function_production = ret_type :: current_fdecl.fname ::
                formals :: "{" :: statements :: ["}"] 
                in 
        function_production @ list_so_far

        in
    let string_tokens = List.fold_left handle_fdecl [] prog
        in
    String.concat " " string_tokens



