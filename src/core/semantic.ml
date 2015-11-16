include Ast (* oh my god include this when using types from Ast *)

(*
  * To analyze: 
  *     Return types
  *     Expression type checks 
  *     Undeclared identifiers being used
  *     Unset identifiers being used (to an extent)
*)

let check prog = 
    
    (* We raise exceptions if stuff goes bad *)
    let handle_fdecl list_so_far current_fdecl = 
        
        
        
         
        
        
        
        
        current_fdecl.fname :: list_so_far
        in
    let string_tokens = List.fold_left handle_fdecl [] prog
        in
    String.concat "" string_tokens

