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
        | Geq -> ">=" in

    (* returns the expanded c expression for *)
    let expand_expr expr_obj = 
        let rec generate_expr expr_obj = match expr_obj with
                | IntLit(i) -> [string_of_int i;]
                | StrLit(str) -> [str;]
                | Id(str) -> 
                        let mystr = String.concat "" ["__"; str;] in
                        [mystr;]
                | Binop(left, op, right) -> 
                        ["(";] 
                        @ (generate_expr left)
                        @ [")"; c_op op; "(";] 
                        @ (generate_expr right) @ [ ")";]
                | Assign(str, expr) -> [ (String.concat "" ["__"; str;]); "=";]
                        @ ( generate_expr expr)
                | _ -> [] (* TODO - WILL THROW AN ERROR. *)
         in generate_expr expr_obj in

    (* We raise exceptions if stuff goes bad *)
    let handle_fdecl list_so_far current_fdecl = 
        
        
        (* handle the type *)
        let ret_type = c_equivalents (current_fdecl.vtype) in     
        let formals = "()" in 
        let statements = 

                (* too many layers of "let", this will be refactored later *)
                let handle_stmt current_stmt list_so_far =
                        match current_stmt with

                                | Print(expr) ->
                                   (match expr with 
                                        | StrLit(str) | Id(str) -> "printf(\"%s\"," ::
                                                ((expand_expr expr) @ (");" :: list_so_far ) )
                                        | _ -> [] (* TODO throw error *))
                                | Return(expr) ->
                                   "return" :: (expand_expr expr @ (";" :: list_so_far ))
                                | _ -> "" :: list_so_far
                       in

                (* use fold_right to generate the statements *)
                let tokens = List.fold_right handle_stmt current_fdecl.body [] in
                String.concat " " tokens in

        let function_production = ret_type :: current_fdecl.fname ::
                formals :: "{" :: statements :: ["}"] 
                in 
        function_production @ list_so_far

        in
    let string_tokens = List.fold_left handle_fdecl [] prog
        in
    String.concat " " string_tokens



