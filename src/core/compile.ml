include Ast

let c_ret_equivalents obj = match obj with
    | Void -> "void"
    | ActingType(t) -> match (snd t) with
        | Dynamic -> ""
        | ArraySize(i) -> ""
        | NotAnArray -> c_equivalents (fst t)



(* Takes as input the checked AST-toplevel, and
generates the C output *) 
let translate checked_program = 

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
    let handle_fdecl current_fdecl = 
        
        
        (* handle the type *)
        let ret_type = c_ret_equivalents (current_fdecl.vtype) in     
        let formals = "()" in 
        let function_name = 
                if (String.compare current_fdecl.fname "main") == 0 then "program_ep"
                else current_fdecl.fname in
        let statements = 

                (* too many layers of "let", this will be refactored later *)
                let handle_stmt current_stmt list_so_far =
                        match current_stmt with

                                | Print(expr) ->
                                   (match expr with 
                                        | StrLit(str) | Id(str) -> "_seam_print(" ::
                                                ((expand_expr expr) @ (");" :: list_so_far ) )
                                        | _ -> [] (* TODO throw error *))
                                | Return(expr) ->
                                   "return" :: (expand_expr expr @ (";" :: list_so_far ))
                                | _ -> "" :: list_so_far
                       in

                (* use fold_right to generate the statements *)
                let tokens = List.fold_right handle_stmt current_fdecl.body [] in
                String.concat " " tokens in

        let function_production = ret_type :: function_name ::
                formals :: "{" :: statements :: ["}"] 
                in 
        function_production

        in

    let handle_toplevel list_so_far current_toplevel =
        let current =
                match current_toplevel with 
                | TopLevelFunction(fdecl) -> handle_fdecl fdecl
                | TopLevelVar(v) -> []
                | TopLevelEntity(e) -> []
        in current @ list_so_far in
    let string_tokens = List.fold_left handle_toplevel [] checked_program
        in
    String.concat " " string_tokens



