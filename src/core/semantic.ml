include Ast (* oh my god include this when using types from Ast *)

(*
  * To analyze: 
  *     Return types
  *     Expression type checks 
  *     Undeclared identifiers being used
  *     Unset identifiers being used (to an extent)
*)


(* actually everything below isn't checking, it's the translation. *)
let c_equivalents obj = match obj with
    | Str -> "char **" 
    | Float -> "float"
    | Int -> "int" 

let c_ret_equivalents obj = match obj with
    | Void -> "void"
    | PrimitiveVariable(p) -> c_equivalents p

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
    | Geq -> ">="


(* learned this one-liner for int maps because ocaml doesn't support
int Maps natively?? *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(String)


type translation_env = {

    current_scope: int;

    (* a map from scopes to the map of things in each scope, 
    which maps the variable name to a vdecl *)
    variables: vdecl StringMap.t IntMap.t;

    entities: entity_decl StringMap.t; 
    functions: fdecl StringMap.t;

    (* errors *)
}

(* error types: 

        you repeated the function declaration
                on the toplevel
                inside an entity

        you repeated the entity declaration

        you repeated a variable in the same scope
                inside a function
                        on the toplevel?
                        inside an entity?

        you used an undeclared variable in a statement
                inside a function
                        on the toplevel?
                        inside an entity?

        you fucked up the function call somewhere
                where was this statement that went wrong?
                        inside a function
                                on the toplevel?
                                inside an entity?
                what was the function we were trying to call?


        you tried to call an entity, but you fucked up
                did you try to get its value?
                did you try to use its function?
                        did the function not exist?
                    

        common themes:
                where did the error occur?
                        what function name?
                        function either toplevel, or
                                inside which entity?
                more details on the error:
                        function calls - how did you fuck up?
                        did you mess up types?
                        did you use something not declared?


*)


(* aliasing the StringMap just for familiarity *)
let search_stringmap target map = 
    StringMap.mem target map

let add_stringmap key addition map = 
    StringMap.add key addition map


(* for entities *) let add_entity_decl env entity_decl =
    let name = entity_decl.name in
    let entities = env.entities in
    let found = search_stringmap name entities in
    if found then
        (* error message, because there shouldn't be another with same name *)
        env
    else
        let updated_entities = StringMap.add name entity_decl entities in
        { env with entities = updated_entities; } 

(* very much like add_entity_decl but for functions *)
let add_function_decl env function_decl =
    let name = function_decl.fname in
    let functions = env.functions in 
    let found = search_stringmap name functions in
    if found then
        (* error message, because there shouldn't be another with same name *)
        env 
    else
        let updated_functions = StringMap.add name function_decl functions in
        { env with functions = updated_functions; } 



(* returns an updated environment for variables *)
let add_var_decl env var_decl =

    (* first let's introduce this auxiliary function for the add_var_decl *)
    let find_variable_scope env var =
        let current_scope = env.current_scope in
        let rec search_scope scope_number =
            (* -1, didn't find *)
            if scope_number < 0 then scope_number
            else
                (* get the map corresponding to this scope *)
                let var_map = IntMap.find scope_number env.variables in
                (* see whether the variable is present *)
                let result = search_stringmap var var_map in
                if result then 
                    scope_number
                else
                    search_scope (scope_number - 1)
            in
        search_scope current_scope
        in

    (* use find_variable_scope *)
    let var_name = snd var_decl in
    let scope_number = find_variable_scope env var_name in

    (* react accordingly *)
    if scope_number == env.current_scope then
        (* error, we have a duplicate *)
        env
    else
        (* whether NOT FOUND or declared in an earlier scope
            it's okay, we're adding it to the current scope now *)
        let current_stringmap = IntMap.find env.current_scope env.variables in
        let updated_stringmap = StringMap.add var_name var_decl current_stringmap in
        let updated_mapping = IntMap.add env.current_scope updated_stringmap env.variables in 
        { env with variables = updated_mapping; }
    

(* uses functions above to update the verification
    environment *)
let main_checker top_level_program = 

    (* first declare our environment *)
    let environment = 
            let scope_0_variables =
                let empty_intmap = IntMap.empty in
                let empty_stringmap = StringMap.empty in
                IntMap.add 0 empty_stringmap empty_intmap
                in
            {
                current_scope = 0;
                variables = scope_0_variables;
                entities = StringMap.empty;
                functions = StringMap.empty;
            } in
     
    (* first go through toplevel things and register them*)
    let register_toplevels env prog = 
        (* register each one appropriately *)
        let handler env top_lvl_decl = match top_lvl_decl with
            | TopLevelFunction f -> add_function_decl env f 
            | TopLevelVar v -> add_var_decl env v 
            | TopLevelEntity e -> add_entity_decl env e 
            in
        List.fold_left handler env prog 
        in 
    
    register_toplevels environment top_level_program 

    (* next, for each function in the toplevel:
        1. add variables
        2. go through each statement, checking the types
        3. the whole time, don't forget to set scope to 1,
        and make sure to rebuild the scope 1 each time
        we do a different function 
       
       for each entity in the toplevel:
                1. add variables
                2. do exactly the same thing above, but
                for that scope and for those functions
        *) 
    

let check prog = 

    let checking_environment = main_checker prog in
    (* check if checking_environment says there are any errors *) 

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
    let string_tokens = List.fold_left handle_toplevel [] prog
        in
    String.concat " " string_tokens



