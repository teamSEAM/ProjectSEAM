(* Errors will include Ast too *)
include Errors


(*
  * To analyze: 
  *     Return types
  *     Expression type checks 
  *     Undeclared identifiers being used
  *     Unset identifiers being used (to an extent)
*)
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
    errors: error list;
}

(* aliasing the StringMap just for familiarity *)
let search_stringmap target map = 
    StringMap.mem target map

let add_stringmap key addition map = 
    StringMap.add key addition map


(* for entities *)
let add_entity_decl env possible_error_locus entity_decl =
    let name = entity_decl.name in
    let entities = env.entities in
    let found = search_stringmap name entities in
    if found then
        (* error message, because there shouldn't be another with same name *)
        let new_error = (
                possible_error_locus,
                Scope(env.current_scope),
                EntityRepeatDecl(entity_decl))
            in
        { env with errors = new_error :: env.errors }
    else
        let updated_entities = StringMap.add name entity_decl entities in
        { env with entities = updated_entities; } 

(* very much like add_entity_decl but for functions *)
let add_function_decl env possible_error_locus function_decl =
    let name = function_decl.fname in
    let functions = env.functions in 
    let found = search_stringmap name functions in
    if found then
        (* error message, because there shouldn't be another with same name *)
        let new_error = (
                (* This is why we force user of add_function_decl to include its 
                own name. Here, if it is somewhere not in the toplevel, then 
                it must be within an entity that we declared repeatedly *)
                possible_error_locus,
                Scope(env.current_scope),
                FunctionRepeatDecl(function_decl))
            in
        { env with errors = new_error :: env.errors }
    else
        let updated_functions = StringMap.add name function_decl functions in
        { env with functions = updated_functions; } 



(* returns an updated environment for variables *)
let add_var_decl env possible_error_locus var_decl =

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
        (* error, we have a duplicate variable declaration
                inside the same scope... *)
        let new_error = (
                possible_error_locus,
                Scope(env.current_scope),
                VariableRepeatDecl(var_decl))
            in
        { env with errors = new_error :: env.errors }

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
                errors = [];
            } in
     
    (* first go through toplevel things and register them*)
    let register_toplevels env prog = 
        (* register each one appropriately, 
           TopLevel is there for possible error locus *)
        let handler env top_lvl_decl = match top_lvl_decl with
            | TopLevelFunction f -> add_function_decl env TopLevel f 
            | TopLevelVar v -> add_var_decl env TopLevel v 
            | TopLevelEntity e -> add_entity_decl env TopLevel e 
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







let semantic_check unchecked_program =
 
    (* check if checking_environment says there are any errors *) 
    let checked_environment = main_checker unchecked_program in
    ""
       
