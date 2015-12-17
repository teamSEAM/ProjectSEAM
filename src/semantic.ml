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

    (* ONLY THE TOPLEVEL FUNCTIONS, for functions
    within entities, we'll just peak inside the entity *)
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
(* issue: entity functions. those can have the same name, as
long as within an entity there are no multiple declarations *)
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


(* ALTERNATE VERSION OF THE ABOVE for within entities, it's just that this one
will only search for function name clashes WITHIN the entity! *)
(* This is annoying, but because function declarations inside Entities are this way,
we don't really have any other choice... *)
let verify_entity_functions env entity =
    (* Just iterate through functions, make sure names don't show up twice *)
    let map = StringMap.empty in 
    let aux result f_decl = 
        let e = fst result and m = snd result in
        let search = 
            try 
                (function a -> true) (StringMap.find f_decl.fname m )
            with
                Not_found -> false
            in
        if search then
            (* error message, because there shouldn't be another with same name *)
            let new_error = (
                    EntityName(entity.name), 
                    Scope(e.current_scope),
                    FunctionRepeatDecl(f_decl))
            in
            ( { e with errors = new_error :: e.errors }, m)
        else 
            ( e, (StringMap.add f_decl.fname f_decl m)) in

    let out = List.fold_left aux (env, map) entity.functions in
    fst out


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
    

(* honestly like the add_var_decl generalized for functions. Make sure
to specify the correct error locus before passing this in, and to make sure
the environment's scope number is right... *)
(* checks a function, updates environment *)
let check_function env possible_error_locus func = 
    (* 1. add variables *)
    let f env current_vdecl =  
        add_var_decl env possible_error_locus current_vdecl in

    List.fold_left f env func.locals 
   
    (* 2. go through each statement, checking the types *)



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

    let env_registered_toplevels =
        let aux env prog = 
            (* register each one appropriately, 
               TopLevel is there for possible error locus *)
            let handler env top_lvl_decl = match top_lvl_decl with
                | TopLevelFunction f -> add_function_decl env TopLevel f 
                | TopLevelVar v -> add_var_decl env TopLevel v 
                | TopLevelEntity e -> add_entity_decl env TopLevel e 
                in
            (* what emerges is a checked environment after toplevels *)
            List.fold_left handler env prog 
            in 
        aux environment top_level_program  in

    (* ANNOYING but before we look into the statements, might want to first make sure that 
    entity definitions are sensible *)

    let env_checked_toplevels = 
        let aux = fun env -> function TopLevelEntity(e) -> verify_entity_functions env e | _ -> env in  
        List.fold_left aux env_registered_toplevels top_level_program in


    (* We're just going to iterate through all the  *)
    (* only do the toplevel function *)

    let checking_functions = 

        let aux env toplevel_element = 
            match toplevel_element with 
                (* let check_function env possible_error_locus func *) 
                | TopLevelFunction f -> 

                    (* Update the environment to clean up the previous function scopes  *)
                    let revised_variables =
                        let empty_stringmap = StringMap.empty in
                        IntMap.add 1 empty_stringmap env.variables 
                        in
                    let fixed_env = { env with current_scope = 1; variables = revised_variables;} in
                    let locus = FunctionName(f.fname) in
                    check_function fixed_env locus f 

                (* discard any other sort of toplevel declaration *)
                |  _ -> env in
        List.fold_left aux env_checked_toplevels top_level_program
    in

    checking_functions
        

    
    (* apply this checking to each function in the toplevel *)


     (*THEN, after that...
       for each entity in the toplevel:
                1. add variables
                2. do exactly the same thing above, but
                for that scope and for those functions
        *) 

let semantic_check unchecked_program =
 
    (* check if checking_environment says there are any errors *) 
    let checked_environment = main_checker unchecked_program in

    (* Spits out all the errors *)
    let handler list_so_far next_error =
        let error_string = String.concat " " (describe_error next_error) in
        let with_nl = String.concat "" [error_string; "\n";] in
        list_so_far @ [ with_nl; ] 
        in

    let my_errors = List.fold_left handler [] checked_environment.errors in
    let result = String.concat "" my_errors in
    print_endline result
