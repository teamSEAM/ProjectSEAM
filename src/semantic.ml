include Errors (* note how if we need Ast, Errors includes Ast *)

(* ********************************************************************** *)
(*              semantic.ml is responsible for checking
          *     Return types
          *     Expression type checks 
          *     Undeclared identifiers being used
          *     Unset identifiers being used (to an extent)               *)
(* ********************************************************************** *)


(* ********************************************************************** *)
(*              Environment, types, settings made here                    *)
(* ********************************************************************** *)
module IntMap = Map.Make(struct type t = int let compare = compare end) (* for int map support *)
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

(* an example of how our checking environmentis made *)
let make_basic_env = 
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
    }
     
(* NOTE TO SELF: THIS IS NOW STRINGMAP WORKS *)
(*  StringMap.mem target map *)
(*  StringMap.add key addition map *)


(* ********************************************************************** *)
(*              this section handles base checking ops                    *) 
(* ********************************************************************** *)


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
                let result = StringMap.mem var var_map in
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


    let f env current_vdecl =  
        add_var_decl env possible_error_locus current_vdecl in

    (* 0: add formals BEFORE the variables, so that variables come into
        conflict with these formals already declared! *)
    (* note: sweet, I could completely reuse the above function *)
    let env = List.fold_left f env func.formals in 

    (* 1. add variables *)
    let env = List.fold_left f env func.locals in 
    env


   
    (* 2. go through each statement, checking the types *)


(* ********************************************************************** *)
(*                  a lot of special cases at toplevel                    *)
(* ********************************************************************** *)

(* for entities, used by toplevel handler *)
let add_entity_decl env possible_error_locus entity_decl =
    let name = entity_decl.name in
    let entities = env.entities in
    let found = StringMap.mem name entities in
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



(* for toplevel functions, used by toplevel handler *)
let add_function_decl env possible_error_locus function_decl =
    let name = function_decl.fname in
    let functions = env.functions in 
    let found = StringMap.mem name functions in
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




let handle_toplevel environment ast_head =
    let register_aux env prog = 
        (* depends on the type of toplevel decl *)
        let handler env top_lvl_decl = match top_lvl_decl with
            | TopLevelFunction f -> add_function_decl env TopLevel f 
            | TopLevelVar v -> add_var_decl env TopLevel v 
            | TopLevelEntity e -> add_entity_decl env TopLevel e 
            in
        List.fold_left handler env prog 
        in 

    (* make sure entity declarations don't have duplicate functions *)
    (* note we couldn't reuse the add-function thing because these aren't
    added anywhere - entity/function associates are just looked up if need be *)

     let verify_entity_functions env entity =
        let map = StringMap.empty in 
        let aux result f_decl = 
            let e = fst result and m = snd result in
            let search = 
                try  (function a -> true) (StringMap.find f_decl.fname m )
                with Not_found -> false in
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
        fst out in

    let check_entity_functions input_env ast_head = 
        let aux = fun env -> function TopLevelEntity(e) -> verify_entity_functions env e | _ -> env in  
        List.fold_left aux input_env ast_head in

  
    let registered = register_aux environment ast_head in
    check_entity_functions registered ast_head

    (* Also needs something to make sure entity declarations don't have
    duplicate variables, but this is easier because the normal
    variable checking works for this: *)
    (* Instead of checking here, we'll just do this as part of our normal entity function handling, 
    because we begin that by adding the variables, in fact *)



(* ********************************************************************** *)
(*                   main compiler's entry and usage point                *)
(* ********************************************************************** *)

let main_checker ast_head = 

    (* take care of setup *)
    let environment = make_basic_env in
   
    let toplevel_done = handle_toplevel environment ast_head in 

    (* We're just going to iterate through all the  *)
    (* only do the toplevel function *)

    let top_f_aux env toplevel_element = match toplevel_element with 
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
        
    (* apply function above, and get one with a checked toplevel functions *)
    let toplevel_functions_done = List.fold_left top_f_aux toplevel_done ast_head in
 
    (*Now do the same for every entity: 
        1. add variables
        2. do function by function *)   
    let do_entities env toplevel_element = match toplevel_element with
            | TopLevelEntity e ->
               
                (* FIRST, new entity, move from scope #0 to a fresh #1: *)
                (* TODO: GENERALIZE THIS scope manipulation to a function *)
                let revised_env  =
                    let empty_stringmap = StringMap.empty in
                    let new_vars = IntMap.add 1 empty_stringmap env.variables in
                    { env with current_scope = 1; variables = new_vars;} in 

                (* Next add the variables, making sure to catch repeat declarations *)
                (* error locus: an entity's toplevel *)
                let possible_error_locus = EntityName(e.name) in
                let env_with_variables_added = 
                    let aux current_env current_vdecl =
                        add_var_decl current_env possible_error_locus current_vdecl in
                    List.fold_left aux revised_env e.members in

                
                let check_function_aux curr_env curr_fdecl = 

                    (* We really need a generalized scope manipulator function ...  *)
                    (* But yeah, function contents in entities are scope #2 *)
                    let revised_env  =
                        let empty_stringmap = StringMap.empty in
                        let new_vars = IntMap.add 2 empty_stringmap curr_env.variables in
                        { env with current_scope = 2; variables = new_vars;} in 

                    (* Locus depends on entity and function so... *)
                    let possible_error_locus = EntitysFunction(e.name, curr_fdecl.fname) in

                    (* Now we check functions *)
                    check_function revised_env possible_error_locus curr_fdecl in

                List.fold_left check_function_aux env_with_variables_added e.functions

            | _ -> env in

    List.fold_left do_entities toplevel_functions_done ast_head
    

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
