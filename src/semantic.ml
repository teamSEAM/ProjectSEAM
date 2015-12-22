include Errors (* note how if we need Ast, Errors includes Ast *)

module IntMap = Map.Make(struct type t = int let compare = compare end) (* for int map support *)
module StringMap = Map.Make(String)


(* The following is my procedure:
    
    Perform repeat entity declaration checks
    Perform repeat function declaration checks
    Perform repeat variable declaration checks
    Iterate through the functions to check everything 

 *)

type translation_env = {
    current_scope: int;
    (* a map from scopes to the map of things in each scope,
    which maps the variable name to a vdecl *)
    variables: vdecl StringMap.t IntMap.t;
    entities: edecl StringMap.t;
    functions: fdecl StringMap.t;

    (* errors *)
    errors: error list;
}


(* //////////////////////////////////////////////////////////////
        auxiliary functions for variables and scoping *)

(* first let's introduce this auxiliary function for the add_var_decl
    and also useful in expression checking *)
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


let add_var_decl env possible_error_locus var_decl =

    (* use find_variable_scope *)
    let var_name = snd var_decl in
    let scope_number = find_variable_scope env var_name in

    (* react accordingly *)
    if scope_number == env.current_scope then
        (* error, we have a duplicate variable declaration
                inside the same scope... *)
        let new_error = (
                possible_error_locus,
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


(* Auxiliary function to set a given scope's variables to zero *)
let clear_variable_scope env scope_number = 
    let revised_variables =
        let empty_stringmap = StringMap.empty in
        IntMap.add scope_number empty_stringmap env.variables
        in
    let fixed_env = { env with variables = revised_variables;}
    in fixed_env

let make_basic_env =
    let empty_intmap = IntMap.empty in
    let basic_environment = 
    {
        current_scope = 0;
        variables = empty_intmap;
        entities = StringMap.empty;
        functions = StringMap.empty;
        errors = [];
    } in
    clear_variable_scope basic_environment 0


(* In fact searching for the ID should be generalized *)
let check_id_usage env expr error_locus identifier = match identifier with
    | Member(entity, id_name) ->

        (env, Void )
        (* use our searcher *)
    | Name(id_name) ->  
        let scope = find_variable_scope env id_name in
        if scope < 0 then
            (* We didn't even find it gg *)
            (* Error message in environment, then spit out a Void result *)
            let new_error = (error_locus, UndeclaredVariable(id_name, expr)) in
            let updated_env = { env with errors = new_error :: env.errors } in
            ( updated_env, Void)  
        else
            (* this is a Stringmap *)
            let var_map = IntMap.find scope env.variables in
            let dtype = fst (StringMap.find id_name var_map) in
            let wrapped_dtype = ActingType(dtype) in
            ( env, wrapped_dtype) 
            



(* //////////////////////////////////////////////////////////////
    the meat of the checking is here  *)

(* We will return a type of rtype, with the possibility of Void,
    the absense of return *)
let rec check_expression env func error_locus expr = match expr with
| Noexpr -> (env, Void) 
| Literal (lit) -> 
    let lit_dtype_lookup = function
        | LitBool(b) -> Bool
        | LitInt(i) -> Int
        | LitFloat(f) -> Float 
        | LitString(s) -> String 
        | LitArray(_, _) -> Int in  
    let equiv_dtype = match lit with
    | LitArray(inner_lit, i) -> ActingType( 
        Array( lit_dtype_lookup inner_lit, i) )
    | LitBool(b) -> ActingType(Bool)
    | LitInt(i) ->ActingType(Int)
    | LitFloat(f) ->ActingType(Float)
    | LitString(s) ->ActingType(String)         in (env, equiv_dtype)

| Call(id, []) -> (env, Void ) (*of identifier * expr list (* functions and methods *) *)
| Call(id, hd::tl) -> (env, Void ) (*of identifier * expr list (* functions and methods *) *)
| Binop(e1, o, e2) ->
    (* First, check e1 and e2 *)
    let tuple1 = check_expression env func error_locus e1 in
    let tuple2 = check_expression (fst tuple1) func error_locus e2 in

    (* Next, compare their types *)
    let type1 = snd tuple1 in
    let type2 = snd tuple2 in


    let resulttype = match o with
    | Add | Sub | Mult | Div -> type1 
    | Equal| Neq | Less | Leq | Greater| Geq ->ActingType(Bool) in


    let env = fst tuple2 in
    if type1 != type2 then
        let error_type = BinopTypeMismatch (type1, o, type2) in
        let new_error = ( error_locus, error_type) in
        let updated_env = { env with errors = new_error :: env.errors } in
        (updated_env, type1)
    else 
        (env, resulttype) 

| Assign(id, val_expr) 
    -> 
    (* check expr, then get its type *)
    let tuple1 = check_expression env func error_locus val_expr in
    let updated_env = fst tuple1 in 
    (* check id, then get its type *)
    let tuple2 = check_id_usage updated_env expr error_locus id in
    (* check that the types are the same *)
    let type1 = snd tuple1 in
    let type2 = snd tuple2 in

    let str1 = rtype_to_str type1 in
    let str2 = rtype_to_str type2 in

    if String.compare str1 str2 == 0  then
        (fst tuple2, type1) 
    else
        (* it's this order because type TWO comes from the id *)
        let error_type = AssignmentError(type2, type1) in
        let new_error = ( error_locus, error_type) in
        let updated_env = { env with errors = new_error :: env.errors } in
        (updated_env, type1)





| Access(id, expr) -> (env, Void ) (*of identifier * expr    (* array access *) *)
| Id(id) -> 
    check_id_usage env expr error_locus id 



(* checks a given statement. returns env with possible errors *)
let rec check_statement env func error_locus statement = match statement with
    (* Nothing happens if it's an empty block *)
    | Block ([]) -> env

    (* Handle head, then handle the tail *)
    | Block (hd :: tl) -> 
        let head_env = check_statement env func error_locus hd in
        let the_rest = Block(tl) in
        check_statement head_env func error_locus the_rest 

    (* we do not care about the type *)
    | Expr (e) -> 
        let out_tuple = check_expression env func error_locus e in
        fst out_tuple

    (* We care that return matches up with the func declaration *) 
    | Return (e) -> 
         
    env 

    (* We care that e is a boolean, and then check statements *)
    | If (e, stmt1, stmt2) -> 
        let tuple = check_expression env func error_locus e in
        let environment = match (snd tuple) with
                | Void -> 
                    let new_error = ( error_locus, 
                        StatementTypeMismatch(ActingType(Bool),
                        Void, "a if statement") ) in 
                    { env with errors = new_error :: env.errors }
                | ActingType t -> match t with 
                    | Bool-> env
                    | _ -> 
                        let actualtype = ActingType(t) in
                        let new_error = ( error_locus, 
                            StatementTypeMismatch(ActingType(Bool),
                                actualtype, "a if statement") ) in 
                            { env with errors = new_error :: env.errors } in
        let env2 = check_statement environment func error_locus stmt1  in
        check_statement env2 func error_locus stmt2 


    | For (exp1, exp2, exp3, s) -> 
        (* For for loops, we honestly couldn't care about the
        expression types, they can do stupid things in it like C permits you to *)
        let e1 = fst (check_expression env func error_locus exp1) in
        let e2 = fst (check_expression e1 func error_locus exp2) in
        let e3 = fst (check_expression e2 func error_locus exp3) in
        check_statement e3 func error_locus s 
    | While (e, s) ->
        (* again, caring that our expression is a boolean *)
        let tuple = check_expression env func error_locus e in
        let environment = match (snd tuple) with
                | Void -> 
                    let new_error = ( error_locus, 
                        StatementTypeMismatch(ActingType(Bool),
                        Void, "a while statement") ) in 
                    { env with errors = new_error :: env.errors }
                | ActingType t -> match t with 
                    | Bool-> env
                    | _ -> 
                        let actualtype = ActingType(t) in
                        let new_error = ( error_locus, 
                            StatementTypeMismatch(ActingType(Bool),
                                actualtype, "a while statement") ) in 
                            { env with errors = new_error :: env.errors }
        in check_statement environment func error_locus s 

(* checks a function, updates environment *)
let check_function env possible_error_locus func =

    (* A variable adde and error-maker *)
    let f env current_vdecl =
        add_var_decl env possible_error_locus current_vdecl in

    (* 0: add formals BEFORE the variables, so that variables come into
        conflict with these formals already declared! *)
    (* note: sweet, I could completely reuse the above function *)
    let env = List.fold_left f env func.formals in

    (* 1. add variables *)
    let env = List.fold_left f env func.locals in

    (* 2. go through each statement, checking the types *)
    let f env current_statement =
        check_statement env func possible_error_locus current_statement in

    List.fold_left f env func.body 







let main_checker ast_head =

    
    let basic_env = make_basic_env in

    (* //////////////////////////////////////////////////////////////
        first, verify that no entities have been duplicated *)
    let verified_duplicate_entities =  
        let f env e = 
           (* Add entity to our environment, check for duplicates *) 
            let name = e.ename in
            let entities = env.entities in
            let found = StringMap.mem name entities in
            if found then
                (* error message, because there shouldn't be another with same name *)
                let new_error = (
                        Global,
                        EntityRepeatDecl(e))
                    in
                { env with errors = new_error :: env.errors }
            else
                let updated_entities = StringMap.add name e entities in
                    { env with entities = updated_entities; } in
        List.fold_left f basic_env ast_head  
    in 
    (* //////////////////////////////////////////////////////////////
        next go entity by entity to 1. check repeat function decls 
                                and 2. handle each function *)

    let do_each_entity env entity =

        (* now for each entity... *)

        (* The part that sees if we have duplicate functions *)
        let verify_entity_functions env function_list =
            let map = StringMap.empty in
            let aux result f_decl =
                (* we're passing a tuple around with both the updated environment
                    and a map that acts as a set for whether we have a function already *)
                let e = fst result and m = snd result in
                let search =
                    try  (function a -> true) (StringMap.find f_decl.fname m )
                    with Not_found -> false in
                if search then
                    (* error message, because there shouldn't be another with same name *)
                    let new_error = (
                            Entity(entity.ename),
                            FunctionRepeatDecl(f_decl))
                    in
                    ( { e with errors = new_error :: e.errors }, m)
                else
                    ( e, (StringMap.add f_decl.fname f_decl m)) in
                let out = List.fold_left aux (env, map) function_list in
                fst out in
       
        let env_after_verifying_functions = verify_entity_functions env entity.methods in

    
        (* The part that sees if we have duplicate variables *)
        let verify_entity_variables env locals = 
            let error_locus = Entity(entity.ename) in
            let cleaned_env = clear_variable_scope env 0 in
            let f env current_vdecl =
                (* let add_var_decl env possible_error_locus var_decl *)
                add_var_decl env error_locus current_vdecl in
            List.fold_left f env locals in

        (* NOTE: at this point, we also have the variables registered
            in the scope 0 of the environment!! *)
        let env_verified_vars = verify_entity_variables env_after_verifying_functions entity.fields in

        (* Finally, delve into each function and check things over *)

        let check_function_aux curr_env curr_fdecl =

            (* Do not forget - the contents are all SCOPE #1 *)
            let revised_env =
                (* use our aux, but set current_scope manually!! *)
                let cleared = clear_variable_scope curr_env 1 in
                { cleared with current_scope = 1; } in

            (* Locus depends on entity and function so... *)
            let possible_error_locus = EntitysFunction(entity.ename, curr_fdecl.fname) in

            (* Now we check functions *)
            check_function revised_env possible_error_locus curr_fdecl in

        List.fold_left check_function_aux env_verified_vars entity.methods in

    (* Right this is where we apply that massive aux function to
    every entity there is *)
    List.fold_left do_each_entity verified_duplicate_entities ast_head


let semantic_check unchecked_program =
    (* check if checking_environment says there are any errors *)
    let checked_environment = main_checker unchecked_program in


    (* Spits out all the errors *)
    let handler list_so_far next_error =
        let error_string = String.concat " " (describe_error next_error) in
        let with_nl = String.concat "" [error_string; "\n";] in
        list_so_far @ [ with_nl; ]
        in

    (* we list.rev the errors because errors are always appended left, 
        thus they are backwards compared to the order in which they came *)
    let my_errors = List.fold_left handler [] (List.rev checked_environment.errors) in
    let result = String.concat "" my_errors in

    if List.length checked_environment.errors == 0 then
        ""
    else
        result (* We return a string from semantic; if empty, no errors *)      
