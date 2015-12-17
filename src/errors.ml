include Ast

(* error_locus: 
        on the toplevel
        inside an entity: entity_name
        function_name: string *)
type error_locus = 
        | TopLevel
        | EntityName of string
        | FunctionName of string
        | EntitysFunction of string * string (* entity, then function names *)

(* error_scope:
        obviously if it's 0, it's the toplevel, but *)
type error_scope =
        | Scope of int


(* error_type *) 
type error_type = 
        (* repeat declarations:
        variable_repeat_decl: variable_name
        function_repeat_decl: function_name
        entity_repeat_decl: entity_name *)
        | VariableRepeatDecl of vdecl
        | FunctionRepeatDecl of fdecl
        | EntityRepeatDecl of entity_decl

        (* usage of a variable we did not declare
        undeclared_variable: variable_name, expression
        undeclared_function: bad_function_name, expression
        undeclared_entity: bad_entity_name, expression *)

        | UndeclaredVariable of string * expr

        (* type check errors: 
        as long as the names and the whole expression/statement
                are provided, they should have enough to figure it out
        expression_type_mismatch: expected_type, gotten_type, 
                expected_name, gotten_name, expression
        statement_type_mismatch: expected_type, gotten_type,
                expected_name, gotten_name, statement *)
        | ExpressionTypeMismatch of primitive * primitive * string * string * expr 
        | StatementTypeMismatch of primitive * primitive * string * string * stmt

        (* function usage issues
        function_parameter_count_mismatch: target_function, actual_function
        function_parameter_type_mismatch: target_function, actual_function *)
        | FunctionParamCountMismatch of string * expr list * fdecl
        | FunctionParamTypeMismatch of string * expr list * fdecl


        (* entity usage issues *)


(* an error is just the three together *)
type error = error_locus * error_scope * error_type

(* the convenient methods to print information about the errors *)


(* Took them away from ast.ml because these are ONLY  for error representations,
    NOT TO BE CONFUSED WITH CODE GENERATION! *)
let acting_type_to_str obj_type = 
        let primitive_equivalent = c_equivalents (fst obj_type) in
        let format_type = match (snd obj_type) with
                | Dynamic -> "[]"
                | ArraySize(i) -> String.concat "" [" ["; string_of_int i; "]";]
                | NotAnArray -> "" in
        String.concat "" [primitive_equivalent; format_type]

let format_vdecl v = 
        (acting_type_to_str (fst v)) :: [ snd v; ]



(* These following functions return lists of strings, since they're getting
assembled together anyway *)




let describe_error_locus = function 
        | TopLevel -> ["in the toplevel";]
        | EntityName(s) -> ["in an entity:"; s;]
        | FunctionName(f) -> ["in function:"; f;] 
        | EntitysFunction(e, f) -> ["in function:";f;"in entity";e;] 

let describe_error_scope = function
        | Scope i ->
           if i > 0 then ["in scope"; string_of_int i;]
           else []

let describe_error_type type_obj = match type_obj with 

(* repeat declarations: *)
        | VariableRepeatDecl v ->   
                "Repeat declaration of variable:" :: (format_vdecl v)
        | FunctionRepeatDecl f ->
                ["Repeat declaration of function:"; f.fname;] 
        | EntityRepeatDecl e ->
                ["Repeat declaration of entity:"; e.name;] 
(*
        (* usage of a variable we did not declare
        undeclared_variable: variable_name, expression
        undeclared_function: bad_function_name, expression
        undeclared_entity: bad_entity_name, expression *)

        | UndeclaredVariable of string * expr

        (* type check errors: 
        as long as the names and the whole expression/statement
                are provided, they should have enough to figure it out
        expression_type_mismatch: expected_type, gotten_type, 
                expected_name, gotten_name, expression
        statement_type_mismatch: expected_type, gotten_type, expected_name, gotten_name, statement *) 
        | ExpressionTypeMismatch of primitive * primitive * string * string * expr 
        | StatementTypeMismatch of primitive * primitive * string * string * stmt

        (* function usage issues
        function_parameter_count_mismatch: target_function, actual_function
        function_parameter_type_mismatch: target_function, actual_function *)
        | FunctionParamCountMismatch of string * expr list * fdecl
        | FunctionParamTypeMismatch of string * expr list * fdecl

       | *) 



let describe_error error_obj =
    match error_obj with (locus, scope, e_type) -> 
        (describe_error_type e_type) @ (describe_error_locus locus)
        @ (describe_error_scope scope) 
    (* first the locus *)
    





