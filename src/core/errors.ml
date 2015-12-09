include Ast

(* error_locus: 
        on the toplevel
        inside an entity: entity_name
        function_name: string *)
type error_locus = 
        | TopLevel
        | EntityName of string
        | FunctionName of string

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



(* brainstorming *)

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
*)

