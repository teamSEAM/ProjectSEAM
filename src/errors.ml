include Ast



(* everything's in an entity and a function *)
type error_locus = 
|Global
|Entity of string
|EntitysFunction of string * string (* provide their names *)

(* error_type *)
type error_type =
        (* repeat declarations:
        variable_repeat_decl: variable_name
        function_repeat_decl: function_name
        entity_repeat_decl: entity_name *)
        | VariableRepeatDecl of vdecl
        | FunctionRepeatDecl of fdecl
        | EntityRepeatDecl of edecl

        (* usage of a variable we did not declare
        undeclared_variable: variable_name, expression
        undeclared_function: bad_function_name, expression
        undeclared_entity: bad_entity_name, expression *)

(* 
type dtype = Bool | Int | String | Float | Instance of string | Array of dtype * int
type rtype = Void | ActingType of dtype
*)
        | UndeclaredVariable of string * expr

        (* the type I wanted, and the type I got *)
        | StatementTypeMismatch of rtype * rtype * string
        | BinopTypeMismatch of rtype * op * rtype  

        | AssignmentError of rtype * rtype 

        | WorldNotFound
        | EntityFunctionsHasArgs
        


(* an error is just the three together *)
type error = error_locus * error_type

let rec dtype_to_str = function
| Bool -> "Bool"
| Int -> "Int"
| String -> "String"
| Float -> "Float" 
| Instance(s) -> String.concat "" ["Instance of "; s;] 
| Array(d, size) -> String.concat "" 
    [dtype_to_str d; " ["; string_of_int size; "]"; ]
 
let rtype_to_str = function
| Void -> "Void type" 
| ActingType(t) -> dtype_to_str t

let format_vdecl v =
        (dtype_to_str (fst v)) :: [ snd v; ]


let describe_error_locus = function
    |Global -> ["when declaring entities"]
    |Entity(s) -> ["in an entity:"; s;]
    |EntitysFunction(e, f) -> ["in function:";f;"in entity";e;]

let describe_binop_type = function
| Add -> "Addition"  
| Sub -> "Subtraction"| Mult -> "Multiplication" 
| Div -> "Division" | Equal -> "==" 
| Neq -> "!=" | Less -> "Less-than"
| Leq -> "Less-than/equal" | Greater -> "Greater-than" 
| Geq -> "Greater-than/equal"

let describe_error_type type_obj = match type_obj with
    (* repeat declarations: *)
        | VariableRepeatDecl v ->
                "Repeat declaration of variable:" :: (format_vdecl v)
        | FunctionRepeatDecl f ->
                ["Repeat declaration of function:"; f.fname;]
        | EntityRepeatDecl e ->
                ["Repeat declaration of entity:"; e.ename;]

        | UndeclaredVariable (id_name, expr) ->
               ["Use of uninitialized variable:"; id_name;] 

        | StatementTypeMismatch(expected, actual, problem) ->
               ["Found a"; rtype_to_str actual; 
               "when we were looking for a"; 
               rtype_to_str expected; problem;]

        | BinopTypeMismatch (rtype1, op, rtype2) ->
            ["Binop type error in"; describe_binop_type op;
                "on"; rtype_to_str rtype1; "and";
                rtype_to_str rtype2;]
        | AssignmentError(rtype_var, rtype_exp) ->
            [ "Tried to set variable of type";
                rtype_to_str rtype_var;
                "to expression of type";
                rtype_to_str rtype_exp; ]
let describe_error error_obj =
    match error_obj with (locus, e_type) ->
        (describe_error_type e_type) @ (describe_error_locus locus)

