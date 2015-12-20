%{ open Ast %}

/* %token ENTITY MAIN FUNCTION TEXTURE */
%token STRING FLOAT INT INSTANCE

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUAREBRACE RSQUAREBRACE COMMA DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token IF ELSE
%token PRINT RETURN FUNCTION ENTITY %token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%nonassoc ELSE
%nonassoc NOELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%
/* Basic Variable stuff */

primitive:
    STRING { Str }
  | FLOAT { Float }
  | INT { Int }
  | INSTANCE { Instance }

acting_type:
        | primitive { $1, NotAnArray }
        | primitive LSQUAREBRACE INT_LITERAL RSQUAREBRACE {
                $1, ArraySize($3) }
        | primitive LSQUAREBRACE RSQUAREBRACE {
                $1, Dynamic }

ret_type:
    | FUNCTION acting_type  FUNCTION { ActingType($2) }
    | FUNCTION { Void }

vdecl_list:
  { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   acting_type ID SEMI { $1, $2 }



/* Function declarations */

fdecl:
    ret_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    {{ vtype = $1;
       fname = $2;
       formals = $4;
       locals = List.rev $7;
       body = List.rev $8 }}

/* Formal arguments for functions */

formals_opt:
{ [] }
| formal_list   { List.rev $1 }

formal_list:
  formal_id                   { [$1] }
| formal_list COMMA formal_id { $3 :: $1 }

formal_id:
  acting_type ID { $1, $2 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

fdecl_list:
  { [] }
  | fdecl_list fdecl { $2 :: $1 }


entity_decl:
        ENTITY ID LBRACE vdecl_list fdecl_list RBRACE
        {
          {
                        name = $2;
                        members = $4;
                        functions = $5;
          }
        }

/* THE ENTIRE PROGRAM */
program:
        | decls EOF {$1}

decls:
        | { [] }
        | decls toplevel_element { $2 :: $1 }

toplevel_element:
        | entity_decl { TopLevelEntity($1) }
        | fdecl { TopLevelFunction($1) }
        | vdecl { TopLevelVar($1) }



expr:
| STRING_LITERAL                        { StrLit($1)	         }
| INT_LITERAL                           { IntLit($1)             }
| ID         			     	{ Id($1)                 }
| expr PLUS   expr 			{ Binop($1, Add, $3) 	 }
| expr MINUS  expr 			{ Binop($1, Sub, $3) 	 }
| expr TIMES  expr 			{ Binop($1, Mult, $3) 	 }
| expr DIVIDE expr 			{ Binop($1, Div, $3)	 }

| ID ASSIGN expr 			{ Assign($1, $3)	 }
| LPAREN expr RPAREN 			{ $2		        }

| expr EQ expr   		   	{ Binop($1, Equal, $3) 	 }
| expr NEQ expr  	  		{ Binop($1, Neq, $3) 	 }
| expr LT expr   	  		{ Binop($1, Less, $3) 	 }
| expr LEQ expr  		   	{ Binop($1, Leq, $3) 	 }
| expr GT expr   		   	{ Binop($1, Greater, $3) }
| expr GEQ expr  		   	{ Binop($1, Geq, $3)	 }


stmt:
  expr SEMI { Expr($1) }
| LBRACE stmt_list RBRACE { Block(List.rev $2) }
| RETURN expr SEMI { Return($2) }
| PRINT expr SEMI { Print($2) }

/*| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
*/| IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
