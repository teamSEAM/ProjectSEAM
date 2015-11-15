%{ open Ast %}

/* %token ENTITY MAIN FUNCTION TEXTURE */
%token STRING FLOAT INT 

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN FUNCTION
%token <int> INT_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token EOF

%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
| decls EOF {$1}

decls:
| { [] } 
| decls fdecl { $2 :: $1 }

/* Function declarations */

/* Formal arguments for functions */

formals_opt: 
    /* nothing */ { [] }
| formal_list   { List.rev $1 }

formal_list: 
  formal_id                   { [$1] }
| formal_list COMMA formal_id { $3 :: $1 }

formal_id:
  var_type ID { $1, $2 }

func_type:
    FUNCTION { Void }
  | STRING { Str }
  | INT { Int }

var_type:
    STRING { Str }
  | INT { Int }

fdecl: 
    func_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    {{ vtype = $1;
       fname = $2;
       formals = $4;
       locals = List.rev $7;
       body = List.rev $8 }}

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl: 
   var_type ID SEMI { $1, $2 }

stmt_list: 
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

expr:
| STRING_LITERAL          	{ StrLit($1)							 }
| INT_LITERAL               { IntLit($1)                             }
| ID         				{ Id($1) 								 }
| expr PLUS   expr 			{ Binop($1, Add, $3) 					 }
| expr MINUS  expr 			{ Binop($1, Sub, $3) 					 }
| expr TIMES  expr 			{ Binop($1, Mult, $3) 					 }
| expr DIVIDE expr 			{ Binop($1, Div, $3)					 }
	
| ID ASSIGN expr 			{ Assign($1, $3)						 }
| LPAREN expr RPAREN 		{ $2 									 }

| expr EQ expr   			{ Binop($1, Equal, $3) 		 			 }
| expr NEQ expr  			{ Binop($1, Neq, $3) 					 }
| expr LT expr   			{ Binop($1, Less, $3) 			 		 }
| expr LEQ expr  			{ Binop($1, Leq, $3) 					 }
| expr GT expr   			{ Binop($1, Greater, $3) 				 }
| expr GEQ expr  			{ Binop($1, Geq, $3)				 	 }

stmt:
  expr SEMI { Expr($1) } 
| LBRACE stmt_list RBRACE { Block(List.rev $2) }
| RETURN expr SEMI { Return($2) }
