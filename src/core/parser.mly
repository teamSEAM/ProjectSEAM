%{ open Ast %}

/* %token ENTITY MAIN FUNCTION TEXTURE */
%token STRING FLOAT INT 

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
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
| decls fdecl 

(* Function declarations *)

(* Formal arguments for functions *)

formals_opt: /*Taken from microc*/
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list: /*Taken from microc*/
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

var_type:
    STRING { Str }
  | INT { Int }

fdecl: 
    var_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    {{ vtype = $1;
       fname = $2;
       formals = $4;
       locals = List.rev $7;
       body = List.rev $8 }}

vdecl_list: /*Taken from microc*/
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl: /*Taken from microc*/
   INT ID SEMI { $2 }

stmt_list: /*Taken from microc*/
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* NEEDS WORK HERE! ENTITY LAYOUT. SEAN WILL DO RESEARCH */
edecl:
	ENTITY LBRACE  RBRACE

expr:
| LITERAL          			{ Lit($1) 								 }
| ID         				{ Id($1) 								 }
| expr PLUS   expr 			{ Binop($1, Add, $3) 					 }
| expr MINUS  expr 			{ Binop($1, Sub, $3) 					 }
| expr TIMES  expr 			{ Binop($1, Mul, $3) 					 }
| expr DIVIDE expr 			{ Binop($1, Div, $3)					 }
	
| ID ASSIGN expr 			{ Asn($1, $3) 							 }
| LPAREN expr RPAREN 		{ $2 									 }

| expr EQ expr   			{ Binop($1, Equal, $3) 		 			 }
| expr NEQ expr  			{ Binop($1, Neq, $3) 					 }
| expr LT expr   			{ Binop($1, Less, $3) 			 		 }
| expr LEQ expr  			{ Binop($1, Leq, $3) 					 }
| expr GT expr   			{ Binop($1, Greater, $3) 				 }
| expr GEQ expr  			{ Binop($1, Geq, $3)				 	 }


stmt: g
  expr SEMI { Expr($1) } 
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([]))	 } 
| IF LPAREN expr RPAREN stmt ELSE stmt 	  {s If($3, $5, $7) 		 } 
| FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
/*NEED TO COUNT THE TABS */
/* NO RETURN IMPLEMENTED YET, nor 
|LBRACE stmt_list RBRACE { Block(List.rev $2) }
| RETURN expr SEMI { Return($2) }
*/
expr_opt:  /*Taken from microc*/
    /* nothing */ { Noexpr }
  | expr          { $1 }


actuals_opt: /*Taken from microc*/
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list: /*Taken from microc*/
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
