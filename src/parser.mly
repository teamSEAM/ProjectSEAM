%{
  open Ast
%}

%token BOOL INT FLOAT STRING
%token ENTITY FUNC TEXTURE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMI COMMA DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
%token SPAWN KILL
%token <string> ID
%token <bool>   LIT_BOOL
%token <int>    LIT_INT
%token <float>  LIT_FLOAT
%token <string> LIT_STRING
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right SPAWN
%right KILL
%left DOT

%start program
%type <Ast.program> program

%%

program:
 | edecls EOF { List.rev $1 }

edecls:
 | /* nothing */ { [] }
 | edecls edecl { $2 :: $1 }

edecl:
 | ENTITY ID LBRACE vdecl_list fdecl_list RBRACE
     { { ename = $2;
	 fields = List.rev $4;
	 methods = $5; } }

fdecl_list:
 | /* nothing */    { [] }
 | fdecl fdecl_list { $1 :: $2 }

fdecl:
 | dtype ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { rtype = ActingType($1);
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8; } }
 | FUNC ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { rtype = Void;
	 fname = $2;
	 formals = $4;
	 locals = List.rev $7;
	 body = List.rev $8; } }

formals_opt:
 | /* nothing */ { [] }
 | formal_list   { List.rev $1 }

formal_list:
 | dtype ID                   { [ ($1, $2) ] }
 | formal_list COMMA dtype ID { ($3, $4) :: $1 }

vdecl_list:
 | /* nothing */    { [] }
 | vdecl_list vdecl { $2 :: $1 }

vdecl:
 | dtype ID SEMI { $1, $2 }

dtype:
 | BOOL   { Bool }
 | INT    { Int }
 | FLOAT  { Float }
 | STRING { String }
 | LT ID GT { Instance($2) }
 | dtype LBRACKET LIT_INT RBRACKET { Array($1, $3) }
 | TEXTURE { Texture }

stmt_list:
 | /* nothing */  { [] }
 | stmt_list stmt { $2 :: $1 }

stmt:
 | expr SEMI { Expr($1) }
 | RETURN expr SEMI { Return($2) }
 | LBRACE stmt_list RBRACE { Block(List.rev $2) }
 | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
 | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
 | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
 | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
 | KILL ID SEMI { Kill(Name($2)) }

expr_opt:
 | /* nothing */ { Noexpr }
 | expr          { $1 }

expr:
 | literal          { Literal($1) }
 | id               { Id($1) }
 | expr PLUS   expr { Binop($1, Add,   $3) }
 | expr MINUS  expr { Binop($1, Sub,   $3) }
 | expr TIMES  expr { Binop($1, Mult,  $3) }
 | expr DIVIDE expr { Binop($1, Div,   $3) }
 | expr EQ     expr { Binop($1, Equal, $3) }
 | expr NEQ    expr { Binop($1, Neq,   $3) }
 | expr LT     expr { Binop($1, Less,  $3) }
 | expr LEQ    expr { Binop($1, Leq,   $3) }
 | expr GT     expr { Binop($1, Greater,  $3) }
 | expr GEQ    expr { Binop($1, Geq,   $3) }
 | SPAWN ID         { Spawn($2) }
 | id ASSIGN expr   { Assign($1, $3) }
 | id LBRACKET expr RBRACKET    { Access($1, $3) }
 | id LPAREN actuals_opt RPAREN { Call($1, $3) }
 | LPAREN expr RPAREN { $2 }

literal:
 | LIT_BOOL { LitBool($1) }
 | LIT_INT { LitInt($1) }
 | LIT_FLOAT { LitFloat($1) }
 | LIT_STRING { LitString($1) }

id:
 | ID          { Name($1) }
 | expr DOT ID { Member(string_of_expr $1, $3) }

actuals_opt:
 | /* nothing */ { [] }
 | actuals_list  { List.rev $1 }

actuals_list:
 | expr                    { [$1] }
 | actuals_list COMMA expr { $3 :: $1 }
