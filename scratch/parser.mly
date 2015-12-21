%{
  open Ast

  let fst3 (a,b,c) = a
  let snd3 (a,b,c) = b
  let trd3 (a,b,c) = c
%}

%token BOOL INT FLOAT STRING
%token ENTITY INSTANCE FUNC
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token SEMI COMMA DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
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

%start program
%type <Ast.program> program

%%

program:
 | decls EOF { List.rev (fst3 $1), List.rev (snd3 $1), List.rev (trd3 $1) }

decls:
 | /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: fst3 $1), snd3 $1 , trd3 $1 }
 | decls edecl { fst3 $1, ($2 :: snd3 $1), trd3 $1 }
 | decls fdecl { fst3 $1, snd3 $1, ($2 :: trd3 $1) }

edecl:
 | ENTITY ID LBRACE vdecl_list fdecl_list RBRACE
     { { ename = $2;
	 fields = List.rev $4;
	 methods = List.rev $5; } }

fdecl_list:
 | /* nothing */    { [] }
 | fdecl_list fdecl { $2 :: $1 }

fdecl:
 | atype ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
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
 | atype ID                   { [ ($1, $2) ] }
 | formal_list COMMA atype ID { ($3, $4) :: $1 }

vdecl_list:
 | /* nothing */    { [] }
 | vdecl_list vdecl { $2 :: $1 }

vdecl:
 | atype ID SEMI { $1, $2 }

atype:
 | primitive array_size { $1, $2 }

primitive:
 | BOOL   { Bool }
 | INT    { Int }
 | FLOAT  { Float }
 | STRING { String }
 | INSTANCE ID { Instance($2) }

array_size:
 | /* nothing */             { NotAnArray }
 | LBRACKET RBRACKET         { Dynamic }
 | LBRACKET LIT_INT RBRACKET { ArraySize($2) }

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
 | id ASSIGN expr   { Assign($1, $3) }
 | id LPAREN actuals_opt RPAREN { Call($1, $3) }
 | LPAREN expr RPAREN { $2 }

literal:
 | LIT_BOOL { LitBool($1) }
 | LIT_INT { LitInt($1) }
 | LIT_FLOAT { LitFloat($1) }
 | LIT_STRING { LitString($1) }

id:
 | ID        { Name($1) }
 | id DOT ID { Member($1, $3) }

actuals_opt:
 | /* nothing */ { [] }
 | actuals_list  { List.rev $1 }

actuals_list:
 | expr                    { [$1] }
 | actuals_list COMMA expr { $3 :: $1 }
