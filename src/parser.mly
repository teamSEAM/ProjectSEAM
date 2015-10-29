%{ open Ast %}

%token PLUS MINUS MULTIPLY DIVIDE EOF 
%token LBRACKET RBRACKET SQLB SQRB  
%token COMMA PERIOD COLON
%token ASSIGN NOTEQUAL EQUAL
%token GT LT GTE LTE 
%token ELSE IF WHILE FOR
%token INT FLOAT
%token ENTITY MAIN TEXTURE FUNCTION

%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| LITERAL          { Lit($1) }
