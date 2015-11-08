%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE
%token EOL EOF ASSIGN SEQUENCE 
%token EQ NEQ LT LEQ GT GEQ RETURN IF ELSE /*SEAN */

%token LPAREN RPAREN SQRB SQLB 
%token COMMA DOT COLON SEMI/* SEAN */
%token WHILE FOR
%token INT FLOAT
%token ENTITY MAIN FUNCTION TEXTURE

%nonassoc NOELSE /* Precedence and associativity of each operator */
%nonassoc ELSE

%token INDENT DEDENT
%token <int> LITERAL
%token <string> ID

%left SEQUENCE
%right ASSIGN
%left EQ NEQ /*SEAN*/
%left LT GT LEQ GEQ /*SEAN*/
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
| LITERAL          			{ Lit($1) }
| ID         				{ Id($1) }
| expr PLUS   expr 			{ Binop($1, Add, $3) }
| expr MINUS  expr 			{ Binop($1, Sub, $3) }
| expr TIMES  expr 			{ Binop($1, Mul, $3) }
| expr DIVIDE expr 			{ Binop($1, Div, $3) }

| ID ASSIGN expr { Asn($1, $3) }
| LPAREN expr RPAREN { $2 }/*SEAN DONE HERE*/

| expr EQ expr { Binop($1, Equal, $3) }
| expr NEQ expr { Binop($1, Neq, $3) }
| expr LT expr { Binop($1, Less, $3) }
| expr LEQ expr { Binop($1, Leq, $3) }
| expr GT expr { Binop($1, Greater, $3) }
| expr GEQ expr { Binop($1, Geq, $3) }


stmt:
| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) } 
| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) } 
