%{
open Lambda.V
%}

%token <string> ID
%token LPAREN LAMBDA ARROW RPAREN APP
%token EOL
%left APP               (* lowest precedence *)
%start main             (* the entry point *)
%type <t> main
%type <t> expr

%%

main:
  e = expr; EOL                  { e }
;

expr:
  | ID                       { VarV $1 }
  | LPAREN; LAMBDA; id = ID; ARROW; body = expr; RPAREN    { LambV (id, body) }
  | f = expr APP arg = expr            { AppV (f, arg) }
;
