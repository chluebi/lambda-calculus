%{
open Lambda.V
%}

%token <string> ID
%token LPAREN LAMBDA ARROW RPAREN APP
%token LET EQ IN
%token EOF
%left APP               (* lowest precedence *)
%start main             (* the entry point *)
%type <t> main
%type <t> expr

%%

main:
  e = expr; EOF                  { e }
;

expr:
  | LPAREN; e = expr; RPAREN { e }
  | id = ID                       { VarV id }
  | LPAREN; LAMBDA; id = ID; ARROW; body = expr; RPAREN    { LambV (id, body) }
  | f = expr; arg = expr            { AppV (f, arg) }
  | LET; id = ID; EQ; e = expr; IN; body = expr { AppV (LambV (id, body), e) }
;
