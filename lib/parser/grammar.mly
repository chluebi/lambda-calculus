%{
open Lambda.V
%}

%token <string> ID
%token LPAREN LAMBDA ARROW RPAREN
%token LET EQ IN
%token EOF
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
  | LET; id = ID; EQ; e = expr; IN; body = expr { AppV (LambV (id, body), e) }
  | a = application { a }
;

application:
  e = primary_expr; rest = application_rest { List.fold_left (fun f arg -> AppV (f, arg)) e rest }
;

application_rest:
  | arg = primary_expr; rest = application_rest { arg :: rest }
  | { [] }
;

primary_expr:
  | LPAREN; e = expr; RPAREN { e }
  | id = ID                       { VarV id }
  | LPAREN; LAMBDA; id = ID; ARROW; body = expr; RPAREN    { LambV (id, body) }
;