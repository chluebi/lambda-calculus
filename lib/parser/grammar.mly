%{
open Frontend.Frontend
%}

%token <string> ID
%token <int> INT
%token LPAREN RPAREN
%token LAMBDA ARROW
%token LET EQ IN
%token IF THEN ELSE
%token PLUS MINUS STAR EXP
%token AND OR NOT
%token EOF
%start main
%type <t> main
%type <t> expr

%%

main:
  e = expr; EOF                  { e }
;

expr:
  | IF; cond = expr; THEN; if_b = expr; ELSE; else_b = expr { IfThenElse (cond, if_b, else_b) }
  | LET; id = ID; EQ; e = expr; IN; body = expr { LetinF (id, e, body) }
  | e = lambda_expr { e }
  | e = app_expr { e }
  | e = primary_expr { e }
;

lambda_expr:
  | LPAREN; LAMBDA; ids = id_list; ARROW; body = expr; RPAREN { LambF (ids, body) }
;

id_list:
  | id = ID; rest = id_list { id :: rest }
  | id = ID { [id] }
;

app_expr:
  e = primary_expr; rest = app_rest { AppF(e, rest) }
;

app_rest:
  | arg = primary_expr; rest = app_rest { arg :: rest }
  | { [] }
;

primary_expr:
  | LPAREN; e = expr; RPAREN { e }
  | id = ID { VarF id }
  | n = INT { NumF n }
  | u = unop; e = primary_expr { UnopF (u, e) }
  | e1 = primary_expr; b = binop; e2 = primary_expr { BinopF (b, e1, e2) }
;

binop:
  | PLUS { IntOpPlus }
  | MINUS { IntOpMinus }
  | STAR { IntOpMult }
  | EXP { IntOpExp }
  | AND { BoolOpAnd }
  | OR { BoolOpOr }
;

unop:
  | MINUS { IntOpNeg }
  | NOT { BoolOpNot }
;
