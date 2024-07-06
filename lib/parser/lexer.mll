{
open Grammar
}

rule read = parse
| ['\t' '\n' ' ']
    { read lexbuf }
| eof
    { EOF }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '\\'
    { LAMBDA }
| "->"
    { ARROW }
| "let"
    { LET }
| '='
    { EQ }
| "in"
    { IN }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { STAR }
| '^'
    { EXP }
| '&'
    { AND }
| '|'
    { OR }
| '~'
    { NOT }
| ['0'-'9']+ as x
    { INT (int_of_string x) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as x
    { ID x }
| _
    { raise (Failure "Unexpected character") }
