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
| "type"
    { TYPE }
| '|'
    { GUARD }
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
| "match"
    { MATCH }
| ':'
    { COLON }
| "with"
    { WITH }
| "end"
    { END }
| "["
    { LBRACK }
| "]"
    { RBRACK }
| ";"
    { SEMICOLON }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { STAR }
| '/'
    { DIV }
| '^'
    { EXP }
| '&'
    { AND }
| '|'
    { OR }
| '~'
    { NOT }
| "<="
    { LEQ }
| ">="
    { GEQ }
| '<'
    { LT }
| '>'
    { GT }
| ['0'-'9']+ as x
    { INT (int_of_string x) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as x
    { ID x }
| _
    { raise (Failure "Unexpected character") }
