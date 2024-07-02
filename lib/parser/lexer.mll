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
| '\\'
    { LAMBDA }
| "->"
    { ARROW }
| ')'
    { RPAREN }
| "let"
    { LET }
| "="
    { EQ }
| "in"
    { IN }
| ['a'-'z']+ as x
    { ID x }
| _
    { raise (Failure "Unexpected character") }

