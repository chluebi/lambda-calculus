{
open Grammar
}

rule read = parse
| ['\t']
    { read lexbuf }
| '\n'
    { EOL }
| '('
    { LPAREN }
| '\\'
    { LAMBDA }
| " -> "
    { ARROW }
| ')'
    { RPAREN }
| ' '
    { APP }
| ['a'-'z']+ as x
    { ID x }
| _
    { raise (Failure "Unexpected character") }

