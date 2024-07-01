open Core
open Lexing
open Lambda
open Parser

let parse_with_error lexbuf = Grammar.main Lexer.read lexbuf

let parse_and_print lexbuf =
  let value = parse_with_error lexbuf in
  print_string (V.to_string (V.of_debruijn (L.eval_eager (V.to_debruijn value))))

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  In_channel.close inx

let () =
  Command.basic_spec ~summary:"Parse and display Lambda calculus"
    Command.Spec.(empty +> anon ("filename" %: string))
    loop
  |> Command_unix.run
