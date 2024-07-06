open Core
open Lambda
open Parser

let parse_with_error lexbuf = Grammar.main Lexer.read lexbuf

let parse_and_print lexbuf =
  let value = parse_with_error lexbuf in
  print_string
    (V.to_string
       (V.of_debruijn (FullReduction.full_reduction (V.to_debruijn value))))

let read_files filenames =
  List.map filenames ~f:In_channel.read_all |> String.concat

let loop filenames () =
  let content = read_files filenames in
  let lexbuf = Lexing.from_string content in
  parse_and_print lexbuf

let () =
  Command.basic_spec ~summary:"Parse and display Lambda calculus"
    Command.Spec.(empty +> anon (sequence ("filename" %: string)))
    loop
  |> Command_unix.run