open Core
open Lambda
open Frontend
open Parser

let parse_with_error lexbuf = Grammar.main Lexer.read lexbuf

let parse_and_print lexbuf =
  let value = parse_with_error lexbuf in
  print_endline (Frontend.to_string value);
  let v = Frontend.to_v value in
  print_endline (V.to_string v);
  let debruijn = V.to_debruijn v in
  print_endline (L.to_string debruijn);
  print_string
    (V.to_string
       (V.of_debruijn (FullReduction.full_reduction (debruijn))))

let read_files filenames =
  List.map filenames ~f:In_channel.read_all |> String.concat ~sep:" "

let loop filenames () =
  let content = read_files filenames in
  let lexbuf = Lexing.from_string content in
  parse_and_print lexbuf

let () =
  Command.basic_spec ~summary:"Parse and display Lambda calculus"
    Command.Spec.(empty +> anon (sequence ("filename" %: string)))
    loop
  |> Command_unix.run
