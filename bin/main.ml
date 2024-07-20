open Core
open Lambda
open Frontend
open Parser

let parse_with_error lexbuf = Grammar.main Lexer.read lexbuf

let parse_and_print lexbuf =
  let value = parse_with_error lexbuf in
  let v = Frontend.to_v value in
  let debruijn = V.to_debruijn v in
  print_string (L.to_type_string debruijn)

let parse_and_lazy_evaluate lexbuf =
  let value = parse_with_error lexbuf in
  let v = Frontend.to_v value in
  let debruijn = V.to_debruijn v in
  print_string (L.to_type_string (L.eval_lazy debruijn))

let parse_and_full_reduction lexbuf =
  let value = parse_with_error lexbuf in
  print_endline (Frontend.to_string value);
  let v = Frontend.to_v value in
  let debruijn = V.to_debruijn v in
  print_string
    (L.to_type_string
       (FullReduction.full_reduction_steps (L.eval_lazy debruijn) 10000))

let read_files filenames =
  List.map filenames ~f:In_channel.read_all |> String.concat ~sep:" "

let loop just_print just_lazy filenames () =
  let content = read_files filenames in
  let lexbuf = Lexing.from_string content in
  if just_print then parse_and_print lexbuf
  else if just_lazy then parse_and_lazy_evaluate lexbuf
  else parse_and_full_reduction lexbuf

let () =
  Command.basic_spec ~summary:"Parse and display Lambda calculus"
    Command.Spec.(
      empty
      +> flag "-just-print" no_arg
           ~doc:" Just print (no evaluate, no full reduction)"
      +> flag "-just-lazy" no_arg ~doc:" Just lazy evaluate (no full reduction)"
      +> anon (sequence ("filename" %: string)))
    (fun just_print just_lazy filenames () ->
      loop just_print just_lazy filenames ())
  |> Command_unix.run
