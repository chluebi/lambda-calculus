open Base.Lambda.LambdaCalculus

let () =
  let tree1 = Var "x" in
  let _ = print_endline (print tree1) in
  let tree2 = App (Lamb ("x", Var "x"), Lamb ("x", Var "x")) in
  let _ = print_endline (print tree2) in
  let _ = print_endline (print (match eval_step_eager tree2 with | (f, _) -> f)) in
  let _ = print_endline (print (eval_eager tree2)) in
  let tree3 = App (Lamb ("x", Lamb ("y", App (Var "x", Var "y"))), Lamb ("x", Var "x")) in
  let _ = print_endline (print tree3) in
  let _ = print_endline (print (match eval_step_eager tree3 with | (f, _) -> f)) in
  let _ = print_endline (print (eval_eager tree3)) in
  let tree4 = App (tree3, Lamb ("z", Var "z")) in
  let _ = print_endline (print tree4) in
  let _ = print_endline (print (match eval_step_eager tree4 with | (f, _) -> f)) in
  let _ = print_endline (print (eval_eager tree4)) in
  let tree5 = App (Lamb ("x", App (Var "x", Var "x")), Lamb ("x", App (Var "x", Var "x"))) in
  let _ = print_endline (print tree5) in
  let _ = print_endline (print (match eval_step_eager tree5 with | (f, _) -> f)) in
  let tree6 = App (Lamb ("x", Var "x"), App (Lamb ("y", Var "y"), Lamb ("z", Var "z"))) in
  let _ = print_endline (print tree6) in
  let _ = print_endline (print (match eval_step_eager tree6 with | (f, _) -> f)) in
  let _ = print_endline (print (eval_eager tree6)) in
  let _ = print_endline (print (match eval_step_lazy tree6 with | (f, _) -> f)) in
  let _ = print_endline (print (eval_lazy tree6)) in
  print_endline ""
