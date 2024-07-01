open Lambda

let trees =
  [
    V.LambV ("x", V.VarV "x");
    V.AppV (V.LambV ("x", V.VarV "x"), V.LambV ("x", V.VarV "x"));
    V.AppV
      ( V.LambV ("x", V.LambV ("y", V.AppV (V.VarV "x", V.VarV "y"))),
        V.LambV ("x", V.VarV "x") );
    V.AppV
      ( V.AppV
          ( V.LambV ("x", V.LambV ("y", V.AppV (V.VarV "x", V.VarV "y"))),
            V.LambV ("x", V.VarV "x") ),
        V.LambV ("z", V.VarV "z") );
    V.AppV
      ( V.LambV ("x", V.AppV (V.VarV "x", V.VarV "x")),
        V.LambV ("x", V.AppV (V.VarV "x", V.VarV "x")) );
    V.AppV
      ( V.LambV ("x", V.VarV "x"),
        V.AppV (V.LambV ("y", V.VarV "y"), V.LambV ("z", V.VarV "z")) );
  ]

let () =
  let _ = List.iter (fun x -> print_endline (V.to_string x)) trees in
  let _ =
    List.iter (fun x -> print_endline (L.to_string (V.to_debruijn x))) trees
  in
  let _ =
    List.iter
      (fun x -> print_endline (V.to_string (V.of_debruijn (V.to_debruijn x))))
      trees
  in
  let _ =
    List.iter
      (fun x ->
        print_endline
          (L.to_string
             (match L.eval_step_eager (V.to_debruijn x) with t, _ -> t)))
      trees
  in
  let _ =
    List.iter
      (fun x ->
        assert (
          V.to_debruijn x = V.to_debruijn (V.of_debruijn (V.to_debruijn x))))
      trees
  in
  print_endline ""
