module LambdaCalculus = struct
  type var = string
  type t = Lamb of var * t | App of t * t | Var of var

  let rec lambda_fold (lamb_f : var * 'a -> 'a) (app_f : 'a * 'a -> 'a)
      (var_f : var -> 'a) (tree : t) : 'a =
    match tree with
    | Lamb (v, b) -> lamb_f (v, lambda_fold lamb_f app_f var_f b)
    | App (f, a) ->
        app_f
          (lambda_fold lamb_f app_f var_f f, lambda_fold lamb_f app_f var_f a)
    | Var v -> var_f v

  let print (tree : t) : string =
    lambda_fold
      (fun (var, t) -> "(\\" ^ var ^ " => " ^ t ^ ")")
      (fun (f, a) -> "(" ^ f ^ " " ^ a ^ ")")
      (fun v -> v)
      tree

  let rec sub (sub_v : var) (sub_expr : t) (tree : t) : t =
    match tree with
    | Lamb (v, b) ->
        if sub_v = v then Lamb (v, b) else Lamb (v, sub sub_v sub_expr b)
    | App (f, a) -> App (sub sub_v sub_expr f, sub sub_v sub_expr a)
    | Var v -> if sub_v = v then sub_expr else Var v

  let rec eval_step_eager (tree : t) : t * bool =
    match tree with
      | Lamb _ -> tree, false
      | App (f, a) -> (
          match eval_step_eager f with
            | (f, false) -> (
              match eval_step_eager a with
                | (a, false) -> (
                  match f with
                    | Lamb (v, b) -> sub v a b, true
                    | _ ->  raise (Failure "Only lambdas can take arguments")
                )
                | (a, true) -> App (f, a), true
              )
            | (f, true) -> App (f, a), true
      )
      | Var _ -> tree, false

  let rec eval_eager (tree: t) : t =
    match eval_step_eager tree with
      | (tree', true) -> eval_eager tree'
      | (tree', false) -> tree'


  let rec eval_step_lazy (tree : t) : t * bool =
    match tree with
      | Lamb _ -> tree, false
      | App (f, a) -> (
          match eval_step_lazy f with
            | (f, false) -> (
              match f with
                | Lamb (v, b) -> sub v a b, true
                | _ ->  raise (Failure "Only lambdas can take arguments")
              )
            | (f, true) -> App (f, a), true
      )
      | Var _ -> tree, false

  let rec eval_lazy (tree: t) : t =
    match eval_step_lazy tree with
      | (tree', true) -> eval_lazy tree'
      | (tree', false) -> tree'
end

module Examples = struct
  open LambdaCalculus

  let example1 : t = Var "x"
  let example2 : t = App (Lamb ("x", Var "x"), Lamb ("x", Var "x"))
end
