module LambdaCalculus = struct
  type var = string
  type t = Lamb of var * t | App of t * t | Var of var

  let rec lambda_fold (lamb_f : (var * 'a) -> 'a) (app_f : ('a * 'a) -> 'a) (var_f : var -> 'a) (tree : t) : 'a =
    match tree with
      | Lamb (v, b) -> lamb_f (v, (lambda_fold lamb_f app_f var_f b))
      | App (f, a) -> app_f (lambda_fold lamb_f app_f var_f f, lambda_fold lamb_f app_f var_f a)  
      | Var v -> var_f v

  let print (tree : t) : string =
    lambda_fold 
      (fun (var, t) -> "(\\" ^ var ^ " => " ^ t ^ ")")
      (fun (f, a) -> f ^ "" ^ a)
      (fun v -> v) 
      tree

  let rec sub (sub_v: var) (sub_expr: t) (tree:  t) : t =
    match tree with
      | Lamb (v, b) -> if sub_v = v then Lamb (v, b) else Lamb (v, sub sub_v sub_expr b)
      | App (f, a) -> App ((sub sub_v sub_expr f), (sub sub_v sub_expr a))
      | Var v -> if sub_v = v then sub_expr else Var v

end


module Examples = struct

  open LambdaCalculus

  let example1 : t = Var "x"
  let example2 : t = App (Lamb ("x", Var "x"), Lamb ("x", Var "x"))

end