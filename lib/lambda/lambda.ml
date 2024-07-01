module L = struct
  type t = Lamb of t | App of t * t | Var of int

  let rec lambda_fold (lamb_f : 'a -> 'a) (app_f : 'a * 'a -> 'a)
      (var_f : int -> 'a) (tree : t) : 'a =
    match tree with
    | Lamb b -> lamb_f (lambda_fold lamb_f app_f var_f b)
    | App (f, a) ->
        app_f
          (lambda_fold lamb_f app_f var_f f, lambda_fold lamb_f app_f var_f a)
    | Var i -> var_f i

  type context = int

  let lambda_fold_with_context (lamb_f : context -> 'a -> 'a)
      (app_f : context -> 'a * 'a -> 'a) (var_f : context -> int -> 'a)
      (tree : t) : 'a =
    let rec helper (ctxt : context) (tree : t) : 'a =
      match tree with
      | Lamb b -> lamb_f ctxt (helper (ctxt + 1) b)
      | App (f, a) -> app_f ctxt (helper ctxt f, helper ctxt a)
      | Var i -> var_f ctxt i
    in
    helper 0 tree

  let to_string (tree : t) : string =
    lambda_fold_with_context
      (fun c b -> "(\\" ^ Int.to_string c ^ " => " ^ b ^ ")")
      (fun _ (f, a) -> "(" ^ f ^ " " ^ a ^ ")")
      (fun _ v -> Int.to_string v)
      tree

  let change_indicies (by : int) (tree : t) : t =
    lambda_fold_with_context
      (fun _ b -> Lamb b)
      (fun _ (f, a) -> App (f, a))
      (fun _ i -> Var (i + by))
      tree

  let sub (sub_index : int) (sub_expr : t) (tree : t) : t =
    lambda_fold_with_context
      (fun _ b -> Lamb b)
      (fun _ (f, a) -> App (f, a))
      (fun ctxt i ->
        if sub_index = i then change_indicies (ctxt + 1) sub_expr else Var i)
      tree

  (*
     let rec sub (sub_v : var) (sub_expr : t) (tree : t) : t =
       match tree with
       | Lamb (v, b) ->
           if sub_v = v then Lamb (v, b) else Lamb (v, sub sub_v sub_expr b)
       | App (f, a) -> App (sub sub_v sub_expr f, sub sub_v sub_expr a)
       | Var v -> if sub_v = v then sub_expr else Var v
  *)

  let rec eval_step_eager (tree : t) : t * bool =
    match tree with
    | Lamb _ -> (tree, false)
    | App (f, a) -> (
        match eval_step_eager f with
        | f, false -> (
            match eval_step_eager a with
            | a, false -> (
                match f with
                | Lamb b -> (change_indicies (-1) (sub 0 a b), true)
                | _ -> raise (Failure "Only lambdas can take arguments"))
            | a, true -> (App (f, a), true))
        | f, true -> (App (f, a), true))
    | Var _ -> (tree, false)

  let rec eval_eager (tree : t) : t =
    match eval_step_eager tree with
    | tree', true -> eval_eager tree'
    | tree', false -> tree'

  let rec eval_step_lazy (tree : t) : t * bool =
    match tree with
    | Lamb _ -> (tree, false)
    | App (f, a) -> (
        match eval_step_lazy f with
        | f, false -> (
            match f with
            | Lamb b -> (change_indicies (-1) (sub 0 a b), true)
            | _ -> raise (Failure "Only lambdas can take arguments"))
        | f, true -> (App (f, a), true))
    | Var _ -> (tree, false)

  let rec eval_lazy (tree : t) : t =
    match eval_step_lazy tree with
    | tree', true -> eval_lazy tree'
    | tree', false -> tree'
end

module V = struct
  type t = LambV of string * t | AppV of t * t | VarV of string

  let rec int_to_string n : string =
    if n < 0 then failwith "Input must be between >= 0"
    else if n > 26 then int_to_string (n / 26) ^ int_to_string (n mod 26)
    else String.make 1 (Char.chr (n + 97))

  let of_debruijn (tree : L.t) : t =
    L.lambda_fold_with_context
      (fun c b -> LambV (int_to_string c, b))
      (fun _ (a, f) -> AppV (a, f))
      (fun _ i -> VarV (int_to_string i))
      tree

  let rec visual_fold (lamb_f : string * 'a -> 'a) (app_f : 'a * 'a -> 'a)
      (var_f : string -> 'a) (tree : t) : 'a =
    match tree with
    | LambV (v, b) -> lamb_f (v, visual_fold lamb_f app_f var_f b)
    | AppV (f, a) ->
        app_f
          (visual_fold lamb_f app_f var_f f, visual_fold lamb_f app_f var_f a)
    | VarV v -> var_f v

  let to_string (tree : t) : string =
    visual_fold
      (fun (v, b) -> "(\\" ^ v ^ " => " ^ b ^ ")")
      (fun (f, a) -> "(" ^ f ^ " " ^ a ^ ")")
      (fun v -> v)
      tree

  module StringMap = Map.Make (String)

  type context = int * int StringMap.t

  let visual_fold_with_context (lamb_f : context -> string * 'a -> 'a)
      (app_f : context -> 'a * 'a -> 'a) (var_f : context -> string -> 'a)
      (tree : t) : 'a =
    let rec helper ((level, map) : context) (tree : t) : 'a =
      match tree with
      | LambV (v, b) ->
          lamb_f (level, map)
            (v, helper (level + 1, StringMap.add v level map) b)
      | AppV (f, a) ->
          app_f (level, map) (helper (level, map) f, helper (level, map) a)
      | VarV v -> var_f (level, map) v
    in
    helper (0, StringMap.empty) tree

  let to_debruijn (tree : t) : L.t =
    visual_fold_with_context
      (fun _ (_, b) -> L.Lamb b)
      (fun _ (f, a) -> L.App (f, a))
      (fun (_, map) v -> L.Var (StringMap.find v map))
      tree
end

module Examples = struct
  open V

  let example1 : t = LambV ("x", VarV "x")
  let example2 : t = AppV (LambV ("x", VarV "x"), LambV ("x", VarV "x"))
end
