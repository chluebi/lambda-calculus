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
    print_endline ("eval_eager with " ^ to_string tree);
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

module FullReduction = struct
  type b = VarB of int | LambB of b | AppB of b * b | FreeAppB of int * v list
  and v = LambV of b | FreeAppV of int * v list

  type context = int

  let b_to_v (tree : b) : v =
    match tree with
    | LambB b -> LambV b
    | FreeAppB (i, l) -> FreeAppV (i, l)
    | _ -> failwith "not supported"

  let v_to_b (tree : v) : b =
    match tree with LambV b -> LambB b | FreeAppV (i, l) -> FreeAppB (i, l)

  let pure_to_b (tree : L.t) : b =
    L.lambda_fold_with_context
      (fun _ b -> LambB b)
      (fun _ (a, f) -> AppB (a, f))
      (fun _ i -> VarB i)
      tree

  let rec b_to_pure (tree : b) : L.t =
    match tree with
    | LambB b -> L.Lamb (b_to_pure b)
    | AppB (f, a) -> L.App (b_to_pure f, b_to_pure a)
    | VarB i -> L.Var i
    | _ -> failwith "not supported"

  let b_fold_with_context (lamb_f : context -> 'a -> 'a)
      (app_f : context -> 'a * 'a -> 'a) (var_f : context -> int -> 'a)
      (free_app_f : context -> int * v list -> 'a) (tree : b) : 'a =
    let rec helper (ctxt : context) (tree : b) : 'a =
      match tree with
      | LambB b -> lamb_f ctxt (helper (ctxt + 1) b)
      | AppB (f, a) -> app_f ctxt (helper ctxt f, helper ctxt a)
      | VarB i -> var_f ctxt i
      | FreeAppB (i, l) -> free_app_f ctxt (i, l)
    in
    helper 0 tree

  let rec b_to_string (tree : b) : string =
    b_fold_with_context
      (fun c b -> "(\\" ^ Int.to_string c ^ " => " ^ b ^ ")")
      (fun _ (f, a) -> "(" ^ f ^ " " ^ a ^ ")")
      (fun _ v -> Int.to_string v)
      (fun _ (i, l) ->
        "[" ^ Int.to_string i ^ " "
        ^ String.concat " " (List.map (fun x -> b_to_string (v_to_b x)) l)
        ^ "]")
      tree

  let v_to_string x = b_to_string (v_to_b x)

  let change_indicies (by : int) (tree : b) : b =
    b_fold_with_context
      (fun _ b -> LambB b)
      (fun _ (f, a) -> AppB (f, a))
      (fun _ i -> VarB (i + by))
      (fun _ (i, l) -> FreeAppB (i, l))
      tree

  let sub (sub_index : int) (sub_expr : b) (tree : b) : b =
    b_fold_with_context
      (fun _ b -> LambB b)
      (fun _ (f, a) -> AppB (f, a))
      (fun ctxt i ->
        if sub_index = i then change_indicies (ctxt + 1) sub_expr else VarB i)
      (fun _ (i, l) -> FreeAppB (i, l))
      tree

  let rec eval_step_eager (tree : b) : b * bool =
    match tree with
    | LambB _ -> (tree, false)
    | AppB (f, a) -> (
        match eval_step_eager f with
        | f, false -> (
            match eval_step_eager a with
            | a, false -> (
                match f with
                | LambB b -> (change_indicies (-1) (sub 0 a b), true)
                | FreeAppB (i, l) -> (
                    match a with
                    | LambB b -> (FreeAppB (i, List.append [ LambV b ] l), true)
                    | FreeAppB (i2, l2) ->
                        (FreeAppB (i, List.append [ FreeAppV (i2, l2) ] l), true)
                    | _ -> failwith "this should never happen")
                | _ ->
                    raise
                      (Failure
                         "Only lambdas or symbolic variables can take arguments")
                )
            | a, true -> (AppB (f, a), true))
        | f, true -> (AppB (f, a), true))
    | VarB _ -> (tree, false)
    | FreeAppB _ -> (tree, false)

  let rec eval_eager (tree : b) : b =
    print_endline ("eval_eager with " ^ b_to_string tree);
    match eval_step_eager tree with
    | tree', true -> eval_eager tree'
    | tree', false -> tree'

  let rec min_debruijn (tree : b) : int =
    Int.min
      (b_fold_with_context
         (fun _ b -> b)
         (fun _ (f, a) -> Int.min f a)
         (fun _ v -> v)
         (fun _ (i, l) ->
           List.fold_left Int.min i
             (List.map (fun x -> min_debruijn (v_to_b x)) l))
         tree)
      0

  let full_reduction (tree : L.t) : L.t =
    let eval (tree : b) : v = b_to_v (eval_eager tree) in
    let rec norm (tree : b) : b =
      let x = readback (eval tree) in
      print_endline ("readback finished with " ^ b_to_string x);
      x
    and readback (tree : v) : b =
      print_endline ("readback of " ^ v_to_string tree);
      match tree with
      | LambV b ->
          LambB
            (change_indicies 1
               (norm (AppB (LambB b, FreeAppB (min_debruijn b - 1, [])))))
      | FreeAppV (i, l) ->
          let rec helper (l : v list) (carry : b) : b =
            match l with
            | [] -> carry
            | x :: xs -> helper xs (AppB (carry, readback x))
          in
          helper l (VarB i)
    in
    b_to_pure (norm (pure_to_b tree))
end