open Lambda

module Frontend = struct
  type binop =
    | IntOpPlus
    | IntOpMinus
    | IntOpMult
    | IntOpDiv
    | IntOpExp
    | CompOpLeq
    | CompOpGeq
    | CompOpLt
    | CompOpGt
    | CompOpEq
    | BoolOpAnd
    | BoolOpOr

  let binop_to_string (b : binop) : string =
    match b with
    | IntOpPlus -> "+"
    | IntOpMinus -> "-"
    | IntOpMult -> "*"
    | IntOpDiv -> "/"
    | IntOpExp -> " ^ "
    | CompOpLeq -> " <= "
    | CompOpGeq -> " >= "
    | CompOpLt -> " < "
    | CompOpGt -> " > "
    | CompOpEq -> " = "
    | BoolOpAnd -> "&"
    | BoolOpOr -> "|"

  let binop_to_identifier (b : binop) : string =
    match b with
    | IntOpPlus -> "add"
    | IntOpMinus -> "sub"
    | IntOpMult -> "mul"
    | IntOpDiv -> "div"
    | IntOpExp -> "exp"
    | CompOpLeq -> "leq"
    | CompOpGeq -> "geq"
    | CompOpLt -> "lt"
    | CompOpGt -> "gt"
    | CompOpEq -> "eq"
    | BoolOpAnd -> "and"
    | BoolOpOr -> "or"

  type unop = IntOpNeg | BoolOpNot

  let unop_to_string (u : unop) : string =
    match u with IntOpNeg -> "-" | BoolOpNot -> "~"

  let unop_to_identifier (u : unop) : string =
    match u with IntOpNeg -> "neg" | BoolOpNot -> "not"

  type t =
    | LambF of string list * t
    | AppF of t * t list
    | VarF of string
    | LetinF of string * t * t
    | IfThenElse of t * t * t
    | NumF of int
    | BinopF of binop * t * t
    | UnopF of unop * t
    | ListF of t list
    | TypeF of (string * (string * string list) list) * t
    | MatchF of (t * string * (string * string list * t) list)

  let rec to_string (tree : t) : string =
    match tree with
    | LambF (args, body) ->
        List.fold_left (fun acc e -> acc ^ e ^ " ") "(\\" args
        ^ "-> " ^ to_string body ^ ")"
    | AppF (f, args) ->
        "(" ^ to_string f
        ^ List.fold_left (fun acc e -> acc ^ " " ^ to_string e) "" args
        ^ ")"
    | VarF id -> id
    | LetinF (id, e, body) ->
        "let " ^ id ^ " = " ^ to_string e ^ " in " ^ to_string body
    | IfThenElse (cond, if_b, else_b) ->
        "if " ^ to_string cond ^ " then " ^ to_string if_b ^ " else "
        ^ to_string else_b
    | NumF n -> Int.to_string n
    | BinopF (binop, op1, op2) ->
        "(" ^ to_string op1 ^ " " ^ binop_to_string binop ^ " " ^ to_string op2
        ^ ")"
    | UnopF (unop, op) -> "(" ^ unop_to_string unop ^ to_string op ^ ")"
    | ListF xs ->
        List.fold_left (fun acc e -> acc ^ "; " ^ to_string e) "[" xs ^ "]"
    | TypeF ((name, constructors), body) ->
        let print_constructor (name, args) =
          List.fold_left (fun acc e -> acc ^ e ^ " ") ("| " ^ name ^ " ") args
        in
        List.fold_left
          (fun acc e -> acc ^ print_constructor e)
          ("type " ^ name ^ " = ")
          constructors
        ^ " in " ^ to_string body
    | MatchF (e, type_name, cases) ->
        let print_case (name, args, body) =
          List.fold_left (fun acc e -> acc ^ e ^ " ") (" | " ^ name ^ " ") args
          ^ " -> " ^ to_string body
        in
        List.fold_left
          (fun acc e -> acc ^ print_case e)
          ("match " ^ to_string e ^ ": " ^ type_name)
          cases
        ^ " end"

  let int_to_v (i : int) : V.t =
    let rec helper i =
      match i with 0 -> V.VarV "x" | _ -> V.AppV (V.VarV "f", helper (i - 1))
    in
    V.LambV ("f", V.LambV ("x", helper i))

  type type_context = (string * string list) list
  type context = (string * type_context) list

  let rec to_v (ctxt : context) (tree : t) : V.t =
    match tree with
    | LambF (args, body) ->
        List.fold_right (fun e acc -> V.LambV (e, acc)) args (to_v ctxt body)
    | AppF (f, args) ->
        List.fold_left
          (fun acc e -> V.AppV (acc, to_v ctxt e))
          (to_v ctxt f) args
    | VarF id -> V.VarV id
    | LetinF (id, e, body) -> V.AppV (V.LambV (id, to_v ctxt body), to_v ctxt e)
    | IfThenElse (cond, if_b, else_b) ->
        V.AppV
          ( V.AppV (V.AppV (V.VarV "ifthenelse", to_v ctxt cond), to_v ctxt if_b),
            to_v ctxt else_b )
    | NumF n -> int_to_v n
    | BinopF (binop, op1, op2) ->
        V.AppV
          ( V.AppV (V.VarV (binop_to_identifier binop), to_v ctxt op1),
            to_v ctxt op2 )
    | UnopF (unop, op) -> V.AppV (V.VarV (unop_to_identifier unop), to_v ctxt op)
    | ListF xs ->
        List.fold_right
          (fun e acc -> V.AppV (V.AppV (V.VarV "cons", to_v ctxt e), acc))
          xs (V.VarV "nil")
    | TypeF ((name, constructors), body) ->
        let type_index = List.length ctxt in
        let rec constructors_to_v (constructors : (string * string list) list)
            (type_ctxt : type_context) : V.t =
          match constructors with
          | [] ->
              let new_ctxt = List.append ctxt [ (name, type_ctxt) ] in
              to_v new_ctxt body
          | constructor :: cs ->
              let constructor_index = List.length type_ctxt in
              let name, args = constructor in
              let new_type_ctxt = List.append type_ctxt [ (name, args) ] in
              let next_v = constructors_to_v cs new_type_ctxt in
              let constructor_f =
                to_v ctxt
                  (LambF
                     ( List.mapi (fun i _ -> Int.to_string i) args,
                       ListF
                         [
                           NumF type_index;
                           NumF constructor_index;
                           ListF
                             (List.mapi
                                (fun i _ -> VarF (Int.to_string i))
                                args);
                         ] ))
              in
              V.AppV (V.LambV (name, next_v), constructor_f)
        in
        constructors_to_v constructors []
    | MatchF (e, type_name, cases) ->
        let type_ctxt : type_context =
          match List.assoc_opt type_name ctxt with
          | None -> failwith ("could not find type " ^ type_name)
          | Some x -> x
        in
        let rec cases_to_v (cases : (string * string list * t) list)
            (remaining_constructors : type_context) : V.t =
          match cases with
          | [] -> failwith "empty cases"
          | (name, args, body) :: cs -> (
              let remaining_constructors, case_f, constructor_index =
                if name = "_" then ([], body, -1)
                else
                  let constructor_name =
                    match List.assoc_opt name remaining_constructors with
                    | None -> (
                        match List.assoc_opt name type_ctxt with
                        | None -> failwith ("constructor " ^ name ^ " not found")
                        | Some _ ->
                            failwith
                              ("redundant case for constructor " ^ name
                             ^ " in match statement"))
                    | Some constructor_args -> (
                        match
                          List.length constructor_args = List.length args
                        with
                        | false ->
                            failwith
                              ("constructor " ^ name ^ " has "
                              ^ Int.to_string (List.length constructor_args)
                              ^ " args, but "
                              ^ Int.to_string (List.length args)
                              ^ " were given")
                        | true -> name)
                  in
                  let constructor_index =
                    match
                      List.find_index (fun (x, _) -> x = name) type_ctxt
                    with
                    | Some i -> i
                    | None ->
                        failwith
                          ("Somehow failed to find constructor " ^ name
                         ^ " when finding index")
                  in
                  let args_v = AppF (VarF "nth", [ NumF 2; NumF 0; e ]) in
                  let case_f =
                    AppF
                      ( LambF (args, body),
                        List.mapi
                          (fun i _ ->
                            AppF (VarF "nth", [ NumF i; NumF 0; args_v ]))
                          args )
                  in
                  let remaining_constructors =
                    List.remove_assoc constructor_name remaining_constructors
                  in
                  (remaining_constructors, case_f, constructor_index)
              in
              match cs with
              | [] ->
                  let _ =
                    match remaining_constructors with
                    | [] -> ()
                    | (x, _) :: _ ->
                        failwith
                          ("in match statement for " ^ type_name
                         ^ " case not covered: " ^ x)
                  in
                  to_v ctxt case_f
              | cs ->
                  let next_v = cases_to_v cs remaining_constructors in
                  let _ =
                    match name with
                    | "_" -> failwith "empty placeholder far too late noticed!!"
                    | _ -> ()
                  in
                  let type_index_v = AppF (VarF "nth", [ NumF 1; NumF 0; e ]) in
                  V.AppV
                    ( V.AppV
                        ( V.AppV
                            ( V.VarV "ifthenelse",
                              to_v ctxt
                                (BinopF
                                   ( CompOpEq,
                                     type_index_v,
                                     NumF constructor_index )) ),
                          to_v ctxt case_f ),
                      next_v ))
        in
        cases_to_v cases type_ctxt

  let to_v = to_v []
end
