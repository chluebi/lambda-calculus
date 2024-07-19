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

  let int_to_v (i : int) : V.t =
    let rec helper i =
      match i with 0 -> V.VarV "x" | _ -> V.AppV (V.VarV "f", helper (i - 1))
    in
    V.LambV ("f", V.LambV ("x", helper i))

  let rec to_v (tree : t) : V.t =
    match tree with
    | LambF (args, body) ->
        List.fold_right (fun e acc -> V.LambV (e, acc)) args (to_v body)
    | AppF (f, args) ->
        List.fold_left (fun acc e -> V.AppV (acc, to_v e)) (to_v f) args
    | VarF id -> V.VarV id
    | LetinF (id, e, body) -> V.AppV (V.LambV (id, to_v body), to_v e)
    | IfThenElse (cond, if_b, else_b) ->
        V.AppV
          ( V.AppV (V.AppV (V.VarV "ifthenelse", to_v cond), to_v if_b),
            to_v else_b )
    | NumF n -> int_to_v n
    | BinopF (binop, op1, op2) ->
        V.AppV (V.AppV (V.VarV (binop_to_identifier binop), to_v op1), to_v op2)
    | UnopF (unop, op) -> V.AppV (V.VarV (unop_to_identifier unop), to_v op)
end
