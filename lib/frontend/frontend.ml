open Lambda

module Frontend = struct

  type int_binop =
    | IntOpPlus
    | IntOpMinus
    | IntOpMult
    | IntOpExp

  let int_binop_to_string (b: int_binop) : string =
    match b with
      | IntOpPlus -> "+"
      | IntOpMinus -> "-"
      | IntOpMult -> "*"
      | IntOpExp -> " ^ "

  let int_binop_to_identifier (b: int_binop) : string =
    match b with
      | IntOpPlus -> "add"
      | IntOpMinus -> "sub"
      | IntOpMult -> "mul"
      | IntOpExp -> "exp"

  type int_unop =
    | IntOpNeg

  let int_unop_to_string (u: int_unop) : string =
    match u with
      | IntOpNeg -> "-"

  let int_unop_to_identifier (u: int_unop) : string =
    match u with
      | IntOpNeg -> "neg"


  type bool_binop =
    | BoolOpAnd
    | BoolOpOr

  let bool_binop_to_string (b: bool_binop) : string =
    match b with
      | BoolOpAnd -> "&"
      | BoolOpOr -> "|"

  let bool_binop_to_identifier (b: bool_binop) : string =
    match b with
      | BoolOpAnd -> "and"
      | BoolOpOr -> "or"


  type bool_unop =
    | BoolOpNot

  let bool_unop_to_string (u: bool_unop) : string =
    match u with
      | BoolOpNot -> "~"

  let bool_unop_to_identifier (u: bool_unop) : string =
    match u with
      | BoolOpNot -> "not"
  


  type t = 
    | LambF of (string list) * t 
    | AppF of t * (t list)
    | VarF of string
    | LetinF of string * t * t
    | IfThenElse of t * t * t
    | NumF of int
    | IntBinopF of int_binop * t * t
    | IntUnopF of int_unop * t
    | BoolBinopF of bool_binop * t * t
    | BoolUnopF of bool_unop * t


  let rec to_string (tree: t) : string =
    match tree with
      | LambF (args, body) -> (List.fold_left (fun acc e -> acc ^ " " ^ e) "\\" args) ^ " -> " ^ (to_string body) ^ ")"
      | AppF (f, args) -> (to_string f) ^ " " ^ (List.fold_left (fun acc e -> acc ^ " " ^ (to_string e)) "" args)
      | VarF id -> id
      | LetinF (id, e, body) -> "let " ^ id ^ " = " ^ (to_string e) ^ " in " ^ (to_string body)
      | IfThenElse (cond, if_b, else_b) -> "if " ^ (to_string cond) ^ " then " ^ (to_string if_b) ^ " else " ^ (to_string else_b)
      | NumF n -> Int.to_string n
      | IntBinopF (binop, op1, op2) -> (to_string op1) ^ " " ^ (int_binop_to_string binop) ^ " " ^ (to_string op2)
      | IntUnopF (unop, op) -> (int_unop_to_string unop) ^ (to_string op)
      | BoolBinopF (binop, op1, op2) -> (to_string op1) ^ " " ^ (bool_binop_to_string binop) ^ " " ^ (to_string op2)
      | BoolUnopF (unop, op) -> (bool_unop_to_string unop) ^ (to_string op)

  
  let int_to_v (i: int) : V.t =
    let rec helper i = 
      match i with
        | 0 -> V.VarV "x"
        | _ -> V.AppV (V.VarV "f", helper (i-1))
    in
    V.LambV ("f", V.LambV ("x", helper i))

  
  let rec to_v (tree: t) : V.t =
      match tree with 
        | LambF (args, body) -> List.fold_right (fun e acc -> V.LambV (e, acc)) args (to_v body)
        | AppF (f, args) -> List.fold_left (fun acc e -> V.AppV (acc, to_v e)) (to_v f) args
        | VarF id -> V.VarV id
        | LetinF (id, e, body) -> V.AppV (V.LambV (id, to_v body), to_v e)
        | IfThenElse (cond, if_b, else_b) -> V.AppV (V.AppV (V.AppV (V.VarV "ifthenelse", to_v cond), to_v if_b), to_v else_b)
        | NumF n -> int_to_v n
        | IntBinopF (binop, op1, op2) -> V.AppV (V.AppV (V.VarV (int_binop_to_identifier binop), to_v op1), to_v op2)
        | IntUnopF (unop, op) -> V.AppV (V.VarV (int_unop_to_identifier unop), to_v op)
        | BoolBinopF (binop, op1, op2) -> V.AppV (V.AppV (V.VarV (bool_binop_to_identifier binop), to_v op1), to_v op2)
        | BoolUnopF (unop, op) -> V.AppV (V.VarV (bool_unop_to_identifier unop), to_v op)

end