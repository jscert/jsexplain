open Stack

type expr =
  | Const [@f value] of int
  | Add [@f left, right] of expr * expr
  | Sub [@f left, right] of expr * expr
  | Mul [@f left, right] of expr * expr
  | Div [@f left, right] of expr * expr

let rec eval_ expr = match expr with
  | Const n -> n
  | Add (ls, rs) -> eval_ ls + eval_ rs
  | Sub (ls, rs) -> eval_ ls - eval_ rs
  | Mul (ls, rs) -> eval_ ls * eval_ rs
  | Div (ls, rs) -> eval_ ls / eval_ rs

let rec print_expr expr = match expr with
  | Const n -> to_string n
  | Add (ls, rs) -> "(" + (print_expr ls) + ")" + " + " + (print_expr rs)
  | Sub (ls, rs) -> "(" + (print_expr ls) + ")" + " - " + (print_expr rs)
  | Mul (ls, rs) -> "(" + (print_expr ls) + ")" + " * " + (print_expr rs)
  | Div (ls, rs) -> "(" + (print_expr ls) + ")" + " / " + (print_expr rs)

let f =
    let source = parse "((1972 / 29) / 2) + 8" in
    print ((print_expr source) + " = " + to_string (eval_ source))
