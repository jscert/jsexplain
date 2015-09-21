open Stack

type expr =
  | Const [@f value] of int
  | Add [@f left, right] of expr * expr
  | Sub [@f left, right] of expr * expr
  | Mul [@f left, right] of expr * expr
  | Div [@f left, right] of expr * expr
  | Pop [@f stack] of sexpr
and sexpr =
  | Emp [@f]
  | Push [@f value, stack] of expr * sexpr

let rec eval_ expr = match expr with
  | Const n -> n
  | Add (ls, rs) -> eval_ ls + eval_ rs
  | Sub (ls, rs) -> eval_ ls - eval_ rs
  | Mul (ls, rs) -> eval_ ls * eval_ rs
  | Div (ls, rs) -> eval_ ls / eval_ rs
  | Pop s -> Stack.pop (evals s)
and evals sexpr = match sexpr with
  | Emp -> Stack.N
  | Push (v, s) -> Stack.push (eval_ v) (evals s)
