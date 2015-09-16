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

let rec print_expr expr = match expr with
  | Const n -> to_string n
  | Add (ls, rs) -> "(" + (print_expr ls) + ")" + " + " + (print_expr rs)
  | Sub (ls, rs) -> "(" + (print_expr ls) + ")" + " - " + (print_expr rs)
  | Mul (ls, rs) -> "(" + (print_expr ls) + ")" + " * " + (print_expr rs)
  | Div (ls, rs) -> "(" + (print_expr ls) + ")" + " / " + (print_expr rs)
  | Pop s -> "Pop(" + print_sexpr s + ")"
and print_sexpr sexpr = match sexpr with
  | Emp -> "Emp"
  | Push (v, s) -> "Push(" + (print_expr v) + ", " + (print_sexpr s) + ")"


let f =
    (*let bli = Stack.C(1, Stack.N) in
    let blii = (Stack.push 2 bli) in*)
    let source = parse "Pop(Push(1, Push(5, Emp)) - 7" in
    print ((print_expr source) + " = " + to_string (eval_ source))
