type stack =
  | C [@f value, stack] of int * stack
  | N  [@f]

let is_empty s = s === N

let push x stack = C(x, stack)

let pop stack =
  match stack with
  | C (x, xs) -> x
  | N -> stuck "Empty list"
