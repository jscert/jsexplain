type stack =
  | C [@f value, stack] of int * stack
  | K [@f value, stack] of int * stack
  | N  [@f]
  | B  [@f]

let is_empty s = s === N

let push x stack = C(x, stack)

let pop stack = 
  match stack with
  | C (x, xs) -> x
  | K (x, xs) -> x
  | B -> stuck "Empty list"
  | N -> stuck "Empty list"
  
