type 'a stack =
  | Nil
  | Cons [@f value, stack] of 'a * 'a stack

let is_empty s = s === Nil

let push x stack = Cons(x, stack)

let pop stack = 
  match stack with
  | Cons (x, xs) -> x
  | Nil -> stuck "Empty list"
  
