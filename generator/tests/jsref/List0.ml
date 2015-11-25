(** val hd : 'a1 -> 'a1 list -> 'a1 **)

let hd default l = match l with
| [] -> default
| x :: l0 -> x

(** val tl : 'a1 list -> 'a1 list **)

let tl l = match l with
| [] -> []
| a :: m -> m

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f l = match l with
  | [] -> []
  | a :: t -> (f a) :: (map f t)

