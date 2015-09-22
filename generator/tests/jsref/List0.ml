(** val hd : 'a1 -> 'a1 list -> 'a1 **)

let hd default = function
| [] -> default
| x :: l0 -> x

(** val tl : 'a1 list -> 'a1 list **)

let tl = function
| [] -> []
| a :: m -> m

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let map f =
  let rec map0 = function
  | [] -> []
  | a :: t -> (f a) :: (map0 t)
  in map0

