(** val plus : int -> int -> int **)

let rec plus = (+)

(** val nat_iter : int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let rec nat_iter n f x =
  if int_eq n 0 then x else f (nat_iter (n-1) f x)

