(** val fold_right : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let rec fold_right f acc l = match l with
  | [] -> acc
  | x :: l' -> f x (fold_right f acc l')

