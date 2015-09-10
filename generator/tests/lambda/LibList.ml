open Datatypes

(** val fold_right : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let fold_right f =
  let rec fold_right0 f0 acc = function
  | Coq_nil -> acc
  | Coq_cons (x, l') -> f0 x (fold_right0 f0 acc l')
  in fold_right0 f

