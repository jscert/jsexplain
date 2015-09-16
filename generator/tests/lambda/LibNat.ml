open Datatypes

(** val nat_compare : nat -> nat -> bool **)

let rec nat_compare x y =
  match x with
  | O ->
    (match y with
     | O -> true
     | S n -> false)
  | S x' ->
    (match y with
     | O -> false
     | S y' -> nat_compare x' y')

