open Datatypes
open LibReflect

(** val nat_compare : nat -> nat -> bool **)

let rec nat_compare x y =
  match x with
  | O ->
    (match y with
     | O -> Coq_true
     | S n -> Coq_false)
  | S x' ->
    (match y with
     | O -> Coq_false
     | S y' -> nat_compare x' y')

(** val nat_comparable : nat coq_Comparable **)

let nat_comparable x y =
  nat_compare x y

