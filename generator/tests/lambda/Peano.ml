open Datatypes

(** val plus : nat -> nat -> nat **)

let rec plus n m =
  match n with
  | O -> m
  | S p -> S (plus p m)

