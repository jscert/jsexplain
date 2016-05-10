open Datatypes
open LibNat

type var = nat

(** val var_comp : var coq_Comparable **)

let var_comp x y =
  nat_compare x y

