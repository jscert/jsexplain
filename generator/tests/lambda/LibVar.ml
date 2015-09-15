open Datatypes
open LibNat

type var = nat

(** val var_comp : var coq_Comparable **)

let var_comp x y =
  nat_compare x y

(** val var_comparable : var coq_Comparable **)

let var_comparable =
  var_comp

