open LibAxioms
open LibLogic
open Specif

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val coq_Inhab_witness : __ -> 'a1 **)

let coq_Inhab_witness _ =
  indefinite_description __

(** val epsilon_def : __ -> 'a1 **)

let epsilon_def _ =
  match classicT with
  | Coq_left -> indefinite_description __
  | Coq_right -> coq_Inhab_witness __

(** val epsilon : __ -> 'a1 **)

let epsilon _ =
  epsilon_def __

