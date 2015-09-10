open Datatypes
open LibAxioms
open Specif

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val classicT : sumbool **)

let classicT =
  let h = indefinite_description __ in
  (match h with
   | Coq_true -> (fun _ -> Coq_left)
   | Coq_false -> (fun _ -> Coq_right)) __

