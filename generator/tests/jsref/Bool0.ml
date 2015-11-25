(** val eqb : bool -> bool -> bool **)

let eqb b1 b2 =
  if b1 then if b2 then true else false else if b2 then false else true
