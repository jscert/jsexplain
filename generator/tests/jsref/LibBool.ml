(** val coq_and : bool -> bool -> bool **)

let coq_and x y =
  if x then if y then true else false else false

(** val coq_or : bool -> bool -> bool **)

let coq_or x y =
  if x then true else if y then true else false

(** val neg : bool -> bool **)

let neg b = if b then false else true

