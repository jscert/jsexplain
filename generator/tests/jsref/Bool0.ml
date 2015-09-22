let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val eqb : bool -> bool -> bool **)

let eqb b1 b2 =
  if b1 then if b2 then true else false else if b2 then false else true

type reflect =
| ReflectT [@f]  (** Auto Generated Attributes **)
| ReflectF [@f]  (** Auto Generated Attributes **)

(** val iff_reflect : bool -> reflect **)

let iff_reflect b =
  (if b then (fun _ -> ReflectT) else (fun _ -> ReflectF)) __

