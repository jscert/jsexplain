(** val negb : bool -> bool **)

let negb b = if b then false else true

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst p = match p with
| Pair (x, y) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd p = match p with
| Pair (x, y) -> y

type comparison =
| Eq [@f]  (** Auto Generated Attributes **)
| Lt [@f]  (** Auto Generated Attributes **)
| Gt [@f]  (** Auto Generated Attributes **)

