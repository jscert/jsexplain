let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, y) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (x, y) -> y

type comparison =
| Eq [@f]  (** Auto Generated Attributes **)
| Lt [@f]  (** Auto Generated Attributes **)
| Gt [@f]  (** Auto Generated Attributes **)

type coq_CompareSpecT =
| CompEqT [@f]  (** Auto Generated Attributes **)
| CompLtT [@f]  (** Auto Generated Attributes **)
| CompGtT [@f]  (** Auto Generated Attributes **)

(** val coq_CompareSpec2Type : comparison -> coq_CompareSpecT **)

let coq_CompareSpec2Type c =
  (match c with
   | Eq -> (fun _ -> CompEqT)
   | Lt -> (fun _ -> CompLtT)
   | Gt -> (fun _ -> CompGtT)) __

type 'a coq_CompSpecT = coq_CompareSpecT

(** val coq_CompSpec2Type : 'a1 -> 'a1 -> comparison -> 'a1 coq_CompSpecT **)

let coq_CompSpec2Type x y c =
  coq_CompareSpec2Type c

