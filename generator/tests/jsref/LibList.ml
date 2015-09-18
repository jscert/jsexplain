open LibOperation
open LibReflect
open Peano

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val list_eq_nil_decidable : 'a1 list -> coq_Decidable **)

let list_eq_nil_decidable = function
| [] -> true
| a :: l0 -> false

(** val fold_right : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let fold_right f =
  let rec fold_right0 f0 acc = function
  | [] -> acc
  | x :: l' -> f0 x (fold_right0 f0 acc l')
  in fold_right0 f

(** val fold_left : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let fold_left f =
  let rec fold_left0 f0 acc = function
  | [] -> acc
  | x :: l' -> fold_left0 f0 (f0 x acc) l'
  in fold_left0 f

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let map f =
  let () = () in fold_right (fun x acc -> (f x) :: acc) []

(** val filter : 'a1 predb -> 'a1 list -> 'a1 list **)

let filter f =
  let () = () in fold_right (fun x acc -> if f x then x :: acc else acc) []

(** val append : 'a1 list -> 'a1 list -> 'a1 list **)

let append l1 l2 =
  let () = () in fold_right (fun x acc -> x :: acc) l2 l1

(** val concat : 'a1 list list -> 'a1 list **)

let concat l =
  (let () = () in fold_right append []) l

(** val rev : 'a1 list -> 'a1 list **)

let rev l =
  (let () = () in fold_left (fun x acc -> x :: acc) []) l

(** val length : 'a1 list -> int **)

let length l =
  (let () = () in fold_right (fun x acc -> plus (Pervasives.succ 0) acc) 0) l

(** val take_drop_last : 'a1 list -> 'a1 list * 'a1 **)

let take_drop_last l =
  let rec take_drop_last0 = function
  | [] -> (raise Not_found) __
  | x :: l' ->
    (match l' with
     | [] -> ([], x)
     | a :: l1 -> let (t, y) = take_drop_last0 l' in ((x :: t), y))
  in take_drop_last0 l

(** val nth_def : 'a1 -> int -> 'a1 list -> 'a1 **)

let nth_def d =
  let rec nth_def0 d0 n = function
  | [] -> d0
  | x :: l' ->
    ((fun fO fS n -> if n=0 then fO () else fS (n-1))
       (fun _ ->
       x)
       (fun n' ->
       nth_def0 d0 n' l')
       n)
  in nth_def0 d

(** val mem_decide : 'a1 coq_Comparable -> 'a1 -> 'a1 list -> bool **)

let rec mem_decide h x = function
| [] -> false
| y :: l' -> if h x y then true else mem_decide h x l'

(** val coq_Mem_decidable :
    'a1 coq_Comparable -> 'a1 -> 'a1 list -> coq_Decidable **)

let coq_Mem_decidable h x l =
  mem_decide h x l

