open LibOperation
open LibReflect
open Peano

(** val list_eq_nil_decidable : 'a1 list -> coq_Decidable **)

let list_eq_nil_decidable l = match l with
| [] -> true
| a :: l0 -> false

(** val fold_right : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let rec fold_right f acc l = match l with
  | [] -> acc
  | x :: l' -> f x (fold_right f acc l')

(** val fold_left : ('a1 -> 'a2 -> 'a2) -> 'a2 -> 'a1 list -> 'a2 **)

let rec fold_left f acc l = match l with
  | [] -> acc
  | x :: l' -> fold_left f (f x acc) l'

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let map f l =
  fold_right (fun x acc -> (f x) :: acc) [] l

(** val filter : 'a1 predb -> 'a1 list -> 'a1 list **)

let filter f l =
  fold_right (fun x acc -> if f x then x :: acc else acc) [] l

(** val append : 'a1 list -> 'a1 list -> 'a1 list **)

let append l1 l2 =
  fold_right (fun x acc -> x :: acc) l2 l1

(** val concat : 'a1 list list -> 'a1 list **)

let concat l =
  fold_right append [] l

(** val rev : 'a1 list -> 'a1 list **)

let rev l =
  fold_left (fun x acc -> x :: acc) [] l

(** val length : 'a1 list -> int **)

let length l =
  fold_right (fun x acc -> plus 1 acc) 0 l

(** val take_drop_last : 'a1 list -> 'a1 list * 'a1 **)

let rec take_drop_last l = match l with
  | [] -> raise Not_found
  | x :: l' ->
    (match l' with
     | [] -> ([], x)
     | a :: l1 -> let (t, y) = take_drop_last l' in ((x :: t), y))

(** val nth_def : 'a1 -> int -> 'a1 list -> 'a1 **)

let rec nth_def d n l = match l with
  | [] -> d
  | x :: l' -> if int_eq n 0 then x else nth_def d (n-1) l'

(** val mem_decide : 'a1 coq_Comparable -> 'a1 -> 'a1 list -> bool **)

let rec mem_decide h x l = match l with
| [] -> false
| y :: l' -> if h x y then true else mem_decide h x l'

(** val coq_Mem_decidable :
    'a1 coq_Comparable -> 'a1 -> 'a1 list -> coq_Decidable **)

let coq_Mem_decidable h x l =
  mem_decide h x l

