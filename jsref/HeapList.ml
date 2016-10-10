(* DEPRECATED: currently not used, but keep around as a possible replacement for StdMap *)


open Datatypes
open LibList

(* type 'a1 comparison = 'a -> 'a -> int *)

type ('k, 'v) heap = ('k * 'v) list

(** val empty : ('a1, 'a2) heap **)

let empty =
 []


(** val read : 'a1 comparison -> ('a1 * 'a2) list -> 'a1 -> 'a2 **)

let rec read h l k = match l with
| [] -> raise Not_found
| p :: l' -> let (x, v) = p in if int_eq (h x k) 0 then v else read h l' k 

(** val write : 'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> 'a2 -> ('a1 * 'a2) list **)

let write h l k v =
 (k, v) :: l

(** val rem :
   'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> ('a1 * 'a2) list **)

let rem h1 l k =
 filter (fun p -> if int_eq (h1 (fst p) k) 0 then false else true) l

(** val read_option :
   'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> 'a2 option **)

let read_option h l k =
 let rec read_option0 l k =
   match l with
   | [] -> None
   | p :: l' ->
     let (x, v) = p in if int_eq (h x k) 0 then Some v else read_option0 l' k
 in read_option0 l k

(** val indom_dec :
   'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> bool **)

let rec indom_dec h1 l k = match l with
| [] -> false
| p :: l' -> let (x, y) = p in int_eq (h1 x k) 0 || (indom_dec h1 l' k)

(** val to_list : 'a1 comparison -> ('a1, 'a2) heap -> ('a1, 'a2) heap **)

(* MODIFIED TO REMOVE DUPLICATE KEYS  --- warning: quadratic complexity *)
let rec to_list h l =
  match l with
  | [] -> []
  | p :: l' ->  p :: (to_list h (rem h l' (fst p)))



