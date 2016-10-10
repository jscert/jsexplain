open Datatypes

type ('k, 'v) t = ('k, 'v) StdMap.t

type ('k, 'v) heap = ('k, 'v) t 

(* type 'a1 comparison = 'a -> 'a -> int *)


(** val empty : ('a1, 'a2) heap **)

let empty =
  StdMap.empty

(** val read : 'a1 comparison -> ('a1 * 'a2) list -> 'a1 -> 'a2 **)

let read h l k = 
  StdMap.find h k l

(** val write : 'a1 comparison ->  ('a1, 'a2) heap -> 'a1 -> 'a2 -> ('a1 * 'a2) list **)

let write h l k v =
  StdMap.add h k v l

(** val rem :
   'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> ('a1 * 'a2) list **)

let rem h l k =
  StdMap.remove h k l

(** val read_option :
   'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> 'a2 option **)

let read_option h l k =
  StdMap.find_option h k l

(** val indom_dec :
   'a1 comparison -> ('a1, 'a2) heap -> 'a1 -> bool **)

let indom_dec h l k = 
  StdMap.mem h k l

(** val to_list : 'a1 comparison -> ('a1, 'a2) heap -> ('a1 * 'a2) list **)

let to_list h l =
  StdMap.fold (fun k v acc -> (k,v)::acc) l []

