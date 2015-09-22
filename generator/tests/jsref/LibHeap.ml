open Datatypes
open LibBool
open LibList
open LibReflect

let __ = let rec f _ = Obj.repr f in Obj.repr f

module type HeapSpec = 
 sig 
  type ('x0, 'x) heap 
  
  val empty : ('a1, 'a2) heap
  
  val write : ('a1, 'a2) heap -> 'a1 -> 'a2 -> ('a1, 'a2) heap
  
  val to_list : ('a1, 'a2) heap -> ('a1 * 'a2) list
  
  val read : 'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> 'a2
  
  val read_option :
    'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> 'a2 option
  
  val rem : 'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> ('a1, 'a2) heap
  
  val indom_decidable :
    'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> coq_Decidable
 end

module HeapList = 
 struct 
  type ('k, 'v) heap = ('k * 'v) list
  
  (** val empty : ('a1, 'a2) heap **)
  
  let empty =
    []
  
  (** val to_list : ('a1, 'a2) heap -> ('a1, 'a2) heap **)
  
  let to_list l =
    l
  
  (** val assoc : 'a1 coq_Comparable -> 'a1 -> ('a1 * 'a2) list -> 'a2 **)
  
  let rec assoc h1 k = function
  | [] -> (raise Not_found) __
  | p :: l' -> let (x, v) = p in if h1 x k then v else assoc h1 k l'
  
  (** val read : 'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> 'a2 **)
  
  let read h l k =
    assoc h k l
  
  (** val write : ('a1, 'a2) heap -> 'a1 -> 'a2 -> ('a1 * 'a2) list **)
  
  let write l k v =
    (k, v) :: l
  
  (** val rem :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> ('a1 * 'a2) list **)
  
  let rem h1 l k =
    filter (fun p -> if h1 (fst p) k then false else true) l
  
  (** val read_option :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> 'a2 option **)
  
  let read_option h =
    let rec read_option0 l k =
      match l with
      | [] -> None
      | p :: l' ->
        let (x, v) = p in if h x k then Some v else read_option0 l' k
    in read_option0
  
  (** val mem_assoc :
      'a2 coq_Comparable -> 'a2 -> ('a2 * 'a1) list -> bool **)
  
  let rec mem_assoc h1 k = function
  | [] -> false
  | p :: l' -> let (x, y) = p in coq_or (h1 x k) (mem_assoc h1 k l')
  
  (** val indom_dec :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> bool **)
  
  let indom_dec h1 h k =
    mem_assoc h1 k h
  
  (** val indom_decidable :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> coq_Decidable **)
  
  let indom_decidable h h0 k =
    indom_dec h h0 k
 end

