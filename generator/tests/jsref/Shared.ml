open BinInt
open Datatypes
open LibHeap
open LibReflect
open String0

(** val option_case : 'a2 -> ('a1 -> 'a2) -> 'a1 option -> 'a2 **)

let option_case d f o = match o with
| Some x -> f x
| None -> d

(** val int_of_char : char -> float **)

let int_of_char = (fun c -> float_of_int (int_of_char c))

(** val ascii_comparable : char coq_Comparable **)

let ascii_comparable = (=)

(** val string_sub : string -> float -> float -> string **)

let string_sub s n l =
  substring (Z.abs_nat n) (Z.abs_nat l) s

(** val lt_int_decidable : float -> float -> coq_Decidable **)

let lt_int_decidable = (<)

(** val le_int_decidable : float -> float -> coq_Decidable **)

let le_int_decidable = (<=)

(** val ge_nat_decidable : int -> int -> coq_Decidable **)

let ge_nat_decidable = (>=)

type 'a coq_Pickable_option =
  'a option
  (* singleton inductive, whose constructor was pickable_option_make *)

module HeapGen = 
 functor (Heap:HeapSpec) ->
 struct 
  type ('k, 'v) heap = int * ('k, 'v) Heap.heap
  
  (** val empty : ('a1, 'a2) heap **)
  
  let empty =
    (0, Heap.empty)
  
  (** val to_list : ('a1, 'a2) heap -> ('a1 * 'a2) list **)
  
  let to_list h =
    Heap.to_list (snd h)
  
  (** val read : 'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> 'a2 **)
  
  let read h h0 =
    Heap.read h (snd h0)
  
  (** val write :
      ('a1, 'a2) heap -> 'a1 -> 'a2 -> int * ('a1, 'a2) Heap.heap **)
  
  let write h k v =
    let (id, h0) = h in ((Pervasives.succ id), (Heap.write (snd h) k v))
  
  (** val rem :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> int * ('a1, 'a2)
      Heap.heap **)
  
  let rem h h0 k =
    let (id, h1) = h0 in ((Pervasives.succ id), (Heap.rem h (snd h0) k))
  
  (** val read_option :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> 'a2 option **)
  
  let read_option h h0 =
    Heap.read_option h (snd h0)
  
  (** val indom_decidable :
      'a1 coq_Comparable -> ('a1, 'a2) heap -> 'a1 -> coq_Decidable **)
  
  let indom_decidable h1 p =
    let (n, h0) = p in Heap.indom_decidable h1 (snd (n, h0))
 end

