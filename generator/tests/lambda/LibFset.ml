open LibList
open LibSet

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type FsetSig = 
 sig 
  type 'x fset 
  
  val empty : 'a1 fset
  
  val singleton : 'a1 -> 'a1 fset
  
  val union : 'a1 fset -> 'a1 fset -> 'a1 fset
  
  val inter : 'a1 fset -> 'a1 fset -> 'a1 fset
  
  val remove : 'a1 fset -> 'a1 fset -> 'a1 fset
  
  val from_list : 'a1 list -> 'a1 fset
 end

module FsetImpl = 
 struct 
  type 'a fset = 'a set
  
  (** val build_fset : __ -> 'a1 set **)
  
  let build_fset _ =
    __
  
  (** val empty : 'a1 fset **)
  
  let empty =
    build_fset __
  
  (** val singleton : 'a1 -> 'a1 set **)
  
  let singleton x =
    build_fset __
  
  (** val union : 'a1 fset -> 'a1 fset -> 'a1 set **)
  
  let union e f =
    build_fset __
  
  (** val inter : 'a1 fset -> 'a1 fset -> 'a1 set **)
  
  let inter e f =
    build_fset __
  
  (** val remove : 'a1 fset -> 'a1 fset -> 'a1 set **)
  
  let remove e f =
    build_fset __
  
  (** val from_list : 'a1 list -> 'a1 fset **)
  
  let from_list l =
    fold_right (fun x acc -> union (singleton x) acc) empty l
 end

