open Datatypes
open LibFset
open LibList
open LibNat
open LibReflect
open Peano

let __ = let rec f _ = Obj.repr f in Obj.repr f

module type VariablesType = 
 sig 
  type var 
  
  val var_comp : var coq_Comparable
  
  val var_comparable : var coq_Comparable
  
  type vars = var FsetImpl.fset
  
  val var_gen : vars -> var
  
  val var_fresh : vars -> var
 end

module Variables = 
 struct 
  type var = nat
  
  (** val var_comp : var coq_Comparable **)
  
  let var_comp x y =
    nat_compare x y
  
  (** val var_comparable : var coq_Comparable **)
  
  let var_comparable x y =
    nat_compare x y
  
  type vars = var FsetImpl.fset
  
  (** val var_gen_list : nat list -> nat **)
  
  let var_gen_list l =
    plus (S O) (fold_right plus O l)
  
  (** val var_gen : vars -> var **)
  
  let var_gen = failwith "AXIOM NOT REALISED"
  
  (** val var_fresh : vars -> var **)
  
  let var_fresh l =
    var_gen l
 end

