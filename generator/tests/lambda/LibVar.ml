open Datatypes
open LibEpsilon
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
  
  let var_comp =
    nat_comparable
  
  (** val var_comparable : var coq_Comparable **)
  
  let var_comparable =
    var_comp
  
  type vars = var FsetImpl.fset
  
  (** val var_gen_list : nat list -> nat **)
  
  let var_gen_list l =
    plus (S O) (fold_right plus O l)
  
  (** val var_gen : vars -> var **)
  
  let var_gen e =
    var_gen_list (epsilon __)
  
  (** val var_fresh : vars -> var **)
  
  let var_fresh l =
    var_gen l
 end

