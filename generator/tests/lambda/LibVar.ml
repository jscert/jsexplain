open Datatypes
open LibFset
open LibList
open LibNat
open LibReflect
open Peano
open Specif

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
  
  let var_gen e =
    var_gen_list
      (let s =
         let h = failwith "AXIOM TO BE REALIZED" in
         (match h with
          | Coq_true -> (fun _ -> Coq_left)
          | Coq_false -> (fun _ -> Coq_right)) __
       in
       match s with
       | Coq_left -> failwith "AXIOM TO BE REALIZED"
       | Coq_right -> failwith "AXIOM TO BE REALIZED")
  
  (** val var_fresh : vars -> var **)
  
  let var_fresh l =
    var_gen l
 end

