(************************************************************
* Wrapper around the library TLC                            *
*************************************************************)

Set Implicit Arguments.
Require Export LibTactics LibCore LibVar LibEnv.
Generalizable Variables A B.


(*==========================================================*)
(* * Definitions *)

(************************************************************)
(* ** Syntax *)

(** Grammar of values and terms *)

Inductive val : Type :=
  | val_int : int -> val
  | val_clo : var -> trm -> val
  | val_err : val

with trm : Type :=
  | trm_val : val -> trm
  | trm_var : var -> trm
  | trm_abs : var -> trm -> trm
  | trm_app : trm -> trm -> trm
  | trm_try : trm -> trm -> trm
  | trm_raise : trm -> trm.

Coercion trm_val : val >-> trm.

(** Substitution *)

Fixpoint subst (x:var) (v:val) (t:trm) : trm :=
  let s := subst x v in
  match t with
  | trm_val v => t
  | trm_var y => If x = y then trm_val v else t
  | trm_abs y t3 => trm_abs y (If x = y then t3 else s t3)
  | trm_app t1 t2 => trm_app (s t1) (s t2)  
  | trm_try t1 t2 => trm_try (s t1) (s t2) 
  | trm_raise t1 => trm_raise (s t1)
  end.


(************************************************************)
(* ** Definition shared by the semantics *)

Inductive beh :=
  | beh_ret : val -> beh
  | beh_exn : val -> beh
  | beh_err : beh.
  
Coercion beh_ret : val >-> beh.


(*==========================================================*)
(* * Definitions *)

Implicit Types v : val.
Implicit Types t : trm.
Implicit Types b : beh.


(************************************************************)
(* ** Results *)

(** Grammar of results of the interpreter *)

Inductive res :=
  | res_return : beh -> res
  | res_bottom : res.

Coercion res_return : beh >-> res.
Implicit Types r : res.


(************************************************************)
(* ** Monadic operators *)

(** Bind-style operators *)

Definition if_success (r:res) (k:val->res) : res :=
  match r with
  | res_return (beh_ret v) => k v
  | _ => r
  end.

Definition if_fault (r:res) (k:val->res) : res :=
  match r with
  | res_return (beh_exn v) => k v
  | _ => r
  end.

Definition if_isclo (v:val) (k:var->trm->res) : res :=
  match v with
  | val_clo x t => k x t
  | _ => beh_err
  end.


(************************************************************)
(* ** Interpreter *)

(** Definition of the interpreter *)

Fixpoint run (n:nat) (t:trm) : res :=
  match n with 
  | O => res_bottom
  | S m => 
    match t with
    | trm_val v => v
    | trm_abs x t1 => val_clo x t1
    | trm_var x => beh_err
    | trm_app t1 t2 => 
       if_success (run m t1) (fun v1 =>
         if_success (run m t2) (fun v2 =>
            if_isclo v1 (fun x t3 =>
              run m (subst x v2 t3))))
    | trm_try t1 t2 =>
       if_fault (run m t1) (fun v => run m (trm_app t2 v))
    | trm_raise t1 => 
       if_success (run m t1) (fun v1 => beh_exn v1)
    end
  end.

(* As classical logic statements are now unused, they should not be extracted
   (otherwise, useless errors will be launched). *)
Extraction Inline classicT LibEpsilon.Inhab_witness LibEpsilon.epsilon LibEpsilon.epsilon_def indefinite_description.

Extract Constant nat_comparable => "(=)".

Set Extraction AccessOpaque.
Unset Extraction Optimize.
Unset Extraction KeepSingleton.
Unset Extraction AutoInline.
Separate Extraction run.

