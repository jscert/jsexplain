open BinNums
open Datatypes
open LibLogic
open LibVar
open Specif

type coq_val =
| Coq_val_int of coq_Z
| Coq_val_clo of Variables.var * trm
| Coq_val_err
and trm =
| Coq_trm_val of coq_val
| Coq_trm_var of Variables.var
| Coq_trm_abs of Variables.var * trm
| Coq_trm_app of trm * trm
| Coq_trm_try of trm * trm
| Coq_trm_raise of trm

(** val subst : Variables.var -> coq_val -> trm -> trm **)

let rec subst x v t =
  let s = subst x v in
  (match t with
   | Coq_trm_val v0 -> t
   | Coq_trm_var y ->
     (match classicT with
      | Coq_left -> Coq_trm_val v
      | Coq_right -> t)
   | Coq_trm_abs (y, t3) ->
     Coq_trm_abs (y,
       (match classicT with
        | Coq_left -> t3
        | Coq_right -> s t3))
   | Coq_trm_app (t1, t2) -> Coq_trm_app ((s t1), (s t2))
   | Coq_trm_try (t1, t2) -> Coq_trm_try ((s t1), (s t2))
   | Coq_trm_raise t1 -> Coq_trm_raise (s t1))

type beh =
| Coq_beh_ret of coq_val
| Coq_beh_exn of coq_val
| Coq_beh_err

type res =
| Coq_res_return of beh
| Coq_res_bottom

(** val if_success : res -> (coq_val -> res) -> res **)

let if_success r k =
  match r with
  | Coq_res_return b ->
    (match b with
     | Coq_beh_ret v -> k v
     | Coq_beh_exn v -> r
     | Coq_beh_err -> r)
  | Coq_res_bottom -> r

(** val if_fault : res -> (coq_val -> res) -> res **)

let if_fault r k =
  match r with
  | Coq_res_return b ->
    (match b with
     | Coq_beh_ret v -> r
     | Coq_beh_exn v -> k v
     | Coq_beh_err -> r)
  | Coq_res_bottom -> r

(** val if_isclo : coq_val -> (Variables.var -> trm -> res) -> res **)

let if_isclo v k =
  match v with
  | Coq_val_int z -> Coq_res_return Coq_beh_err
  | Coq_val_clo (x, t) -> k x t
  | Coq_val_err -> Coq_res_return Coq_beh_err

(** val run : nat -> trm -> res **)

let rec run n t =
  match n with
  | O -> Coq_res_bottom
  | S m ->
    (match t with
     | Coq_trm_val v -> Coq_res_return (Coq_beh_ret v)
     | Coq_trm_var x -> Coq_res_return Coq_beh_err
     | Coq_trm_abs (x, t1) ->
       Coq_res_return (Coq_beh_ret (Coq_val_clo (x, t1)))
     | Coq_trm_app (t1, t2) ->
       if_success (run m t1) (fun v1 ->
         if_success (run m t2) (fun v2 ->
           if_isclo v1 (fun x t3 -> run m (subst x v2 t3))))
     | Coq_trm_try (t1, t2) ->
       if_fault (run m t1) (fun v ->
         run m (Coq_trm_app (t2, (Coq_trm_val v))))
     | Coq_trm_raise t1 ->
       if_success (run m t1) (fun v1 -> Coq_res_return (Coq_beh_exn v1)))

