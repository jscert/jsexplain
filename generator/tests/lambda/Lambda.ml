open BinNums
open Datatypes
open LibNat
open LibVar

type coq_val =
| Coq_val_int  [@f label0] of coq_Z (** Auto Generated Attributes **)
| Coq_val_clo  [@f label0, label1] of Variables.var * trm (** Auto Generated Attributes **)
| Coq_val_err [@f]  (** Auto Generated Attributes **)
and trm =
| Coq_trm_val  [@f label0] of coq_val (** Auto Generated Attributes **)
| Coq_trm_var  [@f label0] of Variables.var (** Auto Generated Attributes **)
| Coq_trm_abs  [@f label0, label1] of Variables.var * trm (** Auto Generated Attributes **)
| Coq_trm_app  [@f label0, label1] of trm * trm (** Auto Generated Attributes **)
| Coq_trm_try  [@f label0, label1] of trm * trm (** Auto Generated Attributes **)
| Coq_trm_raise  [@f label0] of trm (** Auto Generated Attributes **)

(** val subst : Variables.var -> coq_val -> trm -> trm **)

let rec subst x v t =
  let s = subst x v in
  (match t with
   | Coq_trm_val v0 -> t
   | Coq_trm_var y -> if nat_compare x y then Coq_trm_val v else t
   | Coq_trm_abs (y, t3) ->
     Coq_trm_abs (y, (if nat_compare x y then t3 else s t3))
   | Coq_trm_app (t1, t2) -> Coq_trm_app ((s t1), (s t2))
   | Coq_trm_try (t1, t2) -> Coq_trm_try ((s t1), (s t2))
   | Coq_trm_raise t1 -> Coq_trm_raise (s t1))

type beh =
| Coq_beh_ret  [@f label0] of coq_val (** Auto Generated Attributes **)
| Coq_beh_exn  [@f label0] of coq_val (** Auto Generated Attributes **)
| Coq_beh_err [@f]  (** Auto Generated Attributes **)

type res =
| Coq_res_return  [@f label0] of beh (** Auto Generated Attributes **)
| Coq_res_bottom [@f]  (** Auto Generated Attributes **)

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

