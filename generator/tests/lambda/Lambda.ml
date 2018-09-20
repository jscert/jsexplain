open BinNums
open Datatypes
open LibVar

type coq_val =
| Val_int  [@f label0] of coq_Z (* Auto Generated Attributes *)
| Val_clo  [@f label0, label1] of var * trm (* Auto Generated Attributes *)
| Val_err [@f]  (* Auto Generated Attributes *)
and trm =
| Trm_val  [@f label0] of coq_val (* Auto Generated Attributes *)
| Trm_var  [@f label0] of var (* Auto Generated Attributes *)
| Trm_abs  [@f label0, label1] of var * trm (* Auto Generated Attributes *)
| Trm_app  [@f label0, label1] of trm * trm (* Auto Generated Attributes *)
| Trm_try  [@f label0, label1] of trm * trm (* Auto Generated Attributes *)
| Trm_raise  [@f label0] of trm (* Auto Generated Attributes *)

(** val subst : var -> coq_val -> trm -> trm **)

let rec subst x v t = match t with
| Trm_val v0 -> t
| Trm_var y -> if var_comp x y then Trm_val v else t
| Trm_abs (y, t3) ->
  Trm_abs (y, (if var_comp x y then t3 else subst x v t3))
| Trm_app (t1, t2) -> Trm_app ((subst x v t1), (subst x v t2))
| Trm_try (t1, t2) -> Trm_try ((subst x v t1), (subst x v t2))
| Trm_raise t1 -> Trm_raise (subst x v t1)

type beh =
| Beh_ret  [@f label0] of coq_val (* Auto Generated Attributes *)
| Beh_exn  [@f label0] of coq_val (* Auto Generated Attributes *)
| Beh_err [@f]  (* Auto Generated Attributes *)

type res =
| Res_return  [@f label0] of beh (* Auto Generated Attributes *)
| Res_bottom [@f]  (* Auto Generated Attributes *)

(** val if_success : res -> (coq_val -> res) -> res **)

let if_success r k =
  match r with
  | Res_return b ->
    (match b with
     | Beh_ret v -> k v
     | Beh_exn v -> r
     | Beh_err -> r)
  | Res_bottom -> r

(** val if_fault : res -> (coq_val -> res) -> res **)

let if_fault r k =
  match r with
  | Res_return b ->
    (match b with
     | Beh_ret v -> r
     | Beh_exn v -> k v
     | Beh_err -> r)
  | Res_bottom -> r

(** val if_isclo : coq_val -> (var -> trm -> res) -> res **)

let if_isclo v k =
  match v with
  | Val_int z -> Res_return Beh_err
  | Val_clo (x, t) -> k x t
  | Val_err -> Res_return Beh_err

(** val run : nat -> trm -> res **)

let rec run n t =
  match n with
  | O -> Res_bottom
  | S m ->
    (match t with
     | Trm_val v -> Res_return (Beh_ret v)
     | Trm_var x -> Res_return Beh_err
     | Trm_abs (x, t1) ->
       Res_return (Beh_ret (Val_clo (x, t1)))
     | Trm_app (t1, t2) ->
       if_success (run m t1) (fun v1 ->
         if_success (run m t2) (fun v2 ->
           if_isclo v1 (fun x t3 -> run m (subst x v2 t3))))
     | Trm_try (t1, t2) ->
       if_fault (run m t1) (fun v ->
         run m (Trm_app (t2, (Trm_val v))))
     | Trm_raise t1 ->
       if_success (run m t1) (fun v1 -> Res_return (Beh_exn v1)))

