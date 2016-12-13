open JsCommon
(*open JsNumber*)
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open Shared

type 't resultof =
| Coq_result_some of 't [@f value]
| Coq_result_not_yet_implemented
| Coq_result_impossible
| Coq_result_bottom of state [@f state]

type 't specres = 't specret resultof

(** val res_out : out -> 'a1 specres **)

let res_out s r =
  Coq_result_some (Coq_specret_out (s, r))

(** val res_spec : state -> 'a1 -> 'a1 specres **)

let res_spec s a =
  Coq_result_some (Coq_specret_val (s, a))

(** val res_ter : state -> res -> result **)

let res_ter s r =
  res_out s r

(** val res_void : state -> result_void **)

let res_void s =
  res_out s res_empty

(** val get_arg : int -> value list -> value **)

let get_arg x l =
  nth_def Coq_value_undef x l

(** val get_arg_first_and_rest : value list -> value * value list **)

let get_arg_first_and_rest lv =
  ((get_arg 0 lv),
    (match lv with
     | [] -> []
     | v :: rest -> rest))

(** val destr_list : 'a1 list -> 'a2 -> ('a1 -> 'a2) -> 'a2 **)

let destr_list l d f =
  match l with
  | [] -> d
  | a :: l0 -> f a

(** val if_empty_label :
    state -> res -> (unit -> 'a1 resultof) -> 'a1 resultof **)

let if_empty_label s r k =
  if label_compare r.res_label Coq_label_empty
  then k ()
  else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
         s
         ("[if_empty_label] received a normal result with non-empty label.")

(** val if_some : 'a1 option -> ('a1 -> 'a2 resultof) -> 'a2 resultof **)

let if_some op k =
  match op with
  | Some a -> k a
  | None ->
    (fun s -> Debug.impossible_because __LOC__ s; Coq_result_impossible)
      ("[if_some] called with [None].")

(** val if_some_or_default : 'a2 option -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let if_some_or_default o d k =
  option_case d k o

let if_some_or_apply_default o d k =
  match o with
  | Some x -> k x
  | None   -> k d

(** val if_result_some :
    'a1 resultof -> ('a1 -> 'a2 resultof) -> 'a2 resultof **)

let if_result_some w k =
  match w with
  | Coq_result_some a -> k a
  | Coq_result_not_yet_implemented -> Coq_result_not_yet_implemented
  | Coq_result_impossible -> Coq_result_impossible
  | Coq_result_bottom s0 -> Coq_result_bottom s0

(** val if_ter : result -> (state -> res -> 'a1 specres) -> 'a1 specres **)
(** Unpack a result assuming it contains a Coq_specret_out *)

let if_ter w k =
  if_result_some w (fun sp ->
    match sp with
    | Coq_specret_out (s, r) -> k s r
    | _ -> assert false
  )

(** val throw_result : result -> 'a1 specres **)
(** This appears to be the identity function (with input checking, this matches
    the behaviour of the original, with the out type flattened. *)

let throw_result w =
  if_ter w (fun s r -> res_out s r)

(** val if_success_state :
    resvalue -> result -> (state -> resvalue -> result) -> result **)

let if_success_state rv w k =
  if_ter w (fun s0 r ->
    match r.res_type with
    | Coq_restype_normal ->
      if_empty_label s0 r (fun x ->
        k s0
          (if resvalue_compare r.res_value Coq_resvalue_empty
           then rv
           else r.res_value))
    | Coq_restype_throw -> res_ter s0 r
    | _ -> res_ter s0 (res_overwrite_value_if_empty rv r)
    )

(** val if_success :
    result -> (state -> resvalue -> 'a1 specres) -> 'a1 specres **)

let if_success w k =
  if_ter w (fun s0 r ->
    match r.res_type with
    | Coq_restype_normal -> if_empty_label s0 r (fun x -> k s0 r.res_value)
    | Coq_restype_break -> res_out s0 r
    | Coq_restype_continue -> res_out s0 r
    | Coq_restype_return -> res_out s0 r
    | Coq_restype_throw -> res_out s0 r)

(** val if_void : result_void -> (state -> result) -> result **)

let if_void w k =
  if_success w (fun s rv ->
    match rv with
    | Coq_resvalue_empty -> k s
    | Coq_resvalue_value v ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_void called] with non-void result value.")
    | Coq_resvalue_ref r ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_void called] with non-void result value."))

(** val if_not_throw : result -> (state -> res -> result) -> result **)

let if_not_throw w k =
  if_ter w (fun s0 r ->
    match r.res_type with
    | Coq_restype_normal -> k s0 r
    | Coq_restype_break -> k s0 r
    | Coq_restype_continue -> k s0 r
    | Coq_restype_return -> k s0 r
    | Coq_restype_throw -> w)

(** val if_any_or_throw :
    result -> (state -> res -> result) -> (state -> value -> result) ->
    result **)

let if_any_or_throw w k1 k2 =
  if_ter w (fun s r ->
    match r.res_type with
    | Coq_restype_normal -> k1 s r
    | Coq_restype_break -> k1 s r
    | Coq_restype_continue -> k1 s r
    | Coq_restype_return -> k1 s r
    | Coq_restype_throw ->
      (match r.res_value with
       | Coq_resvalue_value v -> if_empty_label s r (fun x -> k2 s v)
       | _ ->
         (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
           s
           ("[if_any_or_throw] called with a non-value result.")))

(** val if_success_or_return :
    result -> (state -> result) -> (state -> resvalue -> result) -> result **)

let if_success_or_return w k1 k2 =
  if_ter w (fun s r ->
    match r.res_type with
    | Coq_restype_normal -> if_empty_label s r (fun x -> k1 s)
    | Coq_restype_break -> w
    | Coq_restype_continue -> w
    | Coq_restype_return -> if_empty_label s r (fun x -> k2 s r.res_value)
    | Coq_restype_throw -> w)

(** val if_break : result -> (state -> res -> result) -> result **)

let if_break w k =
  if_ter w (fun s r ->
    match r.res_type with
    | Coq_restype_normal -> res_ter s r
    | Coq_restype_break -> k s r
    | Coq_restype_continue -> res_ter s r
    | Coq_restype_return -> res_ter s r
    | Coq_restype_throw -> res_ter s r)

(** This method is equivalent to the [!] operator of section 5.2 Algorithm Conventions
    ppx_monads maps this to the [let%value] syntax. *)
let if_value w k =
  if_success w (fun s rv ->
    match rv with
    | Coq_resvalue_empty ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_value] called with non-value.")
    | Coq_resvalue_value v -> k s v
    | Coq_resvalue_ref r ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_value] called with non-value."))

(** val if_bool : result -> (state -> bool -> 'a1 specres) -> 'a1 specres **)

let if_bool w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_undef ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_null ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_bool b -> k s b
    | Coq_value_number n ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_string s0 ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_object o ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value."))

(** val if_object :
    result -> (state -> object_loc -> 'a1 specres) -> 'a1 specres **)

let if_object w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_object l -> k s l
    | _ ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_object] called on a non-object."))

(** val if_string :
    result -> (state -> string -> 'a1 specres) -> 'a1 specres **)

let if_string w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_string s0 -> k s s0
    | _ ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s ("[if_string] called on a non-string value."))

(** val if_number :
    result -> (state -> number -> 'a1 specres) -> 'a1 specres **)

let if_number w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_number n -> k s n
    | _ ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_number] called with non-number value."))

(** val if_prim : result -> (state -> prim -> 'a1 specres) -> 'a1 specres **)

let if_prim w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_object o ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
        s
        ("[if_primitive] called on an object.")
    | _ -> k s v)

(** val convert_option_attributes :
    attributes option -> full_descriptor option **)

let convert_option_attributes o =
  map (fun a -> Coq_full_descriptor_some a) o

(** val if_abort : out -> (unit -> 'a1 resultof) -> 'a1 resultof **)

let if_abort r k =
    if restype_compare r.res_type Coq_restype_normal
    then (Debug.impossible_because __LOC__ "[if_abort] received a normal result!"; Coq_result_impossible)
    else k ()

(** val if_spec :
    'a1 specres -> (state -> 'a1 -> 'a2 specres) -> 'a2 specres **)
(** Unpacks a Coq_specret_val (specification return value), returns an abrupt Coq_specret_out
    This function is nearly equivalent to ReturnIfAbrupt in the ES spec *)
let if_spec w k =
  if_result_some w (fun sp ->
    match sp with
    | Coq_specret_val (s0, a) -> k s0 a
    | Coq_specret_out (s0, r) -> if_abort r (fun _ -> res_out s0 r))

(** A Specification assertion that b is true, continuing with k,
    or failing with Coq_result_impossible otherwise *)
let check_assert b k =
  if b then k () else (Debug.impossible_because __LOC__ "spec assertion failed"; Coq_result_impossible)

(** Executes the continuation if a specret_val, else returns the specret_out.
    Bound to the syntax let%can_return *)
let if_spec_else_return w k =
  if_result_some w (fun sp ->
    match sp with
    | Coq_specret_val (s, v) -> k s v
     (* Don't just reuse w here, the return type needs to be distinct from the input type. *)
    | Coq_specret_out (s, r) -> Coq_result_some (Coq_specret_out (s, r)))

let ifx_prim w k = if_prim w k
let ifx_number w k = if_number w k
let ifx_string w k = if_string w k
let ifx_success_state a b c = if_success_state a b c
let ifx_some_or_default v d f = if_some_or_default v d f
let ifx_success_or_return a b c = if_success_or_return a b c
let ifx_empty_label a b c = if_empty_label a b c
let ifx_any_or_throw a b c = if_any_or_throw a b c
