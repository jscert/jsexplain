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

(** Call when a spec assertion fails

    Spec assertions are not required to be executed by implementations at runtime. If they fail, the spec is
    self-inconsistent. They are useful for us to include, execute, and eventually theorem prove against.

    They often consist of type checks, which we include explicitly, even though are guaranteed safe in our typed ML.

    This function will not return, but terminate execution immediately. (hopefully in such a way that the execution
    environment can handle cleanly).

    [failwith msg] is preferred in situations where the failure is generic.
    Use something else when the specification {i could} assert this, but presently doesn't.
*)
let spec_assertion_failure _ =
  Debug.impossible_because __LOC__ "spec assertion failed"

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

let if_empty_label2 s r k kfail =
  if label_compare r.res_label Coq_label_empty
  then k ()
  else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
         s
         ("[if_empty_label] received a normal result with non-empty label.")

let if_empty_label s r k = if_empty_label2 s r k (fun x -> x)

(** val if_some : 'a1 option -> ('a1 -> 'a2 resultof) -> 'a2 resultof **)

let if_some2 op k kfail =
  match op with
  | Some a -> k a
  | None ->
    (fun s -> Debug.impossible_because __LOC__ s; kfail Coq_result_impossible)
      ("[if_some] called with [None].")

let if_some op k = if_some2 op k (fun x -> x)

(** val if_some_or_default : 'a2 option -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let if_some_or_default o d k =
  option_case d k o

let if_some_or_apply_default o d k =
  match o with
  | Some x -> k x
  | None   -> k d

(** val if_result_some :
    'a1 resultof -> ('a1 -> 'a2 resultof) -> 'a2 resultof **)

let if_result_some2 w k kfail =
  match w with
  | Coq_result_some a -> k a
  | Coq_result_not_yet_implemented -> kfail Coq_result_not_yet_implemented
  | Coq_result_impossible -> kfail Coq_result_impossible
  | Coq_result_bottom s0 -> kfail (Coq_result_bottom s0)

let if_result_some w k = if_result_some2 w k (fun x -> x)

(** Unpack a result assuming it contains a Coq_specret_out *)
let if_ter2 w k kfail =
  if_result_some2 w (fun sp ->
    match sp with
    | Coq_specret_out (s, r) -> k s r
    | _ -> failwith "if_ter2 failed: not a Coq_specret_out"
  ) kfail

let if_ter w k = if_ter2 w k (fun x -> x)

(** if_ter, repacking the specret_out to erase the polymorphic type associated with specret_val *)
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

let if_success2 w k kfail =
  if_ter2 w (fun s0 r ->
    match r.res_type with
    | Coq_restype_normal -> if_empty_label2 s0 r (fun x -> k s0 r.res_value) kfail
    | Coq_restype_break -> kfail (res_out s0 r)
    | Coq_restype_continue -> kfail (res_out s0 r)
    | Coq_restype_return -> kfail (res_out s0 r)
    | Coq_restype_throw -> kfail (res_out s0 r))
    kfail

let if_success w k =
  if_success2 w k (fun x -> x)

let assert_success w k =
  if_success2 w k spec_assertion_failure

(** val if_void : result_void -> (state -> result) -> result **)

let if_void w k =
  if_success w (fun s rv ->
    match rv with
    | Coq_resvalue_empty -> k s
    | Coq_resvalue_value v ->
        (Debug.impossible_resvalue __LOC__ rv "[if_void called] with non-void result value.";
        Coq_result_impossible)
    | Coq_resvalue_ref r ->
        (Debug.impossible_resvalue __LOC__ rv "[if_void called] with non-void result value.";
        Coq_result_impossible))

(** val if_not_throw : result -> (state -> res -> result) -> result **)

let if_not_throw w k =
  if_ter w (fun s0 r ->
    match r.res_type with
    | Coq_restype_normal -> k s0 r
    | Coq_restype_break -> k s0 r
    | Coq_restype_continue -> k s0 r
    | Coq_restype_return -> k s0 r
    | Coq_restype_throw -> res_out s0 r)

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
    | Coq_restype_break -> res_out s r
    | Coq_restype_continue -> res_out s r
    | Coq_restype_return -> if_empty_label s r (fun x -> k2 s r.res_value)
    | Coq_restype_throw -> res_out s r)

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
let if_value2 w k kfail =
  if_success2 w (fun s rv ->
    match rv with
    | Coq_resvalue_empty ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_value] called with non-value.")
    | Coq_resvalue_value v -> k s v
    | Coq_resvalue_ref r ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_value] called with non-value."))
    kfail

let if_value w k =
  if_value2 w k (fun x -> x)

let if_bool2 w k kfail =
  if_value2 w (fun s v ->
    match v with
    | Coq_value_undef ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_null ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_bool b -> k s b
    | Coq_value_number n ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_string s0 ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")
    | Coq_value_object o ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value.")) kfail

let if_bool w k =
  if_bool2 w k (fun x -> x)

(** val if_object :
    result -> (state -> object_loc -> 'a1 specres) -> 'a1 specres **)

let if_object2 w k kfail =
  if_value2 w (fun s v ->
    match v with
    | Coq_value_object l -> k s l
    | _ ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_object] called on a non-object.")) kfail

let if_object w k = if_object2 w k (fun x -> x)

(** val if_string :
    result -> (state -> string -> 'a1 specres) -> 'a1 specres **)

let if_string2 w k kfail =
  if_value2 w (fun s v ->
    match v with
    | Coq_value_string s0 -> k s s0
    | _ ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s ("[if_string] called on a non-string value.")) kfail

let if_string w k = if_string2 w k (fun x -> x)

(** val if_number :
    result -> (state -> number -> 'a1 specres) -> 'a1 specres **)

let if_number2 w k kfail =
  if_value2 w (fun s v ->
    match v with
    | Coq_value_number n -> k s n
    | _ ->
      (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; kfail Coq_result_impossible)
        s
        ("[if_number] called with non-number value.")) kfail

let if_number w k = if_number2 w k (fun x -> x)

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

(** Unpacks a Coq_specret_val (specification return value), returns an abrupt Coq_specret_out *)
let if_spec2 w k kfail =
  if_result_some2 w (fun sp ->
    match sp with
    | Coq_specret_val (s0, a) -> k s0 a
    | Coq_specret_out (s0, r) -> kfail (if_abort r (fun _ -> res_out s0 r))) kfail

let if_spec w k = if_spec2 w k (fun x -> x)

(** A Specification assertion that b is true, continuing with k,
    or failing with Coq_result_impossible otherwise *)
let check_assert b k =
  if b then k () else spec_assertion_failure ()

type ('t, 'a) if_ret_type =
| Return of 't resultof [@f result]
| Continue of 'a [@f cont]

(** Executes the continuation with state s of a [Continue s],
    else returns the result [r] of [Return r].

    Bound to the syntax: [let%ret s = e1 in e2]
    Which compiles to: [let_ret e1 (fun s -> e2)]

    Example usage:
      [let%ret s =
        if condition
        then Return (ret_val ...)
        else Continue s
      in continuation]
*)
let let_ret2 w k kret =
  match w with
  | Continue s -> k s
  | Return  r -> kret r

let let_ret w k = let_ret2 w k (fun x -> x)
let let_ret_ret w k = let_ret2 w k (fun x -> Return x)

let if_some_ret w k = if_some2 w k (fun x -> Return x)

let if_success_ret w k =
  if_success2 w k (fun x -> Return x)

let if_value_ret w k =
  if_value2 w k (fun x -> Return x)

let assert_value_ret w k =
  if_value2 w k spec_assertion_failure

let if_object_ret w k =
  if_object2 w k (fun x -> Return x)

let if_number_ret w k =
  if_number2 w k (fun x -> Return x)

let if_bool_ret w k =
  if_bool2 w k (fun x -> Return x)

let assert_object w k =
  if_object2 w k spec_assertion_failure

let assert_object_ret w k =
  if_object2 w k spec_assertion_failure

let assert_bool w k =
  if_bool2 w k spec_assertion_failure

let assert_bool_ret w k =
  if_bool2 w k spec_assertion_failure

let assert_string w k =
  if_string2 w k spec_assertion_failure

let assert_string_ret w k =
  if_string2 w k spec_assertion_failure

let check_assert_ret b k =
  if b then k () else spec_assertion_failure ()

let if_spec_ret w k =
  if_spec2 w k (fun x -> Return x)

let ifx_prim w k = if_prim w k
let ifx_number w k = if_number w k
let ifx_string w k = if_string w k
let ifx_success_state a b c = if_success_state a b c
let ifx_some_or_default v d f = if_some_or_default v d f
let ifx_success_or_return a b c = if_success_or_return a b c
let ifx_empty_label a b c = if_empty_label a b c
let ifx_any_or_throw a b c = if_any_or_throw a b c

let rec iterate l acc f = iterate' l (Continue acc) f
and iterate' l acc f = match l with
| []      -> acc
| x :: l' -> let_ret_ret acc (fun v -> iterate' l' (f x v) f)

(** A continuation-passing while loop with early function-level return.

    [repeat condition acc f] will: {ul
    {- Initially set the accumulator to be [Continue acc].}
    {- Repeatedly: {ul
      {- If the accumulator is [Return x], then the loop terminates with value [Return x].}
      {- Otherwise, the accumulator will be [Continue v]. {ul
        {- If, [condition v] is [true], then repeat the loop with the value of the accumulator being [f v].}
        {- Otherwise, the loop terminates with the value [Continue v].}
    }}}}}

    When combined with a [let%ret] monadic binder, a repeat loop evaluating to a [Return x] value will return [x] as the
    function's result, whereas a [Continue v] value will bind [v] to the binder's pattern for use in the continuation.
*)
let rec repeat condition acc f = repeat' condition (Continue acc) f
and repeat' condition acc f =
  let_ret_ret acc (fun v ->
    if condition v then repeat' condition (f v) f
    else acc)

