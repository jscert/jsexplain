open JsCommon
open JsNumber
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open Shared

type __ = unit

type 't resultof =
| Coq_result_some  [@f label0] of 't (** Auto Generated Attributes **)
| Coq_result_not_yet_implemented [@f]  (** Auto Generated Attributes **)
| Coq_result_impossible [@f]  (** Auto Generated Attributes **)
| Coq_result_bottom  [@f label0] of state (** Auto Generated Attributes **)

type 't specres = 't specret resultof

(** val res_out : out -> 'a1 specres **)

let res_out o =
  Coq_result_some (Coq_specret_out o)

(** val res_spec : state -> 'a1 -> 'a1 specres **)

let res_spec s a =
  Coq_result_some (Coq_specret_val (s, a))

type retn = __ specret

type result = retn resultof

(** val res_ter : state -> res -> result **)

let res_ter s r =
  res_out (Coq_out_ter (s, r))

type result_void = result

(** val res_void : state -> result_void **)

let res_void s =
  res_out (out_void s)

(** val out_from_retn : retn -> out **)

let out_from_retn _foo_ = match _foo_ with
| Coq_specret_val (s, _) -> assert false (* absurd case *)
| Coq_specret_out o -> o

(** val result_out : out -> result **)

let result_out o =
  res_out o

(** val get_arg : int -> value list -> value **)

let get_arg =
  nth_def (Coq_value_prim Coq_prim_undef)

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

let if_empty_label (s:state) r k =
  if label_comparable r.res_label Coq_label_empty
  then k ()
  else (fun (s:state) message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
         s
         ("[if_empty_label] received a normal result with non-empty label.")

(** val if_some : 'a1 option -> ('a1 -> 'a2 resultof) -> 'a2 resultof **)

let if_some op k =
  match op with
  | Some a -> k a
  | None ->
    (fun s ->
  print_endline (__LOC__ ^ ": Stuck because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_impossible)
      ("[if_some] called with [None].")

(** val if_some_or_default : 'a2 option -> 'a1 -> ('a2 -> 'a1) -> 'a1 **)

let if_some_or_default o d k =
  option_case d k o

(** val if_result_some :
    'a1 resultof -> ('a1 -> 'a2 resultof) -> 'a2 resultof **)

let if_result_some w k =
  match w with
  | Coq_result_some a -> k a
  | Coq_result_not_yet_implemented -> Coq_result_not_yet_implemented
  | Coq_result_impossible -> Coq_result_impossible
  | Coq_result_bottom s0 -> Coq_result_bottom s0

(** val if_out_some : result -> (out -> 'a1 resultof) -> 'a1 resultof **)

let if_out_some w k =
  if_result_some w (fun sp -> k (out_from_retn sp))

(** val throw_result : result -> 'a1 specres **)

let throw_result w =
  if_out_some w (fun o -> res_out o)

(** val if_ter : result -> (state -> res -> 'a1 specres) -> 'a1 specres **)

let if_ter w k =
  if_out_some w (fun o ->
    match o with
    | Coq_out_div -> res_out o
    | Coq_out_ter (s0, r) -> k s0 r)

(** val if_success_state :
    resvalue -> result -> (state -> resvalue -> result) -> result **)

let if_success_state rv w k =
  if_ter w (fun (s0:state) r ->
    match r.res_type with
    | Coq_restype_normal ->
      if_empty_label s0 r (fun x ->
        k s0
          (if resvalue_comparable r.res_value Coq_resvalue_empty
           then rv
           else r.res_value))
    | Coq_restype_break -> res_ter s0 (res_overwrite_value_if_empty rv r)
    | Coq_restype_continue -> res_ter s0 (res_overwrite_value_if_empty rv r)
    | Coq_restype_return -> res_ter s0 (res_overwrite_value_if_empty rv r)
    | Coq_restype_throw -> res_ter s0 r)

(** val if_success :
    result -> (state -> resvalue -> 'a1 specres) -> 'a1 specres **)

let if_success w k =
  if_ter w (fun s0 r ->
    match r.res_type with
    | Coq_restype_normal -> if_empty_label s0 r (fun x -> k s0 r.res_value)
    | Coq_restype_break -> res_out (Coq_out_ter (s0, r))
    | Coq_restype_continue -> res_out (Coq_out_ter (s0, r))
    | Coq_restype_return -> res_out (Coq_out_ter (s0, r))
    | Coq_restype_throw -> res_out (Coq_out_ter (s0, r)))

(** val if_void : result_void -> (state -> result) -> result **)

let if_void w k =
  if_success w (fun s rv ->
    match rv with
    | Coq_resvalue_empty -> k s
    | Coq_resvalue_value v ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_void called] with non-void result value.")
    | Coq_resvalue_ref r ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
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
       | Coq_resvalue_empty ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_any_or_throw] called with a non-value result.")
       | Coq_resvalue_value v -> if_empty_label s r (fun x -> k2 s v)
       | Coq_resvalue_ref r0 ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
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

(** val if_value :
    result -> (state -> value -> 'a1 specres) -> 'a1 specres **)

let if_value w k =
  if_success w (fun s rv ->
    match rv with
    | Coq_resvalue_empty ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_value] called with non-value.")
    | Coq_resvalue_value v -> k s v
    | Coq_resvalue_ref r ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_value] called with non-value."))

(** val if_bool : result -> (state -> bool -> 'a1 specres) -> 'a1 specres **)

let if_bool w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_prim p ->
      (match p with
       | Coq_prim_undef ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_bool] called with non-boolean value.")
       | Coq_prim_null ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_bool] called with non-boolean value.")
       | Coq_prim_bool b -> k s b
       | Coq_prim_number n ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_bool] called with non-boolean value.")
       | Coq_prim_string s0 ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_bool] called with non-boolean value."))
    | Coq_value_object o ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_bool] called with non-boolean value."))

(** val if_object :
    result -> (state -> object_loc -> 'a1 specres) -> 'a1 specres **)

let if_object w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_prim p ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_object] called on a primitive.")
    | Coq_value_object l -> k s l)

(** val if_string :
    result -> (state -> string -> 'a1 specres) -> 'a1 specres **)

let if_string w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_prim p ->
      (match p with
       | Coq_prim_undef ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_string] called on a non-string value.")
       | Coq_prim_null ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_string] called on a non-string value.")
       | Coq_prim_bool b ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_string] called on a non-string value.")
       | Coq_prim_number n ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_string] called on a non-string value.")
       | Coq_prim_string s0 -> k s s0)
    | Coq_value_object o ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_string] called on a non-string value."))

(** val if_number :
    result -> (state -> number -> 'a1 specres) -> 'a1 specres **)

let if_number w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_prim p ->
      (match p with
       | Coq_prim_undef ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_number] called with non-number value.")
       | Coq_prim_null ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_number] called with non-number value.")
       | Coq_prim_bool b ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_number] called with non-number value.")
       | Coq_prim_number n -> k s n
       | Coq_prim_string s0 ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[if_number] called with non-number value."))
    | Coq_value_object o ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_number] called with non-number value."))

(** val if_prim : result -> (state -> prim -> 'a1 specres) -> 'a1 specres **)

let if_prim w k =
  if_value w (fun s v ->
    match v with
    | Coq_value_prim w0 -> k s w0
    | Coq_value_object o ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[if_primitive] called on an object."))

(** val convert_option_attributes :
    attributes option -> full_descriptor option **)

let convert_option_attributes =
  map (fun a -> Coq_full_descriptor_some a)

(** val if_abort : out -> (unit -> 'a1 resultof) -> 'a1 resultof **)

let if_abort o k =
  match o with
  | Coq_out_div -> k ()
  | Coq_out_ter (s0, r) ->
    if restype_comparable r.res_type Coq_restype_normal
    then (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s0
           ("[if_abort] received a normal result!")
    else k ()

(** val if_spec :
    'a1 specres -> (state -> 'a1 -> 'a2 specres) -> 'a2 specres **)

let if_spec w k =
  if_result_some w (fun sp ->
    match sp with
    | Coq_specret_val (s0, a) -> k s0 a
    | Coq_specret_out o -> if_abort o (fun x -> res_out o))

