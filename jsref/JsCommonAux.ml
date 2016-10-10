open Datatypes
open JsCommon
(*open JsNumber*)
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open Shared

let __ = ()

(** val if_some_then_same_dec :
    'a1 option -> 'a1 option -> ('a1 -> 'a1 -> bool) ->
    bool **)

let if_some_then_same_dec x y d =
  match x with
  | Some a ->
    (match y with
     | Some a0 -> d a0 a
     | None -> true)
  | None ->
    (match y with
     | Some a -> false
     | None -> true)

(** val same_value_dec : value -> value -> bool **)

let same_value_dec v1 v2 =
   let h0 = not (type_compare (type_of v1) (type_of v2)) in
  (if h0
   then (fun _ -> false)
   else (fun _ ->
          let t = type_of v1 in
          (match t with
           | Coq_type_undef -> (fun _ _ -> true)
           | Coq_type_null -> (fun _ _ -> true)
           | Coq_type_bool -> (fun _ _ -> value_compare v1 v2)
           | Coq_type_number ->
             (fun _ _ ->
               let h2 =
                     (value_compare v1 (Coq_value_prim (Coq_prim_number JsNumber.nan)))
                  && (value_compare v2 (Coq_value_prim (Coq_prim_number JsNumber.nan)))
               in
               (if h2
                then (fun _ -> true)
                else (fun _ ->
                       let h3 =
                           (value_compare v1 (Coq_value_prim (Coq_prim_number JsNumber.zero)))
                        && (value_compare v2 (Coq_value_prim (Coq_prim_number JsNumber.neg_zero)))
                       in
                       (if h3
                        then (fun _ -> false)
                        else (fun _ ->
                               let h4 =
                                   (value_compare v1 (Coq_value_prim
                                     (Coq_prim_number JsNumber.neg_zero)))
                                 &&
                                   (value_compare v2 (Coq_value_prim
                                     (Coq_prim_number JsNumber.zero)))
                               in
                               (if h4
                                then (fun _ -> false)
                                else (fun _ -> value_compare v1 v2)) __))
                         __)) __)
           | Coq_type_string -> (fun _ _ -> value_compare v1 v2)
           | Coq_type_object -> (fun _ _ -> value_compare v1 v2)) __ __))
    __

(** val attributes_data_compare :
    attributes_data -> attributes_data -> bool **)

let attributes_data_compare ad1 ad2 =
  let { attributes_data_value = v1; attributes_data_writable = w1;
    attributes_data_enumerable = e1; attributes_data_configurable = c1 } =
    ad1
  in
  let { attributes_data_value = v2; attributes_data_writable = w2;
    attributes_data_enumerable = e2; attributes_data_configurable = c2 } =
    ad2
  in
     (value_compare v1 v2)
  && (w1 === w2)
  && (e1 === e2)
  && (c1 === c2)

(** val attributes_accessor_compare :
    attributes_accessor -> attributes_accessor -> bool **)

let attributes_accessor_compare aa1 aa2 =
  let { attributes_accessor_get = v1; attributes_accessor_set = w1;
    attributes_accessor_enumerable = e1; attributes_accessor_configurable =
    c1 } = aa1
  in
  let { attributes_accessor_get = v2; attributes_accessor_set = w2;
    attributes_accessor_enumerable = e2; attributes_accessor_configurable =
    c2 } = aa2
  in
     (value_compare v1 v2)
  && (value_compare w1 w2)
  && (bool_eq e1 e2)
  && (bool_eq c1 c2)

(** val attributes_compare : attributes -> attributes -> bool **)

let attributes_compare a1 a2 =
  match a1 with
  | Coq_attributes_data_of ad1 ->
    (match a2 with
     | Coq_attributes_data_of ad2 -> attributes_data_compare ad1 ad2
     | Coq_attributes_accessor_of a -> false)
  | Coq_attributes_accessor_of aa1 ->
    (match a2 with
     | Coq_attributes_data_of a -> false
     | Coq_attributes_accessor_of aa2 ->
       attributes_accessor_compare aa1 aa2)

(** val full_descriptor_compare :
    full_descriptor -> full_descriptor -> bool **)

let full_descriptor_compare an1 an2 =
  match an1 with
  | Coq_full_descriptor_undef ->
    (match an2 with
     | Coq_full_descriptor_undef -> true
     | Coq_full_descriptor_some a -> false)
  | Coq_full_descriptor_some a1 ->
    (match an2 with
     | Coq_full_descriptor_undef -> false
     | Coq_full_descriptor_some a2 -> attributes_compare a1 a2)

(** val ref_kind_comparable : ref_kind coq_Comparable **)

let ref_kind_comparable x y =
  match x with
  | Coq_ref_kind_null ->
    (match y with
     | Coq_ref_kind_null -> true
     | Coq_ref_kind_undef -> false
     | Coq_ref_kind_primitive_base -> false
     | Coq_ref_kind_object -> false
     | Coq_ref_kind_env_record -> false)
  | Coq_ref_kind_undef ->
    (match y with
     | Coq_ref_kind_null -> false
     | Coq_ref_kind_undef -> true
     | Coq_ref_kind_primitive_base -> false
     | Coq_ref_kind_object -> false
     | Coq_ref_kind_env_record -> false)
  | Coq_ref_kind_primitive_base ->
    (match y with
     | Coq_ref_kind_null -> false
     | Coq_ref_kind_undef -> false
     | Coq_ref_kind_primitive_base -> true
     | Coq_ref_kind_object -> false
     | Coq_ref_kind_env_record -> false)
  | Coq_ref_kind_object ->
    (match y with
     | Coq_ref_kind_null -> false
     | Coq_ref_kind_undef -> false
     | Coq_ref_kind_primitive_base -> false
     | Coq_ref_kind_object -> true
     | Coq_ref_kind_env_record -> false)
  | Coq_ref_kind_env_record ->
    (match y with
     | Coq_ref_kind_null -> false
     | Coq_ref_kind_undef -> false
     | Coq_ref_kind_primitive_base -> false
     | Coq_ref_kind_object -> false
     | Coq_ref_kind_env_record -> true)

(** val object_binds_pickable_option :
    state -> object_loc -> coq_object coq_Pickable_option **)

(* STATEFUL-RO *)
let object_binds_pickable_option s l =
  HeapObj.read_option s.state_object_heap l

(** val env_record_binds_pickable_option :
    state -> env_loc -> env_record coq_Pickable_option **)

(* STATEFUL-RO *)
let env_record_binds_pickable_option s l =
  HeapInt.read_option s.state_env_record_heap l

(** val decl_env_record_pickable_option :
    decl_env_record -> prop_name -> (mutability * value) coq_Pickable_option **)

let decl_env_record_pickable_option ed x =
  HeapStr.read_option ed x

(** val descriptor_is_data_dec : descriptor -> bool **)

let descriptor_is_data_dec desc =
  not
    (  (option_compare value_compare desc.descriptor_value None)
    && (option_compare bool_eq desc.descriptor_writable None))

(** val descriptor_is_accessor_dec : descriptor -> bool **)

let descriptor_is_accessor_dec desc =
  not
    (   (option_compare value_compare desc.descriptor_get None)
     && (option_compare value_compare desc.descriptor_set None))

(** val descriptor_is_generic_dec : descriptor -> bool **)

let descriptor_is_generic_dec desc =
     (not (descriptor_is_data_dec desc))
  && (not (descriptor_is_accessor_dec desc))

(** val prepost_unary_op_dec : unary_op -> bool **)

let prepost_unary_op_dec op = match op with
| Coq_unary_op_delete -> false
| Coq_unary_op_void -> false
| Coq_unary_op_typeof -> false
| Coq_unary_op_post_incr -> true
| Coq_unary_op_post_decr -> true
| Coq_unary_op_pre_incr -> true
| Coq_unary_op_pre_decr -> true
| Coq_unary_op_add -> false
| Coq_unary_op_neg -> false
| Coq_unary_op_bitwise_not -> false
| Coq_unary_op_not -> false

(** val attributes_is_data_dec : attributes -> bool **)

let attributes_is_data_dec a = match a with
| Coq_attributes_data_of a0 -> true
| Coq_attributes_accessor_of a0 -> false

(** val run_object_heap_map_properties :
    state -> object_loc -> (object_properties_type -> object_properties_type)
    -> state option **)

(* STATEFUL -- idea is to have this one imperative *)
let run_object_heap_map_properties s l f =
  map (fun o -> object_write s l (object_map_properties o f))
    (object_binds_pickable_option s l)

(** val object_heap_map_properties_pickable_option :
    state -> object_loc -> (object_properties_type -> object_properties_type)
    -> state coq_Pickable_option **)

(* STATEFUL -- eliminate by inlining *)
let object_heap_map_properties_pickable_option s l f =
  run_object_heap_map_properties s l f

(** val descriptor_contains_dec :
    descriptor -> descriptor -> bool **)

let descriptor_contains_dec desc1 desc2 =
  let { descriptor_value = descriptor_value0; descriptor_writable =
    descriptor_writable0; descriptor_get = descriptor_get0; descriptor_set =
    descriptor_set0; descriptor_enumerable = descriptor_enumerable0;
    descriptor_configurable = descriptor_configurable0 } = desc1
  in
  let { descriptor_value = descriptor_value1; descriptor_writable =
    descriptor_writable1; descriptor_get = descriptor_get1; descriptor_set =
    descriptor_set1; descriptor_enumerable = descriptor_enumerable1;
    descriptor_configurable = descriptor_configurable1 } = desc2
  in
     (if_some_then_same_dec descriptor_value0 descriptor_value1 (fun u v -> same_value_dec u v))
  && (if_some_then_same_dec descriptor_writable0 descriptor_writable1 (fun u v -> bool_eq u v))
  && (if_some_then_same_dec descriptor_get0 descriptor_get1 (fun u v -> same_value_dec u v))
  && (if_some_then_same_dec descriptor_set0 descriptor_set1 (fun u v -> same_value_dec u v))
  && (if_some_then_same_dec descriptor_enumerable0 descriptor_enumerable1 (fun u v -> bool_eq u v))
  && (if_some_then_same_dec descriptor_configurable0 descriptor_configurable1 (fun u v -> bool_eq u v))

(** val descriptor_enumerable_not_same_dec :
    attributes -> descriptor -> bool **)

let descriptor_enumerable_not_same_dec a desc =
  let o = desc.descriptor_enumerable in
  (match o with
   | Some b -> not (bool_eq b (attributes_enumerable a))
   | None -> false)

(** val descriptor_value_not_same_dec :
    attributes_data -> descriptor -> bool **)

let descriptor_value_not_same_dec ad desc =
  let o = desc.descriptor_value in
  (match o with
   | Some v -> not (same_value_dec v ad.attributes_data_value)
   | None -> false)

(** val descriptor_get_not_same_dec :
    attributes_accessor -> descriptor -> bool **)

let descriptor_get_not_same_dec aa desc =
  let o = desc.descriptor_get in
  (match o with
   | Some v -> not (same_value_dec v aa.attributes_accessor_get)
   | None -> false)

(** val descriptor_set_not_same_dec :
    attributes_accessor -> descriptor -> bool **)

let descriptor_set_not_same_dec aa desc =
  let o = desc.descriptor_set in
  (match o with
   | Some v -> not (same_value_dec v aa.attributes_accessor_set)
   | None -> false)

(** val attributes_change_enumerable_on_non_configurable_dec :
    attributes -> descriptor -> bool **)

let attributes_change_enumerable_on_non_configurable_dec a desc =
    (not (attributes_configurable a))
  &&
    ((option_compare bool_eq desc.descriptor_configurable (Some
        true)) 
     || (descriptor_enumerable_not_same_dec a desc))

(** val attributes_change_data_on_non_configurable_dec :
    attributes_data -> descriptor -> bool **)

let attributes_change_data_on_non_configurable_dec ad desc =
    (not (attributes_configurable (Coq_attributes_data_of ad)))
  &&
    (not ad.attributes_data_writable)
  &&  (   (option_compare bool_eq desc.descriptor_writable (Some true)) 
       || (descriptor_value_not_same_dec ad desc))

(** val attributes_change_accessor_on_non_configurable_dec :
    attributes_accessor -> descriptor -> bool **)

let attributes_change_accessor_on_non_configurable_dec aa desc =
     (not (attributes_configurable (Coq_attributes_accessor_of aa)))
  && (    (descriptor_get_not_same_dec aa desc)
       || (descriptor_set_not_same_dec aa desc))

(** val run_function_get_error_case : state -> prop_name -> value -> bool **)

(* STATEFUL-RO *)
let run_function_get_error_case s x v =
match v with
| Coq_value_prim w -> false
| Coq_value_object l ->
    (* In strict mode, cannot call "caller" *)
    (if string_eq x ("caller")
     then true
     else false)
  &&
    (option_case false (fun o ->
      option_case false (fun bd -> funcbody_is_strict bd) o.object_code_)
      (object_binds_pickable_option s l))

(** val spec_function_get_error_case_dec :
    state -> prop_name -> value -> bool **)

(* STATEFUL-RO *)
let spec_function_get_error_case_dec s x v =
  run_function_get_error_case s x v

(** val run_callable : state -> value -> call option option **)

(* STATEFUL-RO *)
let run_callable s v = 
match v with
| Coq_value_prim w -> Some None
| Coq_value_object l ->
  option_case None (fun o -> Some o.object_call_)
    (object_binds_pickable_option s l)

(** val is_callable_dec : state -> value -> bool **)

(* STATEFUL-RO *)
let is_callable_dec s v =
  option_case false (fun o -> option_case false (fun x -> true) o) (run_callable s v)

(** val object_properties_keys_as_list_pickable_option :
    state -> object_loc -> prop_name list coq_Pickable_option **)

(* STATEFUL-RO *)
let object_properties_keys_as_list_pickable_option s l =
  map (fun props -> LibList.map fst (HeapStr.to_list props))
    (map object_properties_ (object_binds_pickable_option s l))

