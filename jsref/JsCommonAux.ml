open Datatypes
open JsCommon
(*open JsNumber*)
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open Shared

(** If x is Some value, then y has same value, according to comparison d.
    (x = Some a) implies (y = Some b) and (d a b) *)
let if_some_then_same x y d =
  match x with
  | None -> true (* <<-- if_some *)
  | Some a ->
    (match y with
     | Some b -> d a b
     | None -> false)


(* FIXME: To be replaced with same_value *)
let same_value_dec v1 v2 = failwith "FIXME: To be replaced with same_value"

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
     | Coq_full_descriptor_some _ -> false)
  | Coq_full_descriptor_some a1 ->
    (match an2 with
     | Coq_full_descriptor_some a2 -> attributes_compare a1 a2
     | Coq_full_descriptor_undef -> false)

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

(** Fetches Some object from location l in the heap, None if it is not allocated *)
(* STATEFUL-RO *)
let object_binds_option s l =
  HeapObj.read_option s.state_object_heap l

(** val env_record_binds_option :
    state -> env_loc -> env_record option **)

(* STATEFUL-RO *)
let env_record_binds_option s l =
  HeapInt.read_option s.state_env_record_heap l

(** val decl_env_record_option :
    decl_env_record -> prop_name -> (mutability * value) option **)

let decl_env_record_option ed x =
  HeapStr.read_option ed x

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

(** Unpacks a the descriptor_undef type to a descriptor.

    The spec assumes this step automatically after testing that a
    descriptor is not undefined, or through the IsGenericDescriptor
    and IsDataDescriptor tests. *)
let descriptor_get_defined desc =
  match desc with
  | Descriptor_undef -> failwith "Pre-checked safe type conversion failed"
  | Descriptor d -> d

(** Function to test if d0 is contained within d1.
    Implements the spec text:
    "true, if every field in d0 also occurs in d1 and the value of every field in d0 is the
    same value as the corresponding field in d1 when compared using the sv algorithm."
*)
let descriptor_contained_by d0 d1 sv =
     (if_some_then_same d0.descriptor_writable     d1.descriptor_writable     bool_eq)
  && (if_some_then_same d0.descriptor_enumerable   d1.descriptor_enumerable   bool_eq)
  && (if_some_then_same d0.descriptor_configurable d1.descriptor_configurable bool_eq)
  && (if_some_then_same d0.descriptor_value        d1.descriptor_value        sv)
  && (if_some_then_same d0.descriptor_get          d1.descriptor_get          sv)
  && (if_some_then_same d0.descriptor_set          d1.descriptor_set          sv)

(** Implements spec text: "true, if every field in d is absent" *)
let descriptor_is_empty d =
  descriptor_contained_by d descriptor_intro_empty (fun v1 v2 -> false)

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
| Coq_value_object l ->
    (* In strict mode, cannot call "caller" *)
    (if string_eq x ("caller")
     then true
     else false)
  &&
    (option_case false (fun o ->
      option_case false (fun bd -> funcbody_is_strict bd) o.object_code_)
      (object_binds_option s l))
| _ -> false

(** val spec_function_get_error_case_dec :
    state -> prop_name -> value -> bool **)

(* STATEFUL-RO *)
let spec_function_get_error_case_dec s x v =
  run_function_get_error_case s x v

(** val run_callable : state -> value -> call option option **)

(* STATEFUL-RO *)
let run_callable s v = 
match v with
| Coq_value_object l ->
  option_case None (fun o -> Some o.object_call_)
    (object_binds_option s l)
| _ -> Some None

(** val is_callable_dec : state -> value -> bool **)

(* STATEFUL-RO *)
let is_callable_dec s v =
  option_case false (fun o -> option_case false (fun x -> true) o) (run_callable s v)

(** val object_properties_keys_as_list_option :
    state -> object_loc -> prop_name list option **)

(* STATEFUL-RO *)
let object_properties_keys_as_list_option s l =
  map (fun props -> LibList.map fst (HeapStr.to_list props))
    (map object_properties_ (object_binds_option s l))

(** Updates the properties Heap of a given object using f *)
(* STATEFUL -- idea is to have this one imperative *)
let run_object_heap_map_properties s l f =
  map (fun o -> object_write s l (object_map_properties o f)) (object_binds_option s l)

(** Given a properties Heap, a property name, updates the given property attributes using function f *)
let properties_map_property p x f =
  HeapStr.write p x (f (HeapStr.read p x))

(** Update an object's property given the location, property name and new attributes value *)
(* STATEFUL *)
let object_set_property s l x a =
  run_object_heap_map_properties s l (fun p -> HeapStr.write p x a)

(** Updates an object's property given the location, property name and function mapping old to new attributes *)
let object_map_property s l x f =
  run_object_heap_map_properties s l (fun p -> properties_map_property p x f)

let object_retrieve_property s l x =
  let so = object_binds_option s l in
  match map object_properties_ so with
  | None -> None
  | Some p -> HeapStr.read_option p x

let object_property_exists s l x =
  is_some (object_retrieve_property s l x)

(** Fetches a given object slot value (using proj) from the object l in state s
    FIXME: The name is very confusing. *)
let run_object_method proj s l =
  LibOption.map proj (object_binds_option s l)

(** val run_object_heap_set :
    (coq_object -> a' -> coq_object) -> state -> object_loc -> a' -> state option **)
(** Updates an object's internal field with the given update function [prj].
    (Update functions are defined in JsSyntaxAux) *)

let run_object_set_internal prj s l v =
  LibOption.map (fun o -> object_write s l (prj o v)) (object_binds_option s l)

