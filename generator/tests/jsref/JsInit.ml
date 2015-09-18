open JsCommon
open JsNumber
open JsPreliminary
open JsSyntax
open JsSyntaxAux
open LibInt
open LibStream

(** val prop_attributes_for_global_object : value -> attributes_data **)

let prop_attributes_for_global_object v =
  { attributes_data_value = v; attributes_data_writable = true;
    attributes_data_enumerable = false; attributes_data_configurable = true }

(** val attrib_constant : value -> attributes_data **)

let attrib_constant v =
  { attributes_data_value = v; attributes_data_writable = false;
    attributes_data_enumerable = false; attributes_data_configurable =
    false }

(** val object_create_builtin :
    value -> class_name -> object_properties_type -> coq_object **)

let object_create_builtin vproto sclass p =
  object_create vproto sclass true p

(** val object_create_prealloc_call_or_construct :
    value -> object_properties_type -> coq_object **)

let object_create_prealloc_call_or_construct length p =
  let sclass = "Function" in
  let p' =
    Heap.write p ("length")
      (Coq_attributes_data_of (attrib_constant length))
  in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_function_proto)) sclass p'

(** val object_create_prealloc_call :
    prealloc -> value -> object_properties_type -> coq_object **)

let object_create_prealloc_call fprealloc length p =
  let o = object_create_prealloc_call_or_construct length p in
  object_with_invokation o None (Some (Coq_call_prealloc fprealloc)) None

(** val object_create_prealloc_constructor :
    prealloc -> value -> object_properties_type -> coq_object **)

let object_create_prealloc_constructor fprealloc length p =
  let o = object_create_prealloc_call_or_construct length p in
  object_with_invokation o (Some (Coq_construct_prealloc fprealloc)) (Some
    (Coq_call_prealloc fprealloc)) (Some Coq_builtin_has_instance_function)

(** val write_native :
    object_properties_type -> prop_name -> value -> (prop_name, attributes)
    Heap.heap **)

let write_native p name v =
  Heap.write p name (Coq_attributes_data_of
    (prop_attributes_for_global_object v))

(** val write_constant :
    object_properties_type -> prop_name -> value -> (prop_name, attributes)
    Heap.heap **)

let write_constant p name value0 =
  Heap.write p name (Coq_attributes_data_of (attrib_constant value0))

(** val object_prealloc_global_proto : value **)

let object_prealloc_global_proto = (Coq_value_prim Coq_prim_null)

(** val object_prealloc_global_class : char list **)

let object_prealloc_global_class = (
  let rec aux s = function
  | 0 -> []
  | n -> let n' = n - 1 in
    s.[n'] :: aux s n'
  in let aux2 s =
    List.rev (aux s (String.length s))
  in aux2 "GlobalClass")

(** val object_prealloc_global_properties :
    (prop_name, attributes) Heap.heap **)

let object_prealloc_global_properties =
  let p =
    write_constant Heap.empty ("NaN") (Coq_value_prim
      (Coq_prim_number nan))
  in
  let p0 =
    write_constant p
      ("Infinity")
      (Coq_value_prim (Coq_prim_number infinity))
  in
  let p1 =
    write_constant p0
      ("undefined")
      (Coq_value_prim Coq_prim_undef)
  in
  let p2 =
    write_native p1 ("eval") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_global_eval))
  in
  let p3 =
    write_native p2
      ("parseInt")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_parse_int))
  in
  let p4 =
    write_native p3
      ("parseFloat")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_parse_float))
  in
  let p5 =
    write_native p4 ("isNaN") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_global_is_nan))
  in
  let p6 =
    write_native p5
      ("isFinite")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_is_finite))
  in
  let p7 =
    write_native p6
      ("decodeURI")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_decode_uri))
  in
  let p8 =
    write_native p7
      ("decodeURIComponent")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_decode_uri_component))
  in
  let p9 =
    write_native p8
      ("encodeURI")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_encode_uri))
  in
  let p10 =
    write_native p9
      ("encodeURIComponent")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_global_encode_uri_component))
  in
  let p11 =
    write_native p10 ("Object")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object))
  in
  let p12 =
    write_native p11
      ("Function")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function))
  in
  let p13 =
    write_native p12 ("Array") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_array))
  in
  let p14 =
    write_native p13 ("String")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string))
  in
  let p15 =
    write_native p14 ("Boolean")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool))
  in
  let p16 =
    write_native p15 ("Number")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number))
  in
  let p17 =
    write_native p16 ("Math") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_math))
  in
  let p18 =
    write_native p17 ("Date") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_date))
  in
  let p19 =
    write_native p18 ("RegExp")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_regexp))
  in
  let p20 =
    write_native p19 ("Error") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_error))
  in
  let p21 =
    write_native p20
      ("EvalError")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_eval)))
  in
  let p22 =
    write_native p21
      ("RangeError")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_range)))
  in
  let p23 =
    write_native p22
      ("ReferenceError")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_ref)))
  in
  let p24 =
    write_native p23
      ("SyntaxError")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_syntax)))
  in
  let p25 =
    write_native p24
      ("TypeError")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_type)))
  in
  let p26 =
    write_native p25
      ("URIError")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_uri)))
  in
  write_native p26 ("JSON") (Coq_value_object
    (Coq_object_loc_prealloc Coq_prealloc_json))

(** val object_prealloc_global : coq_object **)

let object_prealloc_global =
  object_create_builtin object_prealloc_global_proto
    object_prealloc_global_class object_prealloc_global_properties

(** val global_eval_function_object : coq_object **)

let global_eval_function_object =
  object_create_prealloc_call Coq_prealloc_global_eval (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_parse_int_function_object : coq_object **)

let global_parse_int_function_object =
  object_create_prealloc_call Coq_prealloc_global_parse_int (Coq_value_prim
    (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ (Pervasives.succ 0)))))) Heap.empty

(** val global_parse_float_function_object : coq_object **)

let global_parse_float_function_object =
  object_create_prealloc_call Coq_prealloc_global_parse_float (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_is_nan_function_object : coq_object **)

let global_is_nan_function_object =
  object_create_prealloc_call Coq_prealloc_global_is_nan (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_is_finite_function_object : coq_object **)

let global_is_finite_function_object =
  object_create_prealloc_call Coq_prealloc_global_is_finite (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_decode_uri_function_object : coq_object **)

let global_decode_uri_function_object =
  object_create_prealloc_call Coq_prealloc_global_decode_uri (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_decode_uri_component_function_object : coq_object **)

let global_decode_uri_component_function_object =
  object_create_prealloc_call Coq_prealloc_global_decode_uri_component
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_encode_uri_function_object : coq_object **)

let global_encode_uri_function_object =
  object_create_prealloc_call Coq_prealloc_global_encode_uri (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val global_encode_uri_component_function_object : coq_object **)

let global_encode_uri_component_function_object =
  object_create_prealloc_call Coq_prealloc_global_encode_uri_component
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_object : coq_object **)

let object_prealloc_object =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto))
  in
  let p0 =
    write_native p
      ("getPrototypeOf")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_get_proto_of))
  in
  let p1 =
    write_native p0
      ("getOwnPropertyDescriptor")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_get_own_prop_descriptor))
  in
  let p2 =
    write_native p1
      ("getOwnPropertyNames")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_get_own_prop_name))
  in
  let p3 =
    write_native p2 ("create")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_create))
  in
  let p4 =
    write_native p3
      ("defineProperty")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_define_prop))
  in
  let p5 =
    write_native p4
      ("defineProperties")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_define_props))
  in
  let p6 =
    write_native p5 ("seal") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_object_seal))
  in
  let p7 =
    write_native p6 ("freeze")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_freeze))
  in
  let p8 =
    write_native p7
      ("preventExtensions")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_prevent_extensions))
  in
  let p9 =
    write_native p8
      ("isSealed")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_is_sealed))
  in
  let p10 =
    write_native p9
      ("isFrozen")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_is_frozen))
  in
  let p11 =
    write_native p10
      ("isExtensible")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_is_extensible))
  in
  object_create_prealloc_constructor Coq_prealloc_object (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p11

(** val object_get_proto_of_function_object : coq_object **)

let object_get_proto_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_get_proto_of
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_get_own_prop_descriptor_function_object : coq_object **)

let object_get_own_prop_descriptor_function_object =
  object_create_prealloc_call Coq_prealloc_object_get_own_prop_descriptor
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_get_own_prop_name_function_object : coq_object **)

let object_get_own_prop_name_function_object =
  object_create_prealloc_call Coq_prealloc_object_get_own_prop_name
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_create_function_object : coq_object **)

let object_create_function_object =
  object_create_prealloc_call Coq_prealloc_object_create (Coq_value_prim
    (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ (Pervasives.succ 0)))))) Heap.empty

(** val object_define_prop_function_object : coq_object **)

let object_define_prop_function_object =
  object_create_prealloc_call Coq_prealloc_object_define_prop (Coq_value_prim
    (Coq_prim_number
    (of_int
      (my_Z_of_nat (Pervasives.succ (Pervasives.succ (Pervasives.succ 0)))))))
    Heap.empty

(** val object_define_props_function_object : coq_object **)

let object_define_props_function_object =
  object_create_prealloc_call Coq_prealloc_object_define_props
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ (Pervasives.succ 0)))))) Heap.empty

(** val object_seal_function_object : coq_object **)

let object_seal_function_object =
  object_create_prealloc_call Coq_prealloc_object_seal (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_freeze_function_object : coq_object **)

let object_freeze_function_object =
  object_create_prealloc_call Coq_prealloc_object_freeze (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prevent_extensions_function_object : coq_object **)

let object_prevent_extensions_function_object =
  object_create_prealloc_call Coq_prealloc_object_prevent_extensions
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_is_sealed_function_object : coq_object **)

let object_is_sealed_function_object =
  object_create_prealloc_call Coq_prealloc_object_is_sealed (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_is_frozen_function_object : coq_object **)

let object_is_frozen_function_object =
  object_create_prealloc_call Coq_prealloc_object_is_frozen (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_is_extensible_function_object : coq_object **)

let object_is_extensible_function_object =
  object_create_prealloc_call Coq_prealloc_object_is_extensible
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_object_proto : coq_object **)

let object_prealloc_object_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object))
  in
  let p0 =
    write_native p ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_to_string))
  in
  let p1 =
    write_native p0 ("valueOf")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_value_of))
  in
  let p2 =
    write_native p1
      ("hasOwnProperty")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_has_own_prop))
  in
  let p3 =
    write_native p2
      ("isPrototypeOf")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_is_prototype_of))
  in
  let p4 =
    write_native p3
      ("propertyIsEnumerable")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_prop_is_enumerable))
  in
  object_create_builtin (Coq_value_prim Coq_prim_null)
    ("Object") p4

(** val object_proto_to_string_function_object : coq_object **)

let object_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_proto_value_of_function_object : coq_object **)

let object_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_value_of
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_proto_has_own_prop_function_object : coq_object **)

let object_proto_has_own_prop_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_has_own_prop
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_proto_is_prototype_of_function_object : coq_object **)

let object_proto_is_prototype_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_is_prototype_of
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_proto_prop_is_enumerable_function_object : coq_object **)

let object_proto_prop_is_enumerable_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_prop_is_enumerable
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_function : coq_object **)

let object_prealloc_function =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_function_proto))
  in
  object_create_prealloc_constructor Coq_prealloc_function (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p

(** val object_prealloc_function_proto : coq_object **)

let object_prealloc_function_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function))
  in
  let p0 =
    Heap.write p ("length")
      (Coq_attributes_data_of
      (attrib_constant (Coq_value_prim (Coq_prim_number
        (of_int (my_Z_of_nat 0))))))
  in
  let p1 =
    write_native p0
      ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_function_proto_to_string))
  in
  let p2 =
    write_native p1 ("apply") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_function_proto_apply))
  in
  let p3 =
    write_native p2 ("call") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_function_proto_call))
  in
  let p4 =
    write_native p3 ("bind") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_function_proto_bind))
  in
  let o =
    object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto))
      ("Function") p4
  in
  object_with_invokation o None (Some (Coq_call_prealloc
    Coq_prealloc_function_proto)) None

(** val function_proto_to_string_function_object : coq_object **)

let function_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val function_proto_call_function_object : coq_object **)

let function_proto_call_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_call
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val function_proto_bind_function_object : coq_object **)

let function_proto_bind_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_bind
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val function_proto_apply_function_object : coq_object **)

let function_proto_apply_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_apply
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ (Pervasives.succ 0)))))) Heap.empty

(** val object_prealloc_number : coq_object **)

let object_prealloc_number =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto))
  in
  let p0 =
    write_constant p ("NaN") (Coq_value_prim (Coq_prim_number
      nan))
  in
  let p1 =
    write_constant p0
      ("NEGATIVE_INFINITY")
      (Coq_value_prim (Coq_prim_number neg_infinity))
  in
  let p2 =
    write_constant p1
      ("POSITIVE_INFINITY")
      (Coq_value_prim (Coq_prim_number infinity))
  in
  let p3 =
    write_constant p2
      ("MAX_VALUE")
      (Coq_value_prim (Coq_prim_number max_value))
  in
  let p4 =
    write_constant p3
      ("MIN_VALUE")
      (Coq_value_prim (Coq_prim_number min_value))
  in
  object_create_prealloc_constructor Coq_prealloc_number (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p4

(** val object_prealloc_number_proto : coq_object **)

let object_prealloc_number_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number))
  in
  let p0 =
    write_native p ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_number_proto_to_string))
  in
  let p1 =
    write_native p0 ("valueOf")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_number_proto_value_of))
  in
  let o =
    object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto))
      ("Number") p1
  in
  object_with_primitive_value o (Coq_value_prim (Coq_prim_number zero))

(** val number_proto_to_string_function_object : coq_object **)

let number_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_number_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val number_proto_value_of_function_object : coq_object **)

let number_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_number_proto_value_of
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_prealloc_array : coq_object **)

let object_prealloc_array =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto))
  in
  let p0 =
    write_native p ("isArray")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_array_is_array))
  in
  let p1 =
    write_constant p0 ("length")
      (Coq_value_prim (Coq_prim_number
      (of_int (my_Z_of_nat (Pervasives.succ 0)))))
  in
  object_create_prealloc_constructor Coq_prealloc_array (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p1

(** val array_is_array_function_object : coq_object **)

let array_is_array_function_object =
  object_create_prealloc_call Coq_prealloc_array_is_array (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_array_proto : coq_object **)

let object_prealloc_array_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array))
  in
  let p0 =
    write_native p ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_array_proto_to_string))
  in
  let p1 =
    write_native p0 ("join") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_array_proto_join))
  in
  let p2 =
    write_native p1 ("pop") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_array_proto_pop))
  in
  let p3 =
    write_native p2 ("push") (Coq_value_object
      (Coq_object_loc_prealloc Coq_prealloc_array_proto_push))
  in
  let p4 =
    write_constant p3 ("length")
      (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0))))
  in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_object_proto)) ("Array") p4

(** val array_proto_pop_function_object : coq_object **)

let array_proto_pop_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_pop (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val array_proto_push_function_object : coq_object **)

let array_proto_push_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_push (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val array_proto_to_string_function_object : coq_object **)

let array_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val array_proto_join_function_object : coq_object **)

let array_proto_join_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_join (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_string : coq_object **)

let object_prealloc_string =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string_proto))
  in
  object_create_prealloc_constructor Coq_prealloc_string (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p

(** val object_prealloc_string_proto : coq_object **)

let object_prealloc_string_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string))
  in
  let p0 =
    write_native p ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_string_proto_to_string))
  in
  let p1 =
    write_native p0 ("valueOf")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_string_proto_value_of))
  in
  let o =
    object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto))
      ("String") p1
  in
  object_with_primitive_value o (Coq_value_prim (Coq_prim_string []))

(** val string_proto_to_string_function_object : coq_object **)

let string_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_string_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val string_proto_value_of_function_object : coq_object **)

let string_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_string_proto_value_of
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_prealloc_bool : coq_object **)

let object_prealloc_bool =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto))
  in
  object_create_prealloc_constructor Coq_prealloc_bool (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p

(** val object_prealloc_bool_proto : coq_object **)

let object_prealloc_bool_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool))
  in
  let p0 =
    write_native p ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_bool_proto_to_string))
  in
  let p1 =
    write_native p0 ("valueOf")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_bool_proto_value_of))
  in
  let o =
    object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto))
      ("Boolean") p1
  in
  object_with_primitive_value o (Coq_value_prim (Coq_prim_bool false))

(** val bool_proto_to_string_function_object : coq_object **)

let bool_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_bool_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val bool_proto_value_of_function_object : coq_object **)

let bool_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_bool_proto_value_of
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_prealloc_math : coq_object **)

let object_prealloc_math =
  let p =
    write_constant Heap.empty ("PI") (Coq_value_prim
      (Coq_prim_number pi))
  in
  let p0 = write_constant p ("E") (Coq_value_prim (Coq_prim_number e)) in
  let p1 =
    write_constant p0 ("LN2") (Coq_value_prim
      (Coq_prim_number ln2))
  in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_object_proto)) ("Math") p1

(** val object_prealloc_date : coq_object **)

let object_prealloc_date =
  object_create_prealloc_constructor Coq_prealloc_date (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_regexp : coq_object **)

let object_prealloc_regexp =
  object_create_prealloc_constructor Coq_prealloc_regexp (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) Heap.empty

(** val object_prealloc_error : coq_object **)

let object_prealloc_error =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error_proto))
  in
  object_create_prealloc_constructor Coq_prealloc_error (Coq_value_prim
    (Coq_prim_number (of_int (my_Z_of_nat (Pervasives.succ 0))))) p

(** val object_prealloc_error_proto : coq_object **)

let object_prealloc_error_proto =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error))
  in
  let p0 =
    write_native p ("name") (Coq_value_prim
      (Coq_prim_string ("Error")))
  in
  let p1 =
    write_native p0 ("message")
      (Coq_value_prim (Coq_prim_string []))
  in
  let p2 =
    write_native p1
      ("toString")
      (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_error_proto_to_string))
  in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_object_proto)) ("Error") p2

(** val error_proto_to_string_function_object : coq_object **)

let error_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_error_proto_to_string
    (Coq_value_prim (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty

(** val object_prealloc_native_error : native_error -> coq_object **)

let object_prealloc_native_error ne =
  let p =
    write_constant Heap.empty
      ("prototype")
      (Coq_value_object (Coq_object_loc_prealloc
      (Coq_prealloc_native_error_proto ne)))
  in
  object_create_prealloc_constructor (Coq_prealloc_native_error ne)
    (Coq_value_prim (Coq_prim_number
    (of_int (my_Z_of_nat (Pervasives.succ 0))))) p

(** val object_prealloc_native_error_proto : native_error -> coq_object **)

let object_prealloc_native_error_proto ne =
  let p =
    write_native Heap.empty
      ("constructor")
      (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error
      ne)))
  in
  let p0 =
    write_native p ("name") (Coq_value_prim
      (Coq_prim_string (string_of_native_error ne)))
  in
  let p1 =
    write_native p0 ("message")
      (Coq_value_prim (Coq_prim_string []))
  in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_error_proto)) ("Error") p1

(** val object_prealloc_json : coq_object **)

let object_prealloc_json =
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_object_proto)) ("JSON") Heap.empty

(** val throw_type_error_object : coq_object **)

let throw_type_error_object =
  let o =
    object_create_prealloc_call Coq_prealloc_throw_type_error (Coq_value_prim
      (Coq_prim_number (of_int (my_Z_of_nat 0)))) Heap.empty
  in
  let o0 = object_with_scope o (Some lexical_env_initial) in
  let o1 = object_with_formal_params o0 (Some []) in
  object_set_extensible o1 false

(** val object_heap_initial_function_objects_1 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_1 h =
  let h0 =
    Heap.write h (Coq_object_loc_prealloc Coq_prealloc_throw_type_error)
      throw_type_error_object
  in
  let h1 =
    Heap.write h0 (Coq_object_loc_prealloc Coq_prealloc_global_eval)
      global_eval_function_object
  in
  let h2 =
    Heap.write h1 (Coq_object_loc_prealloc Coq_prealloc_global_parse_int)
      global_parse_int_function_object
  in
  let h3 =
    Heap.write h2 (Coq_object_loc_prealloc Coq_prealloc_global_parse_float)
      global_parse_float_function_object
  in
  let h4 =
    Heap.write h3 (Coq_object_loc_prealloc Coq_prealloc_global_is_nan)
      global_is_nan_function_object
  in
  let h5 =
    Heap.write h4 (Coq_object_loc_prealloc Coq_prealloc_global_is_finite)
      global_is_finite_function_object
  in
  let h6 =
    Heap.write h5 (Coq_object_loc_prealloc Coq_prealloc_global_decode_uri)
      global_decode_uri_function_object
  in
  let h7 =
    Heap.write h6 (Coq_object_loc_prealloc
      Coq_prealloc_global_decode_uri_component)
      global_decode_uri_component_function_object
  in
  let h8 =
    Heap.write h7 (Coq_object_loc_prealloc Coq_prealloc_global_encode_uri)
      global_encode_uri_function_object
  in
  Heap.write h8 (Coq_object_loc_prealloc
    Coq_prealloc_global_encode_uri_component)
    global_encode_uri_component_function_object

(** val object_heap_initial_function_objects_2 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_2 h =
  let h0 = object_heap_initial_function_objects_1 h in
  let h1 =
    Heap.write h0 (Coq_object_loc_prealloc Coq_prealloc_object_get_proto_of)
      object_get_proto_of_function_object
  in
  let h2 =
    Heap.write h1 (Coq_object_loc_prealloc
      Coq_prealloc_object_get_own_prop_descriptor)
      object_get_own_prop_descriptor_function_object
  in
  let h3 =
    Heap.write h2 (Coq_object_loc_prealloc
      Coq_prealloc_object_get_own_prop_name)
      object_get_own_prop_name_function_object
  in
  let h4 =
    Heap.write h3 (Coq_object_loc_prealloc Coq_prealloc_object_create)
      object_create_function_object
  in
  let h5 =
    Heap.write h4 (Coq_object_loc_prealloc Coq_prealloc_object_define_prop)
      object_define_prop_function_object
  in
  let h6 =
    Heap.write h5 (Coq_object_loc_prealloc Coq_prealloc_object_define_props)
      object_define_props_function_object
  in
  let h7 =
    Heap.write h6 (Coq_object_loc_prealloc Coq_prealloc_object_seal)
      object_seal_function_object
  in
  let h8 =
    Heap.write h7 (Coq_object_loc_prealloc Coq_prealloc_object_freeze)
      object_freeze_function_object
  in
  let h9 =
    Heap.write h8 (Coq_object_loc_prealloc
      Coq_prealloc_object_prevent_extensions)
      object_prevent_extensions_function_object
  in
  let h10 =
    Heap.write h9 (Coq_object_loc_prealloc Coq_prealloc_object_is_sealed)
      object_is_sealed_function_object
  in
  let h11 =
    Heap.write h10 (Coq_object_loc_prealloc Coq_prealloc_object_is_frozen)
      object_is_frozen_function_object
  in
  Heap.write h11 (Coq_object_loc_prealloc Coq_prealloc_object_is_extensible)
    object_is_extensible_function_object

(** val object_heap_initial_function_objects_3 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_3 h =
  let h0 = object_heap_initial_function_objects_2 h in
  let h1 =
    Heap.write h0 (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_to_string)
      object_proto_to_string_function_object
  in
  let h2 =
    Heap.write h1 (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_value_of)
      object_proto_value_of_function_object
  in
  let h3 =
    Heap.write h2 (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_has_own_prop)
      object_proto_has_own_prop_function_object
  in
  let h4 =
    Heap.write h3 (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_is_prototype_of)
      object_proto_is_prototype_of_function_object
  in
  let h5 =
    Heap.write h4 (Coq_object_loc_prealloc
      Coq_prealloc_object_proto_prop_is_enumerable)
      object_proto_prop_is_enumerable_function_object
  in
  let h6 =
    Heap.write h5 (Coq_object_loc_prealloc
      Coq_prealloc_function_proto_to_string)
      function_proto_to_string_function_object
  in
  let h7 =
    Heap.write h6 (Coq_object_loc_prealloc Coq_prealloc_function_proto_call)
      function_proto_call_function_object
  in
  let h8 =
    Heap.write h7 (Coq_object_loc_prealloc Coq_prealloc_function_proto_bind)
      function_proto_bind_function_object
  in
  Heap.write h8 (Coq_object_loc_prealloc Coq_prealloc_function_proto_apply)
    function_proto_apply_function_object

(** val object_heap_initial_function_objects_4 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_4 h =
  let h0 = object_heap_initial_function_objects_3 h in
  let h1 =
    Heap.write h0 (Coq_object_loc_prealloc Coq_prealloc_array_is_array)
      array_is_array_function_object
  in
  let h2 =
    Heap.write h1 (Coq_object_loc_prealloc
      Coq_prealloc_array_proto_to_string)
      array_proto_to_string_function_object
  in
  let h3 =
    Heap.write h2 (Coq_object_loc_prealloc Coq_prealloc_array_proto_join)
      array_proto_join_function_object
  in
  let h4 =
    Heap.write h3 (Coq_object_loc_prealloc Coq_prealloc_array_proto_pop)
      array_proto_pop_function_object
  in
  let h5 =
    Heap.write h4 (Coq_object_loc_prealloc Coq_prealloc_array_proto_push)
      array_proto_push_function_object
  in
  let h6 =
    Heap.write h5 (Coq_object_loc_prealloc
      Coq_prealloc_string_proto_to_string)
      string_proto_to_string_function_object
  in
  let h7 =
    Heap.write h6 (Coq_object_loc_prealloc
      Coq_prealloc_string_proto_value_of)
      string_proto_value_of_function_object
  in
  let h8 =
    Heap.write h7 (Coq_object_loc_prealloc Coq_prealloc_bool_proto_to_string)
      bool_proto_to_string_function_object
  in
  Heap.write h8 (Coq_object_loc_prealloc Coq_prealloc_bool_proto_value_of)
    bool_proto_value_of_function_object

(** val object_heap_initial_function_objects :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects h =
  let h0 = object_heap_initial_function_objects_4 h in
  let h1 =
    Heap.write h0 (Coq_object_loc_prealloc
      Coq_prealloc_number_proto_to_string)
      number_proto_to_string_function_object
  in
  let h2 =
    Heap.write h1 (Coq_object_loc_prealloc
      Coq_prealloc_number_proto_value_of)
      number_proto_value_of_function_object
  in
  Heap.write h2 (Coq_object_loc_prealloc Coq_prealloc_error_proto_to_string)
    error_proto_to_string_function_object

(** val object_heap_initial : (object_loc, coq_object) Heap.heap **)

let object_heap_initial =
  let h =
    Heap.write Heap.empty (Coq_object_loc_prealloc Coq_prealloc_global)
      object_prealloc_global
  in
  let h0 =
    Heap.write h (Coq_object_loc_prealloc Coq_prealloc_object)
      object_prealloc_object
  in
  let h1 =
    Heap.write h0 (Coq_object_loc_prealloc Coq_prealloc_object_proto)
      object_prealloc_object_proto
  in
  let h2 =
    Heap.write h1 (Coq_object_loc_prealloc Coq_prealloc_bool)
      object_prealloc_bool
  in
  let h3 =
    Heap.write h2 (Coq_object_loc_prealloc Coq_prealloc_bool_proto)
      object_prealloc_bool_proto
  in
  let h4 =
    Heap.write h3 (Coq_object_loc_prealloc Coq_prealloc_number)
      object_prealloc_number
  in
  let h5 =
    Heap.write h4 (Coq_object_loc_prealloc Coq_prealloc_number_proto)
      object_prealloc_number_proto
  in
  let h6 =
    Heap.write h5 (Coq_object_loc_prealloc Coq_prealloc_function)
      object_prealloc_function
  in
  let h7 =
    Heap.write h6 (Coq_object_loc_prealloc Coq_prealloc_function_proto)
      object_prealloc_function_proto
  in
  let h8 =
    Heap.write h7 (Coq_object_loc_prealloc Coq_prealloc_array)
      object_prealloc_array
  in
  let h9 =
    Heap.write h8 (Coq_object_loc_prealloc Coq_prealloc_array_proto)
      object_prealloc_array_proto
  in
  let h10 =
    Heap.write h9 (Coq_object_loc_prealloc Coq_prealloc_string)
      object_prealloc_string
  in
  let h11 =
    Heap.write h10 (Coq_object_loc_prealloc Coq_prealloc_string_proto)
      object_prealloc_string_proto
  in
  let h12 =
    Heap.write h11 (Coq_object_loc_prealloc Coq_prealloc_math)
      object_prealloc_math
  in
  let h13 =
    Heap.write h12 (Coq_object_loc_prealloc Coq_prealloc_date)
      object_prealloc_date
  in
  let h14 =
    Heap.write h13 (Coq_object_loc_prealloc Coq_prealloc_regexp)
      object_prealloc_regexp
  in
  let h15 =
    Heap.write h14 (Coq_object_loc_prealloc Coq_prealloc_error_proto)
      object_prealloc_error_proto
  in
  let h16 =
    Heap.write h15 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto
      Coq_native_error_eval))
      (object_prealloc_native_error_proto Coq_native_error_eval)
  in
  let h17 =
    Heap.write h16 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto
      Coq_native_error_range))
      (object_prealloc_native_error_proto Coq_native_error_range)
  in
  let h18 =
    Heap.write h17 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto
      Coq_native_error_ref))
      (object_prealloc_native_error_proto Coq_native_error_ref)
  in
  let h19 =
    Heap.write h18 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto
      Coq_native_error_syntax))
      (object_prealloc_native_error_proto Coq_native_error_syntax)
  in
  let h20 =
    Heap.write h19 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto
      Coq_native_error_type))
      (object_prealloc_native_error_proto Coq_native_error_type)
  in
  let h21 =
    Heap.write h20 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto
      Coq_native_error_uri))
      (object_prealloc_native_error_proto Coq_native_error_uri)
  in
  let h22 =
    Heap.write h21 (Coq_object_loc_prealloc Coq_prealloc_error)
      object_prealloc_error
  in
  let h23 =
    Heap.write h22 (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_eval))
      (object_prealloc_native_error Coq_native_error_eval)
  in
  let h24 =
    Heap.write h23 (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_range))
      (object_prealloc_native_error Coq_native_error_range)
  in
  let h25 =
    Heap.write h24 (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_ref))
      (object_prealloc_native_error Coq_native_error_ref)
  in
  let h26 =
    Heap.write h25 (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_syntax))
      (object_prealloc_native_error Coq_native_error_syntax)
  in
  let h27 =
    Heap.write h26 (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_type))
      (object_prealloc_native_error Coq_native_error_type)
  in
  let h28 =
    Heap.write h27 (Coq_object_loc_prealloc (Coq_prealloc_native_error
      Coq_native_error_uri))
      (object_prealloc_native_error Coq_native_error_uri)
  in
  let h29 =
    Heap.write h28 (Coq_object_loc_prealloc Coq_prealloc_json)
      object_prealloc_json
  in
  object_heap_initial_function_objects h29

(** val env_record_heap_initial : (env_loc, env_record) Heap.heap **)

let env_record_heap_initial =
  Heap.write Heap.empty env_loc_global_env_record
    (env_record_object_default (Coq_object_loc_prealloc Coq_prealloc_global))

(** val all_locations : int -> int stream **)

let rec all_locations k =
  lazy (Coq_stream_intro (k, (all_locations (Pervasives.succ k))))

(** val dummy_fresh_locations : int stream **)

let dummy_fresh_locations =
  all_locations (Pervasives.succ 0)

(** val state_initial : state **)

let state_initial =
  { state_object_heap = object_heap_initial; state_env_record_heap =
    env_record_heap_initial; state_fresh_locations = dummy_fresh_locations;
    state_event_list = [] }

