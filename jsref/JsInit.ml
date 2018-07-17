open JsCommon
(*open JsNumber*)
open JsSyntax
open JsSyntaxAux

(**------------JS Preliminary ------------------*)

(** val string_of_native_error : native_error -> string **)

let string_of_native_error _foo_ = match _foo_ with
| Coq_native_error_eval -> "EvalError"
| Coq_native_error_range -> "RangeError"
| Coq_native_error_ref -> "ReferenceError"
| Coq_native_error_syntax -> "SyntaxError"
| Coq_native_error_type -> "TypeError"
| Coq_native_error_uri -> "URIError"

(**------------End JS Preliminary ------------------*)


(** Val prop_attributes_for_global_object : value -> attributes_data **)

let prop_attributes_for_global_object v =
  { attributes_data_value = v; attributes_data_writable = true;
    attributes_data_enumerable = false; attributes_data_configurable = true }

(** val attrib_constant : value -> attributes_data **)

let attrib_constant v =
  { attributes_data_value = v; attributes_data_writable = false;
    attributes_data_enumerable = false; attributes_data_configurable =
    false }

(** @essec sec-ecmascript-standard-built-in-objects
    @esid 17-11

    Unless otherwise specified, the length property of a built-in function object has the attributes
    {v \{ \[\[Writable\]\]: false, \[\[Enumerable\]\]: false, \[\[Configurable\]\]: true \} v}.
*)
let length_property_attributes length = {
  attributes_data_value = Coq_value_number length;
  attributes_data_writable = false;
  attributes_data_enumerable = false;
  attributes_data_configurable = true;
}

(** @essec sec-ecmascript-standard-built-in-objects
    @esid 17-13

    Unless otherwise specified, the name property of a built-in function object, if it exists, has the attributes
    {v \{ \[\[Writable\]\]: false, \[\[Enumerable\]\]: false, \[\[Configurable\]\]: true \} v}.
*)
let name_property_attributes name = {
  attributes_data_value = Coq_value_string name;
  attributes_data_writable = false;
  attributes_data_enumerable = false;
  attributes_data_configurable = true;
}

(** val object_create_builtin :
    value -> class_name -> object_properties_type -> coq_object **)

let object_create_builtin vproto sclass p =
  object_create_default_record vproto sclass true p

(** val object_create_prealloc_call_or_construct :
    value -> object_properties_type -> coq_object **)

let object_create_prealloc_call_or_construct name length p =
  let sclass = "Function" in
  let p = HeapStr.write p "length" (Coq_attributes_data_of (length_property_attributes length)) in
  let p = HeapStr.write p "name" (Coq_attributes_data_of (name_property_attributes name)) in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto)) sclass p

(** val object_create_prealloc_call :
    prealloc -> value -> object_properties_type -> coq_object **)

let object_create_prealloc_call fprealloc name length p =
  let o = object_create_prealloc_call_or_construct name length p in
  object_with_invokation o None (Some (Coq_call_prealloc fprealloc)) None

(** val object_create_prealloc_constructor :
    prealloc -> value -> object_properties_type -> coq_object **)

let object_create_prealloc_constructor fprealloc name length p =
  let o = object_create_prealloc_call_or_construct name length p in
  object_with_invokation o (Some (Coq_construct_prealloc fprealloc))
    (Some (Coq_call_prealloc fprealloc)) (Some Coq_builtin_has_instance_function)

(** val write_native :
    object_properties_type -> prop_name -> value -> (prop_name, attributes)
    Heap.heap **)

let write_native p name v =
  HeapStr.write p name (Coq_attributes_data_of (prop_attributes_for_global_object v))

(** val write_constant :
    object_properties_type -> prop_name -> value -> (prop_name, attributes)
    Heap.heap **)

let write_constant p name value0 =
  HeapStr.write p name (Coq_attributes_data_of (attrib_constant value0))

(** val object_prealloc_global_proto : value **)

let object_prealloc_global_proto = Coq_value_null

(** val object_prealloc_global_class : string **)

let object_prealloc_global_class = "GlobalClass"

(** val object_prealloc_global_properties :
    (prop_name, attributes) Heap.heap **)

let object_prealloc_global_properties =
  let p = write_constant Heap.empty "NaN" (Coq_value_number JsNumber.nan) in
  let p0 = write_constant p "Infinity" (Coq_value_number JsNumber.infinity) in
  let p1 = write_constant p0 "undefined" Coq_value_undef in
  let p2 = write_native p1 "eval" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_eval)) in
  let p3 = write_native p2 "parseInt" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_parse_int)) in
  let p4 = write_native p3 "parseFloat" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_parse_float)) in
  let p5 = write_native p4 "isNaN" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_is_nan)) in
  let p6 = write_native p5 "isFinite" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_is_finite)) in
  let p7 = write_native p6 "decodeURI" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_decode_uri)) in
  let p8 = write_native p7 "decodeURIComponent" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_decode_uri_component)) in
  let p9 = write_native p8 "encodeURI" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_encode_uri)) in
  let p10 = write_native p9 "encodeURIComponent" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global_encode_uri_component)) in
  let p11 = write_native p10 "Object" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object)) in
  let p12 = write_native p11 "Function" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function)) in
  let p13 = write_native p12 "Array" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array)) in
  let p14 = write_native p13 "String" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string)) in
  let p15 = write_native p14 "Boolean" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool)) in
  let p16 = write_native p15 "Number" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number)) in
  let p17 = write_native p16 "Math" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_math)) in
  let p18 = write_native p17 "Date" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_date)) in
  let p19 = write_native p18 "RegExp" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_regexp)) in
  let p20 = write_native p19 "Error" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error)) in
  let p21 = write_native p20 "EvalError" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_eval))) in
  let p22 = write_native p21 "RangeError" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_range))) in
  let p23 = write_native p22 "ReferenceError" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_ref))) in
  let p24 = write_native p23 "SyntaxError" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_syntax))) in
  let p25 = write_native p24 "TypeError" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_type))) in
  let p26 = write_native p25 "URIError" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_uri))) in
  let p27 = write_native p26 "JSON" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_json)) in
  write_native p27 "Proxy" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_proxy))

(** val object_prealloc_global : coq_object **)

let object_prealloc_global =
  object_create_builtin object_prealloc_global_proto object_prealloc_global_class object_prealloc_global_properties

(** val global_eval_function_object : coq_object **)

let global_eval_function_object =
  object_create_prealloc_call Coq_prealloc_global_eval "eval" 1.0 Heap.empty

(** val global_parse_int_function_object : coq_object **)

let global_parse_int_function_object =
  object_create_prealloc_call Coq_prealloc_global_parse_int "parseInt" 2.0 Heap.empty

(** val global_parse_float_function_object : coq_object **)

let global_parse_float_function_object =
  object_create_prealloc_call Coq_prealloc_global_parse_float "parseFloat" 1.0 Heap.empty

(** val global_is_nan_function_object : coq_object **)

let global_is_nan_function_object =
  object_create_prealloc_call Coq_prealloc_global_is_nan "isNaN" 1.0 Heap.empty

(** val global_is_finite_function_object : coq_object **)

let global_is_finite_function_object =
  object_create_prealloc_call Coq_prealloc_global_is_finite "isFinite" 1.0 Heap.empty

(** val global_decode_uri_function_object : coq_object **)

let global_decode_uri_function_object =
  object_create_prealloc_call Coq_prealloc_global_decode_uri "decodeURI" 1.0 Heap.empty

(** val global_decode_uri_component_function_object : coq_object **)

let global_decode_uri_component_function_object =
  object_create_prealloc_call Coq_prealloc_global_decode_uri_component "decodeURIComponent" 1.0 Heap.empty

(** val global_encode_uri_function_object : coq_object **)

let global_encode_uri_function_object =
  object_create_prealloc_call Coq_prealloc_global_encode_uri "encodeURI" 1.0 Heap.empty

(** val global_encode_uri_component_function_object : coq_object **)

let global_encode_uri_component_function_object =
  object_create_prealloc_call Coq_prealloc_global_encode_uri_component "encodeURIComponent" 1.0 Heap.empty

(** val object_prealloc_object : coq_object **)

let object_prealloc_object =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) in
  let p = write_native p "getPrototypeOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_get_proto_of)) in
  let p = write_native p "setPrototypeOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_set_proto_of)) in
  let p = write_native p "getOwnPropertyDescriptor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_get_own_prop_descriptor)) in
  let p = write_native p "getOwnPropertyNames" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_get_own_prop_name)) in
  let p = write_native p "create" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_create)) in
  let p = write_native p "defineProperty" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_define_prop)) in
  let p = write_native p "defineProperties" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_define_props)) in
  let p = write_native p "seal" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_seal)) in
  let p = write_native p "freeze" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_freeze)) in
  let p = write_native p "preventExtensions" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_prevent_extensions)) in
  let p = write_native p "isSealed" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_is_sealed)) in
  let p = write_native p "isFrozen" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_is_frozen)) in
  let p = write_native p "isExtensible" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_is_extensible)) in
  let p = write_native p "keys" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_keys)) in
  object_create_prealloc_constructor Coq_prealloc_object "Object" 1.0 p

(** val object_get_proto_of_function_object : coq_object **)

let object_get_proto_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_get_proto_of "getPrototypeOf" 1.0 Heap.empty

let object_set_proto_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_set_proto_of "setPrototypeOf" 2.0 Heap.empty

(** val object_get_own_prop_descriptor_function_object : coq_object **)

let object_get_own_prop_descriptor_function_object =
  object_create_prealloc_call Coq_prealloc_object_get_own_prop_descriptor "getOwnPropertyDescriptor" 1.0 Heap.empty

(** val object_get_own_prop_name_function_object : coq_object **)

let object_get_own_prop_name_function_object =
  object_create_prealloc_call Coq_prealloc_object_get_own_prop_name "getOwnPropertyName" 1.0 Heap.empty

(** val object_create_function_object : coq_object **)

let object_create_function_object =
  object_create_prealloc_call Coq_prealloc_object_create "create" 2.0 Heap.empty

(** val object_define_prop_function_object : coq_object **)

let object_define_prop_function_object =
  object_create_prealloc_call Coq_prealloc_object_define_prop "defineProperty" 3.0 Heap.empty

(** val object_define_props_function_object : coq_object **)

let object_define_props_function_object =
  object_create_prealloc_call Coq_prealloc_object_define_props "defineProperties" 2.0 Heap.empty

(** val object_seal_function_object : coq_object **)

let object_seal_function_object =
  object_create_prealloc_call Coq_prealloc_object_seal "seal" 1.0 Heap.empty

(** val object_freeze_function_object : coq_object **)

let object_freeze_function_object =
  object_create_prealloc_call Coq_prealloc_object_freeze "freeze" 1.0 Heap.empty

(** val object_prevent_extensions_function_object : coq_object **)

let object_prevent_extensions_function_object =
  object_create_prealloc_call Coq_prealloc_object_prevent_extensions "preventExtensions" 1.0 Heap.empty

(** val object_is_sealed_function_object : coq_object **)

let object_is_sealed_function_object =
  object_create_prealloc_call Coq_prealloc_object_is_sealed "isSealed" 1.0 Heap.empty

(** val object_is_frozen_function_object : coq_object **)

let object_is_frozen_function_object =
  object_create_prealloc_call Coq_prealloc_object_is_frozen "isFrozen" 1.0 Heap.empty

(** val object_is_extensible_function_object : coq_object **)

let object_is_extensible_function_object =
  object_create_prealloc_call Coq_prealloc_object_is_extensible "isExtensible" 1.0 Heap.empty

let object_keys_function_object =
  object_create_prealloc_call Coq_prealloc_object_keys "keys" 1.0 Heap.empty

(** val object_prealloc_object_proto : coq_object **)

let object_prealloc_object_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object)) in
  let p0 = write_native p "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto_to_string)) in
  let p1 = write_native p0 "valueOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto_value_of)) in
  let p2 = write_native p1 "hasOwnProperty" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto_has_own_prop)) in
  let p3 = write_native p2 "isPrototypeOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto_is_prototype_of)) in
  let p4 = write_native p3 "propertyIsEnumerable" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto_prop_is_enumerable)) in
  object_create_builtin Coq_value_null "Object" p4

(** val object_proto_to_string_function_object : coq_object **)

let object_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_to_string "toString" 0.0 Heap.empty

(** val object_proto_value_of_function_object : coq_object **)

let object_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_value_of "valueOf" 0.0 Heap.empty

(** val object_proto_has_own_prop_function_object : coq_object **)

let object_proto_has_own_prop_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_has_own_prop "hasOwnProperty" 0.0 Heap.empty

(** val object_proto_is_prototype_of_function_object : coq_object **)

let object_proto_is_prototype_of_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_is_prototype_of "isPrototypeOf" 1.0 Heap.empty

(** val object_proto_prop_is_enumerable_function_object : coq_object **)

let object_proto_prop_is_enumerable_function_object =
  object_create_prealloc_call Coq_prealloc_object_proto_prop_is_enumerable "isEnumerable" 1.0 Heap.empty

(** val object_prealloc_function : coq_object **)

let object_prealloc_function =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto)) in
  object_create_prealloc_constructor Coq_prealloc_function "Function" 1.0 p

(* Note: manual object construction due to non-standard prototype. *)
let object_prealloc_function_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function)) in
  let p = HeapStr.write p "length" (Coq_attributes_data_of (length_property_attributes 0.0)) in
  let p = HeapStr.write p "name" (Coq_attributes_data_of (name_property_attributes "")) in
  let p = write_native p "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto_to_string)) in
  let p = write_native p "apply" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto_apply)) in
  let p = write_native p "call" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto_call)) in
  let p = write_native p "bind" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto_bind)) in
  let o = object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "Function" p in
  object_with_invokation o None (Some (Coq_call_prealloc Coq_prealloc_function_proto)) None

(** val function_proto_to_string_function_object : coq_object **)

let function_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_to_string "toString" 0.0 Heap.empty

(** val function_proto_call_function_object : coq_object **)

let function_proto_call_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_call "call" 1.0 Heap.empty

(** val function_proto_bind_function_object : coq_object **)

let function_proto_bind_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_bind "bind" 1.0 Heap.empty

(** val function_proto_apply_function_object : coq_object **)

let function_proto_apply_function_object =
  object_create_prealloc_call Coq_prealloc_function_proto_apply "apply" 2.0 Heap.empty

(** val object_prealloc_number : coq_object **)

let object_prealloc_number =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto)) in
  let p0 = write_constant p "NaN" (Coq_value_number JsNumber.nan) in
  let p1 = write_constant p0 "NEGATIVE_INFINITY" (Coq_value_number JsNumber.neg_infinity) in
  let p2 = write_constant p1 "POSITIVE_INFINITY" (Coq_value_number JsNumber.infinity) in
  let p3 = write_constant p2 "MAX_VALUE" (Coq_value_number JsNumber.max_value) in
  let p4 = write_constant p3 "MIN_VALUE" (Coq_value_number JsNumber.min_value) in
  object_create_prealloc_constructor Coq_prealloc_number "Number" 1.0 p4

(** val object_prealloc_number_proto : coq_object **)

let object_prealloc_number_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number)) in
  let p0 = write_native p "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto_to_string)) in
  let p1 = write_native p0 "valueOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto_value_of)) in
  let o = object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "Number" p1 in
  object_with_primitive_value o (Coq_value_number JsNumber.zero)

(** val number_proto_to_string_function_object : coq_object **)

let number_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_number_proto_to_string "toString" 0.0 Heap.empty

(** val number_proto_value_of_function_object : coq_object **)

let number_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_number_proto_value_of "valueOf" 0.0 Heap.empty

(** val object_prealloc_array : coq_object **)

let object_prealloc_array =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto)) in
  let p0 = write_native p "isArray" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_is_array)) in
  object_create_prealloc_constructor Coq_prealloc_array "Array" 1.0 p0

(** val array_is_array_function_object : coq_object **)

let array_is_array_function_object =
  object_create_prealloc_call Coq_prealloc_array_is_array "isArray" 1.0 Heap.empty

(** val object_prealloc_array_proto : coq_object **)

let object_prealloc_array_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array)) in
  let p0 = write_native p "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto_to_string)) in
  let p1 = write_native p0 "join" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto_join)) in
  let p2 = write_native p1 "pop" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto_pop)) in
  let p3 = write_native p2 "push" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto_push)) in
  let p4 = HeapStr.write p3 "length"
      (Coq_attributes_data_of { attributes_data_writable = true; attributes_data_enumerable = false;
                                attributes_data_configurable = false; attributes_data_value = Coq_value_number 0.0}) in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "Array" p4

(** val array_proto_pop_function_object : coq_object **)

let array_proto_pop_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_pop "pop" 0.0 Heap.empty

(** val array_proto_push_function_object : coq_object **)

let array_proto_push_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_push "push" 1.0 Heap.empty

(** val array_proto_to_string_function_object : coq_object **)

let array_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_to_string "toString" 0.0 Heap.empty

(** val array_proto_join_function_object : coq_object **)

let array_proto_join_function_object =
  object_create_prealloc_call Coq_prealloc_array_proto_join "join" 1.0 Heap.empty

(** val object_prealloc_string : coq_object **)

let object_prealloc_string =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string_proto)) in
  object_create_prealloc_constructor Coq_prealloc_string "String" 1.0 p

(** val object_prealloc_string_proto : coq_object **)

let object_prealloc_string_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string)) in
  let p0 = write_native p "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string_proto_to_string)) in
  let p1 = write_native p0 "valueOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string_proto_value_of)) in
  let o = object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "String" p1 in
  object_with_primitive_value o (Coq_value_string "")

(** val string_proto_to_string_function_object : coq_object **)

let string_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_string_proto_to_string "toString" 0.0 Heap.empty

(** val string_proto_value_of_function_object : coq_object **)

let string_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_string_proto_value_of "valueOf" 0.0 Heap.empty

(** val object_prealloc_bool : coq_object **)

let object_prealloc_bool =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto)) in
  object_create_prealloc_constructor Coq_prealloc_bool "Boolean" 1.0 p

(** val object_prealloc_bool_proto : coq_object **)

let object_prealloc_bool_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool)) in
  let p0 = write_native p "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto_to_string)) in
  let p1 = write_native p0 "valueOf" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto_value_of)) in
  let o = object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "Boolean" p1 in
  object_with_primitive_value o (Coq_value_bool false)

(** val bool_proto_to_string_function_object : coq_object **)

let bool_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_bool_proto_to_string "toString" 0.0 Heap.empty

(** val bool_proto_value_of_function_object : coq_object **)

let bool_proto_value_of_function_object =
  object_create_prealloc_call Coq_prealloc_bool_proto_value_of "valueOf" 0.0 Heap.empty

(** val object_prealloc_math : coq_object **)

let object_prealloc_math =
  let p = write_constant Heap.empty "PI" (Coq_value_number JsNumber.pi) in
  let p0 = write_constant p "E" (Coq_value_number JsNumber.e) in
  let p1 = write_constant p0 "LN2" (Coq_value_number JsNumber.ln2) in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "Math" p1

(** val object_prealloc_date : coq_object **)

let object_prealloc_date =
  object_create_prealloc_constructor Coq_prealloc_date "Date" 1.0 Heap.empty

(** val object_prealloc_regexp : coq_object **)

let object_prealloc_regexp =
  object_create_prealloc_constructor Coq_prealloc_regexp "RegExp" 1.0 Heap.empty

(** val object_prealloc_error : coq_object **)

let object_prealloc_error =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error_proto)) in
  object_create_prealloc_constructor Coq_prealloc_error "Error" 1.0 p

(** val object_prealloc_error_proto : coq_object **)

let object_prealloc_error_proto =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error)) in
  let p0 = write_native p "name" (Coq_value_string "Error") in
  let p1 = write_native p0 "message" (Coq_value_string "") in
  let p2 = write_native p1 "toString" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error_proto_to_string)) in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "Error" p2

(** val error_proto_to_string_function_object : coq_object **)

let error_proto_to_string_function_object =
  object_create_prealloc_call Coq_prealloc_error_proto_to_string "toString" 0.0 Heap.empty

(** val object_prealloc_native_error : native_error -> coq_object **)

let object_prealloc_native_error ne =
  let p = write_constant Heap.empty "prototype" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto ne))) in
  object_create_prealloc_constructor (Coq_prealloc_native_error ne) (string_of_native_error ne) 1.0 p

(** val object_prealloc_native_error_proto : native_error -> coq_object **)

let object_prealloc_native_error_proto ne =
  let p = write_native Heap.empty "constructor" (Coq_value_object (Coq_object_loc_prealloc (Coq_prealloc_native_error ne))) in
  let p0 = write_native p "name" (Coq_value_string (string_of_native_error ne)) in
  let p1 = write_native p0 "message" (Coq_value_string "") in
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_error_proto)) "Error" p1

(** val object_prealloc_json : coq_object **)

let object_prealloc_json =
  object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) "JSON" Heap.empty

(** @essec 9.2.7.1
    @esid sec-%throwtypeerror% *)
(* Note: manual object creation due to non-standard length attributes. *)
let throw_type_error_object =
  let p = write_constant Heap.empty "length" (Coq_value_number 0.0) in
  let o = object_create_builtin (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto)) "Function" p in
  let o = object_with_invokation o None (Some (Coq_call_prealloc Coq_prealloc_throw_type_error)) None in
  let o = object_with_scope o (Some lexical_env_initial) in
  let o = object_with_formal_params o (Some []) in
  object_set_extensible o false

(** @essec 26.2.1
    @esid sec-proxy-constructor *)
let object_prealloc_proxy =
  let p = write_native Heap.empty "revocable" (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_proxy_revocable)) in
  object_create_prealloc_constructor Coq_prealloc_proxy "Proxy" 2. p

let object_prealloc_proxy_revocable =
  object_create_prealloc_call Coq_prealloc_proxy_revocable "revocable" 2. Heap.empty

(** val object_heap_initial_function_objects_1 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_1 h =
  let h0 = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_throw_type_error) throw_type_error_object in
  let h1 = HeapObj.write h0 (Coq_object_loc_prealloc Coq_prealloc_global_eval) global_eval_function_object in
  let h2 = HeapObj.write h1 (Coq_object_loc_prealloc Coq_prealloc_global_parse_int) global_parse_int_function_object in
  let h3 = HeapObj.write h2 (Coq_object_loc_prealloc Coq_prealloc_global_parse_float) global_parse_float_function_object in
  let h4 = HeapObj.write h3 (Coq_object_loc_prealloc Coq_prealloc_global_is_nan) global_is_nan_function_object in
  let h5 = HeapObj.write h4 (Coq_object_loc_prealloc Coq_prealloc_global_is_finite) global_is_finite_function_object in
  let h6 = HeapObj.write h5 (Coq_object_loc_prealloc Coq_prealloc_global_decode_uri) global_decode_uri_function_object in
  let h7 = HeapObj.write h6 (Coq_object_loc_prealloc Coq_prealloc_global_decode_uri_component) global_decode_uri_component_function_object in
  let h8 = HeapObj.write h7 (Coq_object_loc_prealloc Coq_prealloc_global_encode_uri) global_encode_uri_function_object in
  HeapObj.write h8 (Coq_object_loc_prealloc Coq_prealloc_global_encode_uri_component) global_encode_uri_component_function_object

(** val object_heap_initial_function_objects_2 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_2 h =
  let h = object_heap_initial_function_objects_1 h in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_get_proto_of) object_get_proto_of_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_set_proto_of) object_set_proto_of_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_get_own_prop_descriptor) object_get_own_prop_descriptor_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_get_own_prop_name) object_get_own_prop_name_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_create) object_create_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_define_prop) object_define_prop_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_define_props) object_define_props_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_seal) object_seal_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_freeze) object_freeze_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_prevent_extensions) object_prevent_extensions_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_is_sealed) object_is_sealed_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_is_frozen) object_is_frozen_function_object in
  let h = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_keys) object_keys_function_object in
  HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object_is_extensible) object_is_extensible_function_object

(** val object_heap_initial_function_objects_3 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_3 h =
  let h0 = object_heap_initial_function_objects_2 h in
  let h1 = HeapObj.write h0 (Coq_object_loc_prealloc Coq_prealloc_object_proto_to_string) object_proto_to_string_function_object in
  let h2 = HeapObj.write h1 (Coq_object_loc_prealloc Coq_prealloc_object_proto_value_of) object_proto_value_of_function_object in
  let h3 = HeapObj.write h2 (Coq_object_loc_prealloc Coq_prealloc_object_proto_has_own_prop) object_proto_has_own_prop_function_object in
  let h4 = HeapObj.write h3 (Coq_object_loc_prealloc Coq_prealloc_object_proto_is_prototype_of) object_proto_is_prototype_of_function_object in
  let h5 = HeapObj.write h4 (Coq_object_loc_prealloc Coq_prealloc_object_proto_prop_is_enumerable) object_proto_prop_is_enumerable_function_object in
  let h6 = HeapObj.write h5 (Coq_object_loc_prealloc Coq_prealloc_function_proto_to_string) function_proto_to_string_function_object in
  let h7 = HeapObj.write h6 (Coq_object_loc_prealloc Coq_prealloc_function_proto_call) function_proto_call_function_object in
  let h8 = HeapObj.write h7 (Coq_object_loc_prealloc Coq_prealloc_function_proto_bind) function_proto_bind_function_object in
  HeapObj.write h8 (Coq_object_loc_prealloc Coq_prealloc_function_proto_apply) function_proto_apply_function_object

(** val object_heap_initial_function_objects_4 :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects_4 h =
  let h0 = object_heap_initial_function_objects_3 h in
  let h1 = HeapObj.write h0 (Coq_object_loc_prealloc Coq_prealloc_array_is_array) array_is_array_function_object in
  let h2 = HeapObj.write h1 (Coq_object_loc_prealloc Coq_prealloc_array_proto_to_string) array_proto_to_string_function_object in
  let h3 = HeapObj.write h2 (Coq_object_loc_prealloc Coq_prealloc_array_proto_join) array_proto_join_function_object in
  let h4 = HeapObj.write h3 (Coq_object_loc_prealloc Coq_prealloc_array_proto_pop) array_proto_pop_function_object in
  let h5 = HeapObj.write h4 (Coq_object_loc_prealloc Coq_prealloc_array_proto_push) array_proto_push_function_object in
  let h6 = HeapObj.write h5 (Coq_object_loc_prealloc Coq_prealloc_string_proto_to_string) string_proto_to_string_function_object in
  let h7 = HeapObj.write h6 (Coq_object_loc_prealloc Coq_prealloc_string_proto_value_of) string_proto_value_of_function_object in
  let h8 = HeapObj.write h7 (Coq_object_loc_prealloc Coq_prealloc_bool_proto_to_string) bool_proto_to_string_function_object in
  HeapObj.write h8 (Coq_object_loc_prealloc Coq_prealloc_bool_proto_value_of) bool_proto_value_of_function_object

(** val object_heap_initial_function_objects :
    (object_loc, coq_object) Heap.heap -> (object_loc, coq_object) Heap.heap **)

let object_heap_initial_function_objects h =
  let h0 = object_heap_initial_function_objects_4 h in
  let h1 = HeapObj.write h0 (Coq_object_loc_prealloc Coq_prealloc_number_proto_to_string) number_proto_to_string_function_object in
  let h2 = HeapObj.write h1 (Coq_object_loc_prealloc Coq_prealloc_number_proto_value_of) number_proto_value_of_function_object in
  let h3 = HeapObj.write h2 (Coq_object_loc_prealloc Coq_prealloc_error_proto_to_string) error_proto_to_string_function_object in
  HeapObj.write h3 (Coq_object_loc_prealloc Coq_prealloc_proxy_revocable) object_prealloc_proxy_revocable

(** val object_heap_initial : (object_loc, coq_object) HeapObj.heap **)

let object_heap_initial =
  let h = HeapObj.write Heap.empty (Coq_object_loc_prealloc Coq_prealloc_global) object_prealloc_global in
  let h0 = HeapObj.write h (Coq_object_loc_prealloc Coq_prealloc_object) object_prealloc_object in
  let h1 = HeapObj.write h0 (Coq_object_loc_prealloc Coq_prealloc_object_proto) object_prealloc_object_proto in
  let h2 = HeapObj.write h1 (Coq_object_loc_prealloc Coq_prealloc_bool) object_prealloc_bool in
  let h3 = HeapObj.write h2 (Coq_object_loc_prealloc Coq_prealloc_bool_proto) object_prealloc_bool_proto in
  let h4 = HeapObj.write h3 (Coq_object_loc_prealloc Coq_prealloc_number) object_prealloc_number in
  let h5 = HeapObj.write h4 (Coq_object_loc_prealloc Coq_prealloc_number_proto) object_prealloc_number_proto in
  let h6 = HeapObj.write h5 (Coq_object_loc_prealloc Coq_prealloc_function) object_prealloc_function in
  let h7 = HeapObj.write h6 (Coq_object_loc_prealloc Coq_prealloc_function_proto) object_prealloc_function_proto in
  let h8 = HeapObj.write h7 (Coq_object_loc_prealloc Coq_prealloc_array) object_prealloc_array in
  let h9 = HeapObj.write h8 (Coq_object_loc_prealloc Coq_prealloc_array_proto) object_prealloc_array_proto in
  let h10 = HeapObj.write h9 (Coq_object_loc_prealloc Coq_prealloc_string) object_prealloc_string in
  let h11 = HeapObj.write h10 (Coq_object_loc_prealloc Coq_prealloc_string_proto) object_prealloc_string_proto in
  let h12 = HeapObj.write h11 (Coq_object_loc_prealloc Coq_prealloc_math) object_prealloc_math in
  let h13 = HeapObj.write h12 (Coq_object_loc_prealloc Coq_prealloc_date) object_prealloc_date in
  let h14 = HeapObj.write h13 (Coq_object_loc_prealloc Coq_prealloc_regexp) object_prealloc_regexp in
  let h15 = HeapObj.write h14 (Coq_object_loc_prealloc Coq_prealloc_error_proto) object_prealloc_error_proto in
  let h16 = HeapObj.write h15 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto Coq_native_error_eval))
      (object_prealloc_native_error_proto Coq_native_error_eval) in
  let h17 = HeapObj.write h16 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto Coq_native_error_range))
      (object_prealloc_native_error_proto Coq_native_error_range) in
  let h18 = HeapObj.write h17 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto Coq_native_error_ref))
      (object_prealloc_native_error_proto Coq_native_error_ref) in
  let h19 = HeapObj.write h18 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto Coq_native_error_syntax))
      (object_prealloc_native_error_proto Coq_native_error_syntax) in
  let h20 = HeapObj.write h19 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto Coq_native_error_type))
      (object_prealloc_native_error_proto Coq_native_error_type) in
  let h21 = HeapObj.write h20 (Coq_object_loc_prealloc (Coq_prealloc_native_error_proto Coq_native_error_uri))
      (object_prealloc_native_error_proto Coq_native_error_uri) in
  let h22 = HeapObj.write h21 (Coq_object_loc_prealloc Coq_prealloc_error) object_prealloc_error in
  let h23 = HeapObj.write h22 (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_eval))
      (object_prealloc_native_error Coq_native_error_eval) in
  let h24 = HeapObj.write h23 (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_range))
      (object_prealloc_native_error Coq_native_error_range) in
  let h25 = HeapObj.write h24 (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_ref))
      (object_prealloc_native_error Coq_native_error_ref) in
  let h26 = HeapObj.write h25 (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_syntax))
      (object_prealloc_native_error Coq_native_error_syntax) in
  let h27 = HeapObj.write h26 (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_type))
      (object_prealloc_native_error Coq_native_error_type) in
  let h28 = HeapObj.write h27 (Coq_object_loc_prealloc (Coq_prealloc_native_error Coq_native_error_uri))
      (object_prealloc_native_error Coq_native_error_uri) in
  let h29 = HeapObj.write h28 (Coq_object_loc_prealloc Coq_prealloc_json) object_prealloc_json in
  let h30 = HeapObj.write h29 (Coq_object_loc_prealloc Coq_prealloc_proxy) object_prealloc_proxy in
  object_heap_initial_function_objects h30

(** val env_record_heap_initial : (env_loc, env_record) Heap.heap **)

let env_record_heap_initial =
  HeapInt.write Heap.empty env_loc_global_env_record
    (env_record_object_default (Coq_object_loc_prealloc Coq_prealloc_global))

(** val dummy_fresh_locations : int stream **)

let dummy_fresh_locations = 0

(** val state_initial : state **)

let state_initial =
  { state_object_heap = object_heap_initial;
    state_env_record_heap = env_record_heap_initial;
    state_fresh_locations = dummy_fresh_locations;
     }

