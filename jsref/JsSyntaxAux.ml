(*open JsNumber*)  
open JsSyntax
open LibList
open LibOption

let int_of_native_error e =
  match e with
  | Native_error_eval -> 1
  | Native_error_range -> 2
  | Native_error_ref -> 3
  | Native_error_syntax -> 4
  | Native_error_type -> 5
  | Native_error_uri -> 6

let int_of_mathop o =
   match o with
   | Mathop_abs -> 1

let int_of_prealloc p =
  match p with
  | Prealloc_global -> 1
  | Prealloc_global_eval -> 2
  | Prealloc_global_parse_int -> 3
  | Prealloc_global_parse_float -> 4
  | Prealloc_global_is_finite -> 5
  | Prealloc_global_is_nan -> 6
  | Prealloc_global_decode_uri -> 7
  | Prealloc_global_decode_uri_component -> 8
  | Prealloc_global_encode_uri -> 9
  | Prealloc_global_encode_uri_component -> 10
  | Prealloc_object -> 11
  | Prealloc_object_get_proto_of -> 12
  | Prealloc_object_get_own_prop_descriptor -> 13
  | Prealloc_object_get_own_prop_name -> 14
  | Prealloc_object_create -> 15
  | Prealloc_object_define_prop -> 16
  | Prealloc_object_define_props -> 17
  | Prealloc_object_seal -> 18
  | Prealloc_object_freeze -> 19
  | Prealloc_object_prevent_extensions -> 20
  | Prealloc_object_is_sealed -> 21
  | Prealloc_object_is_frozen -> 22
  | Prealloc_object_is_extensible -> 23
  | Prealloc_object_keys -> 24
  | Prealloc_object_proto -> 26
  | Prealloc_object_proto_to_string -> 27
  | Prealloc_object_proto_value_of -> 28
  | Prealloc_object_proto_has_own_prop -> 29
  | Prealloc_object_proto_is_prototype_of -> 30
  | Prealloc_object_proto_prop_is_enumerable -> 31
  | Prealloc_function -> 32
  | Prealloc_function_proto -> 33
  | Prealloc_function_proto_to_string -> 34
  | Prealloc_function_proto_apply -> 35
  | Prealloc_function_proto_call -> 36
  | Prealloc_function_proto_bind -> 37
  | Prealloc_bool -> 38
  | Prealloc_bool_proto -> 39
  | Prealloc_bool_proto_to_string -> 40
  | Prealloc_bool_proto_value_of -> 41
  | Prealloc_number -> 42
  | Prealloc_number_proto -> 43
  | Prealloc_number_proto_to_string -> 44
  | Prealloc_number_proto_value_of -> 45
  | Prealloc_number_proto_to_fixed -> 46
  | Prealloc_number_proto_to_exponential -> 47
  | Prealloc_number_proto_to_precision -> 48
  | Prealloc_array -> 49
  | Prealloc_array_is_array -> 50
  | Prealloc_array_proto -> 51
  | Prealloc_array_proto_to_string -> 52
  | Prealloc_array_proto_join -> 53
  | Prealloc_array_proto_pop -> 54
  | Prealloc_array_proto_push -> 55
  | Prealloc_string -> 56
  | Prealloc_string_proto -> 57
  | Prealloc_string_proto_to_string -> 58
  | Prealloc_string_proto_value_of -> 59
  | Prealloc_string_proto_char_at -> 60
  | Prealloc_string_proto_char_code_at -> 61
  | Prealloc_math -> 62
  | Prealloc_date -> 63
  | Prealloc_regexp -> 64
  | Prealloc_error -> 65
  | Prealloc_error_proto -> 66
  | Prealloc_error_proto_to_string -> 67
  | Prealloc_throw_type_error -> 68
  | Prealloc_json -> 69
  | Prealloc_proxy -> 70
  | Prealloc_proxy_revocable -> 71
  | Builtin_proxy_revocation -> 72
  | Prealloc_object_set_proto_of -> 73
  | Prealloc_reflect                             -> 74
  | Prealloc_reflect_apply                       -> 75
  | Prealloc_reflect_construct                   -> 76
  | Prealloc_reflect_define_property             -> 77
  | Prealloc_reflect_delete_property             -> 78
  | Prealloc_reflect_get                         -> 79
  | Prealloc_reflect_get_own_property_descriptor -> 80
  | Prealloc_reflect_get_prototype_of            -> 81
  | Prealloc_reflect_has                         -> 82
  | Prealloc_reflect_is_extensible               -> 83
  | Prealloc_reflect_own_keys                    -> 84
  | Prealloc_reflect_prevent_extensions          -> 85
  | Prealloc_reflect_set                         -> 86
  | Prealloc_reflect_set_prototype_of            -> 87
  | Prealloc_mathop o -> 100 + int_of_mathop o
  | Prealloc_native_error e -> 200 + int_of_native_error e
  | Prealloc_native_error_proto e -> 300 + int_of_native_error e

(** val string_of_prealloc : prealloc -> string **)

let string_of_prealloc prealloc = match prealloc with
  | Prealloc_global -> "global"
  | Prealloc_global_eval ->
    "global_eval"
  | Prealloc_global_parse_int ->
    "global_parse_int"
  | Prealloc_global_parse_float ->
    "global_parse_float"
  | Prealloc_global_is_finite ->
    "global_is_finite"
  | Prealloc_global_is_nan ->
    "global_is_nan"
  | Prealloc_global_decode_uri ->
    "global_decode_uri"
  | Prealloc_global_decode_uri_component ->
    "global_decode_uri_component"
  | Prealloc_global_encode_uri ->
    "global_encode_uri"
  | Prealloc_global_encode_uri_component ->
    "global_encode_uri_component"
  | Prealloc_object -> "object"
  | Prealloc_object_get_proto_of ->
    "object_get_proto_of"
  | Prealloc_object_set_proto_of ->
    "object_set_proto_of"
  | Prealloc_object_get_own_prop_descriptor ->
    "object_get_own_prop_descriptor"
  | Prealloc_object_get_own_prop_name ->
    "object_get_own_prop_name"
  | Prealloc_object_create ->
    "object_create"
  | Prealloc_object_define_prop ->
    "object_define_prop"
  | Prealloc_object_define_props ->
    "object_define_props"
  | Prealloc_object_seal ->
    "object_seal"
  | Prealloc_object_freeze ->
    "object_freeze"
  | Prealloc_object_prevent_extensions ->
    "object_prevent_extensions"
  | Prealloc_object_is_sealed ->
    "object_is_sealed"
  | Prealloc_object_is_frozen ->
    "object_is_frozen"
  | Prealloc_object_is_extensible ->
    "object_is_extensible"
  | Prealloc_object_keys ->
    "object_keys"
  | Prealloc_object_proto ->
    "object_proto_"
  | Prealloc_object_proto_to_string ->
    "object_proto_to_string"
  | Prealloc_object_proto_value_of ->
    "object_proto_value_of"
  | Prealloc_object_proto_has_own_prop ->
    "object_proto_has_own_prop"
  | Prealloc_object_proto_is_prototype_of ->
    "object_proto_is_prototype_of"
  | Prealloc_object_proto_prop_is_enumerable ->
    "object_proto_prop_is_enumerable"
  | Prealloc_function ->
    "function"
  | Prealloc_function_proto ->
    "function_proto"
  | Prealloc_function_proto_to_string ->
    "function_proto_to_string"
  | Prealloc_function_proto_apply ->
    "function_proto_apply"
  | Prealloc_function_proto_call ->
    "function_proto_call"
  | Prealloc_function_proto_bind ->
    "function_proto_bind"
  | Prealloc_bool -> "bool"
  | Prealloc_bool_proto ->
    "bool_proto"
  | Prealloc_bool_proto_to_string ->
    "bool_proto_to_string"
  | Prealloc_bool_proto_value_of ->
    "bool_proto_value_of"
  | Prealloc_number -> "number"
  | Prealloc_number_proto ->
    "number_proto"
  | Prealloc_number_proto_to_string ->
    "number_proto_to_string"
  | Prealloc_number_proto_value_of ->
    "number_proto_value_of"
  | Prealloc_number_proto_to_fixed ->
    "number_proto_to_fixed"
  | Prealloc_number_proto_to_exponential ->
    "number_proto_to_exponential"
  | Prealloc_number_proto_to_precision ->
    "number_proto_to_precision"
  | Prealloc_array -> "array"
  | Prealloc_array_is_array ->
    "array_is_array"
  | Prealloc_array_proto ->
    "array_proto"
  | Prealloc_array_proto_to_string ->
    "array_proto_to_string"
  | Prealloc_array_proto_join ->
    "array_proto_join"
  | Prealloc_array_proto_pop ->
    "array_proto_pop"
  | Prealloc_array_proto_push ->
    "array_proto_push"
  | Prealloc_string -> "string"
  | Prealloc_string_proto ->
    "string_proto"
  | Prealloc_string_proto_to_string ->
    "string_proto_to_string"
  | Prealloc_string_proto_value_of ->
    "string_proto_value_of"
  | Prealloc_string_proto_char_at ->
    "string_proto_char_at"
  | Prealloc_string_proto_char_code_at ->
    "string_proto_char_code_at"
  | Prealloc_math -> "math"
  | Prealloc_mathop m -> "mathop"
  | Prealloc_date -> "date"
  | Prealloc_regexp -> "regexp"
  | Prealloc_error -> "error"
  | Prealloc_error_proto ->
    "error_proto"
  | Prealloc_native_error n ->
    "native_error"
  | Prealloc_native_error_proto n ->
    "native_error_proto"
  | Prealloc_error_proto_to_string ->
    "error_proto_to_string"
  | Prealloc_throw_type_error ->
    "throw_type_error"
  | Prealloc_json -> "json"
  | Prealloc_proxy -> "proxy"
  | Prealloc_proxy_revocable -> "proxy_revocable"
  | Builtin_proxy_revocation -> "proxy_revocation"
  | Prealloc_reflect                             -> "Prealloc_reflect"
  | Prealloc_reflect_apply                       -> "Prealloc_reflect_apply"
  | Prealloc_reflect_construct                   -> "Prealloc_reflect_construct"
  | Prealloc_reflect_define_property             -> "Prealloc_reflect_define_property"
  | Prealloc_reflect_delete_property             -> "Prealloc_reflect_delete_property"
  | Prealloc_reflect_get                         -> "Prealloc_reflect_get"
  | Prealloc_reflect_get_own_property_descriptor -> "Prealloc_reflect_get_own_property_descriptor"
  | Prealloc_reflect_get_prototype_of            -> "Prealloc_reflect_get_prototype_of"
  | Prealloc_reflect_has                         -> "Prealloc_reflect_has"
  | Prealloc_reflect_is_extensible               -> "Prealloc_reflect_is_extensible"
  | Prealloc_reflect_own_keys                    -> "Prealloc_reflect_own_keys"
  | Prealloc_reflect_prevent_extensions          -> "Prealloc_reflect_prevent_extensions"
  | Prealloc_reflect_set                         -> "Prealloc_reflect_set"
  | Prealloc_reflect_set_prototype_of            -> "Prealloc_reflect_set_prototype_of"

let prealloc_cmp p1 p2 =
  int_compare (int_of_prealloc p1) (int_of_prealloc p2)

let object_loc_cmp l1 l2 =
   match l1 with
   | Object_loc_normal n1 ->
      begin match l2 with 
      | Object_loc_normal n2 -> int_compare n1 n2
      | Object_loc_prealloc p2 -> 1
      end
   | Object_loc_prealloc p1 ->
      begin match l2 with 
      | Object_loc_normal n2 -> -1
      | Object_loc_prealloc p2 -> prealloc_cmp p1 p2
      end

(** val object_create :
    value -> class_name -> bool -> object_properties_type -> coq_object **)

let object_create_default_record vproto sclass bextens p =
  { object_proto_ = vproto;
    object_class_ = sclass;
    object_extensible_ = bextens;
    object_prim_value_ = None;
    object_properties_ = p;
    object_get_prototype_of_ = Builtin_get_prototype_of_default;
    object_set_prototype_of_ = Builtin_set_prototype_of_default;
    object_is_extensible_ = Builtin_is_extensible_default;
    object_prevent_extensions_ = Builtin_prevent_extensions_default;
    object_get_ = Builtin_get_default;
    object_get_own_prop_ = Builtin_get_own_prop_default;
    object_get_prop_ = Builtin_get_prop_default;
    object_set_ = Builtin_set_default;
    object_has_prop_ = Builtin_has_prop_default;
    object_delete_ = Builtin_delete_default;
    object_default_value_ = Builtin_default_value_default;
    object_define_own_prop_ = Builtin_define_own_prop_default;
    object_own_property_keys_ = Builtin_own_property_keys_default;
    object_construct_ = None;
    object_call_ = None;
    object_has_instance_ = None;
    object_scope_ = None;
    object_formal_parameters_ = None;
    object_code_ = None;
    object_target_function_ = None;
    object_bound_this_ = None;
    object_bound_args_ = None;
    object_parameter_map_ = None;
    object_revocable_proxy_ = None;
    object_proxy_target_ = None;
    object_proxy_handler_ = None }

let create_builtin_function_record prototype bi props isconstructor =
  { object_proto_ = prototype;
    object_class_ = "Function";
    object_extensible_ = true;
    object_prim_value_ = None;
    object_properties_ = props;
    object_get_prototype_of_ = Builtin_get_prototype_of_default;
    object_set_prototype_of_ = Builtin_set_prototype_of_default;
    object_is_extensible_ = Builtin_is_extensible_default;
    object_prevent_extensions_ = Builtin_prevent_extensions_default;
    object_get_ = Builtin_get_default;
    object_get_own_prop_ = Builtin_get_own_prop_default;
    object_get_prop_ = Builtin_get_prop_default;
    object_set_ = Builtin_set_default;
    object_has_prop_ = Builtin_has_prop_default;
    object_delete_ = Builtin_delete_default;
    object_default_value_ = Builtin_default_value_default;
    object_define_own_prop_ = Builtin_define_own_prop_default;
    object_own_property_keys_ = Builtin_own_property_keys_default;
    object_construct_ = if isconstructor then Some (Construct_prealloc bi) else None;
    object_call_ = Some (Call_prealloc bi);
    object_has_instance_ = None;
    object_scope_ = None;
    object_formal_parameters_ = None;
    object_code_ = None;
    object_target_function_ = None;
    object_bound_this_ = None;
    object_bound_args_ = None;
    object_parameter_map_ = None;
    object_revocable_proxy_ = None;
    object_proxy_target_ = None;
    object_proxy_handler_ = None }

let proxy_object_create_record =
  { object_proto_ = Value_undef;
    object_class_ = "";
    object_extensible_ = false;
    object_prim_value_ = None;
    object_properties_ = Heap.empty;
    object_get_prototype_of_ = Builtin_get_prototype_of_proxy;
    object_set_prototype_of_ = Builtin_set_prototype_of_proxy;
    object_is_extensible_ = Builtin_is_extensible_proxy;
    object_prevent_extensions_ = Builtin_prevent_extensions_proxy;
    object_get_ = Builtin_get_proxy;
    object_get_own_prop_ = Builtin_get_own_prop_proxy;
    object_get_prop_ = Builtin_get_prop_default;
    object_set_ = Builtin_set_proxy;
    object_has_prop_ = Builtin_has_prop_proxy;
    object_delete_ = Builtin_delete_proxy;
    object_default_value_ = Builtin_default_value_default;
    object_define_own_prop_ = Builtin_define_own_prop_proxy;
    object_own_property_keys_ = Builtin_own_property_keys_proxy;
    object_construct_ = None;
    object_call_ = None;
    object_has_instance_ = None;
    object_scope_ = None;
    object_formal_parameters_ = None;
    object_code_ = None;
    object_target_function_ = None;
    object_bound_this_ = None;
    object_bound_args_ = None;
    object_parameter_map_ = None;
    object_revocable_proxy_ = None;
    object_proxy_target_ = Some Value_undef;
    object_proxy_handler_ = Some Value_undef }

(** val object_set_proto : coq_object -> value -> coq_object **)

let object_set_proto o v =
  { o with object_proto_ = v }

(** val object_set_class : coq_object -> class_name -> coq_object **)

let object_set_class o s =
  { o with object_class_ = s }

(** val object_set_extensible : coq_object -> bool -> coq_object **)

let object_set_extensible o b =
  { o with object_extensible_ = b }

let object_set_call o v =
  { o with object_call_ = Some v }
let object_set_construct o v =
  { o with object_construct_ = Some v }
let object_set_proxy_target o v =
  { o with object_proxy_target_ = Some v }
let object_set_proxy_handler o v =
  { o with object_proxy_handler_ = Some v }
let object_set_revocable_proxy o v =
  { o with object_revocable_proxy_ = Some v }

(** val object_with_primitive_value : coq_object -> value -> coq_object **)

let object_with_primitive_value o v =
  { o with object_prim_value_ = (Some v) }

(** val object_with_extension : coq_object -> bool -> coq_object **)

let object_with_extension o b =
  { o with object_extensible_ = b }

(** val object_with_properties :
    coq_object -> object_properties_type -> coq_object **)

let object_with_properties o properties =
  { o with object_properties_ = properties }

(** val object_with_get : coq_object -> builtin_get -> coq_object **)

let object_with_get o g =
  { o with object_get_ = g }

(** val object_with_get_own_property :
    coq_object -> builtin_get_own_prop -> coq_object **)

let object_with_get_own_property o gop =
  { o with object_get_own_prop_ = gop }

(** val object_with_invokation :
    coq_object -> construct option -> call option -> builtin_has_instance
    option -> coq_object **)

let object_with_invokation o constr call0 has_instance =
  { o with object_construct_ = constr; object_call_ = call0;
    object_has_instance_ = has_instance }

(** val object_with_scope :
    coq_object -> lexical_env option -> coq_object **)

let object_with_scope o scope =
  { o with object_scope_ = scope }

(** val object_with_formal_params :
    coq_object -> string list option -> coq_object **)

let object_with_formal_params o params =
  { o with object_formal_parameters_ = params }

(** val object_with_details :
    coq_object -> lexical_env option -> string list option -> funcbody
    option -> object_loc option -> value option -> value list option ->
    object_loc option -> coq_object **)

let object_with_details o scope params code target boundthis boundargs paramsmap =
  { o with object_scope_ = scope; object_formal_parameters_ = params;
    object_code_ = code; object_target_function_ = target;
    object_bound_this_ = boundthis; object_bound_args_ = boundargs;
    object_parameter_map_ = paramsmap }

(** val object_for_array :
    coq_object -> builtin_define_own_prop -> coq_object **)

let object_for_array o defineownproperty =
  { o with object_define_own_prop_ = defineownproperty }

(** val object_for_args_object :
    coq_object -> object_loc -> builtin_get -> builtin_get_own_prop ->
    builtin_define_own_prop -> builtin_delete -> coq_object **)

let object_for_args_object o paramsmap get getownproperty defineownproperty delete_prop =
  { o with object_get_ = get; object_get_own_prop_ = getownproperty;
    object_delete_ = delete_prop; object_define_own_prop_ = defineownproperty;
    object_parameter_map_ = (Some paramsmap) }

(** val mathop_compare : mathop -> mathop -> bool **)

let mathop_compare m1 m2 = (m1:mathop) === m2

(* NEVER USED
    (** val native_error_compare : native_error -> native_error -> bool **)

  let native_error_compare ne1 ne2 = (ne1:native_error) === ne2

*)

(** val prealloc_compare : prealloc -> prealloc -> bool **)

let prealloc_compare bl1 bl2 = (bl1:prealloc) === bl2


(** val object_loc_compare : object_loc -> object_loc -> bool **)

let object_loc_compare l1 l2 = (l1:object_loc) === l2

(** val value_compare : value -> value -> bool **)

let value_compare v1 v2 = (v1:value) === v2

(** val mutability_compare : mutability -> mutability -> bool **)

let mutability_compare m1 m2 = (m1:mutability) === m2

(** val ref_base_type_compare : ref_base_type -> ref_base_type -> bool **)

let ref_base_type_compare rb1 rb2 =
  match rb1 with
  | Ref_base_type_value v1 ->
    (match rb2 with
     | Ref_base_type_value v2 -> value_compare v1 v2
     | Ref_base_type_env_loc e -> false)
  | Ref_base_type_env_loc l1 ->
    (match rb2 with
     | Ref_base_type_value v -> false
     | Ref_base_type_env_loc l2 -> nat_eq l1 l2)

(** val ref_compare : ref -> ref -> bool **)

let ref_compare r1 r2 =
    (ref_base_type_compare r1.ref_base r2.ref_base)
 && (string_eq r1.ref_name r2.ref_name)
 && (bool_eq r1.ref_strict r2.ref_strict)
 && (some_compare value_compare r1.ref_this_value r2.ref_this_value)

(** val type_compare : coq_type -> coq_type -> bool **)

let type_compare t1 t2 = (t1:coq_type) === t2

(** val res_with_value : res -> resvalue -> res **)

let res_with_value r rv =
  { r with res_value = rv }

(** val resvalue_compare : resvalue -> resvalue -> bool **)

let resvalue_compare rv1 rv2 =
  match rv1 with
  | Resvalue_empty ->
    (match rv2 with
     | Resvalue_empty -> true
     | Resvalue_value v -> false
     | Resvalue_ref r -> false)
  | Resvalue_value v1 ->
    (match rv2 with
     | Resvalue_empty -> false
     | Resvalue_value v2 -> value_compare v1 v2
     | Resvalue_ref r -> false)
  | Resvalue_ref r1 ->
    (match rv2 with
     | Resvalue_empty -> false
     | Resvalue_value v -> false
     | Resvalue_ref r2 -> ref_compare r1 r2)

(** val binary_op_compare : binary_op -> binary_op -> bool **)

let rec binary_op_compare op1 op2 = (op1:binary_op) === op2

(** val prog_intro_strictness : prog -> strictness_flag **)

let prog_intro_strictness p = match p with
| Prog_intro (str, els) -> str

(** val prog_elements : prog -> element list **)

let prog_elements p = match p with
| Prog_intro (str, els) -> els

(** val funcbody_prog : funcbody -> prog **)

let funcbody_prog fb = match fb with
| Funcbody_intro (p, s) -> p

(** val funcbody_is_strict : funcbody -> strictness_flag **)

let funcbody_is_strict fb = match fb with
| Funcbody_intro (p, s) ->
  match p with
  | Prog_intro (b_strict, l) -> b_strict

(** val restype_compare : restype -> restype -> bool **)

let restype_compare rt1 rt2 = (rt1:restype) === rt2

(** val label_compare : label -> label -> bool **)

let label_compare lab1 lab2 =
  match lab1 with
  | Label_empty ->
    (match lab2 with
     | Label_empty -> true
     | Label_string s -> false)
  | Label_string s1 ->
    (match lab2 with
     | Label_empty -> false
     | Label_string s2 -> string_eq s1 s2)

(** val label_set_empty : label_set **)

let label_set_empty =
  []

(** val label_set_add : label -> label list -> label list **)

let label_set_add lab labs =
  lab :: labs

(** val label_set_add_empty : label list -> label list **)

let label_set_add_empty labs =
  label_set_add Label_empty labs

(** val label_set_mem : label -> label list -> bool **)

let label_set_mem lab labs =
  mem_decide label_compare lab labs

(** val attributes_data_with_value :
    attributes_data -> value -> attributes_data **)

let attributes_data_with_value ad v' =
  { ad with attributes_data_value = v' }

(** val descriptor_with_value : descriptor -> value option -> descriptor **)

let descriptor_with_value desc v' =
  { desc with descriptor_value = v' }

(** val descriptor_with_writable :
    descriptor -> bool option -> descriptor **)

let descriptor_with_writable desc bw' =
  { desc with descriptor_writable = bw' }

(** val descriptor_with_get : descriptor -> value option -> descriptor **)

let descriptor_with_get desc vg' =
  { desc with descriptor_get = vg' }

(** val descriptor_with_set : descriptor -> value option -> descriptor **)

let descriptor_with_set desc vs' =
  { desc with descriptor_set = vs' }

(** val descriptor_with_enumerable :
    descriptor -> bool option -> descriptor **)

let descriptor_with_enumerable desc be' =
  { desc with descriptor_enumerable = be' }

(** val descriptor_with_configurable :
    descriptor -> bool option -> descriptor **)

let descriptor_with_configurable desc bc' =
  { desc with descriptor_configurable = bc' }

(** val codetype_compare : codetype -> codetype -> bool **)

let codetype_compare ct1 ct2 = (ct1:codetype) === ct2

(** {3 Convenience functions to unpack JS Value types to natives}
    Intended for use after explicit typechecks in the spec. *)
let string_of_value v = match v with
| Value_string s -> s
| _ -> failwith "Pre-checked safe type conversion failed"

let bool_of_value v = match v with
| Value_bool b -> b
| _ -> failwith "Pre-checked safe type conversion failed"

let number_of_value v = match v with
| Value_number n -> n
| _ -> failwith "Pre-checked safe type conversion failed"

let loc_of_value v = match v with
| Value_object l -> l
| _ -> failwith "Pre-checked safe type conversion failed"
