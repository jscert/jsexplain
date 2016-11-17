(*open JsNumber*)
open Heap
open Shared

type unary_op =
| Coq_unary_op_delete
| Coq_unary_op_void
| Coq_unary_op_typeof
| Coq_unary_op_post_incr
| Coq_unary_op_post_decr
| Coq_unary_op_pre_incr
| Coq_unary_op_pre_decr
| Coq_unary_op_add
| Coq_unary_op_neg
| Coq_unary_op_bitwise_not
| Coq_unary_op_not

type binary_op =
| Coq_binary_op_mult
| Coq_binary_op_div
| Coq_binary_op_mod
| Coq_binary_op_add
| Coq_binary_op_sub
| Coq_binary_op_left_shift
| Coq_binary_op_right_shift
| Coq_binary_op_unsigned_right_shift
| Coq_binary_op_lt
| Coq_binary_op_gt
| Coq_binary_op_le
| Coq_binary_op_ge
| Coq_binary_op_instanceof
| Coq_binary_op_in
| Coq_binary_op_equal
| Coq_binary_op_disequal
| Coq_binary_op_strict_equal
| Coq_binary_op_strict_disequal
| Coq_binary_op_bitwise_and
| Coq_binary_op_bitwise_or
| Coq_binary_op_bitwise_xor
| Coq_binary_op_and
| Coq_binary_op_or
| Coq_binary_op_coma

type literal =
| Coq_literal_null
| Coq_literal_bool of bool [@f value]
| Coq_literal_number of JsNumber.number [@f value]
| Coq_literal_string of string [@f value]

type label =
| Coq_label_empty
| Coq_label_string of string [@f value]

type label_set = label list

type strictness_flag = bool

(** val strictness_false : strictness_flag **)

let strictness_false =
  false

type propname =
| Coq_propname_identifier of string [@f value]
| Coq_propname_string of string [@f value]
| Coq_propname_number of JsNumber.number [@f value]

type expr =
| Coq_expr_this
| Coq_expr_identifier of string [@f name]
| Coq_expr_literal of literal [@f value]
| Coq_expr_object of (propname * propbody) list [@f fields]
| Coq_expr_array of expr option list [@f elements]
| Coq_expr_function of string option * string list * funcbody [@f func_name_opt, arg_names, body]
| Coq_expr_access of expr * expr [@f obj, field]
| Coq_expr_member of expr * string [@f obj, field_name]
| Coq_expr_new of expr * expr list [@f func, args]
| Coq_expr_call of expr * expr list [@f func, args]
| Coq_expr_unary_op of unary_op * expr [@f op, arg]
| Coq_expr_binary_op of expr * binary_op * expr [@f arg1, op, arg2]
| Coq_expr_conditional of expr * expr * expr [@f cond, then_branch, else_branch]
| Coq_expr_assign of expr * binary_op option * expr [@f left_expr, op_opt, right_expr]
and propbody =
| Coq_propbody_val of expr [@f expr]
| Coq_propbody_get of funcbody [@f body]
| Coq_propbody_set of string list * funcbody [@f names, body]
and funcbody =
| Coq_funcbody_intro of prog * string [@f prog, source]
and stat =
| Coq_stat_expr of expr [@f expr]
| Coq_stat_label of string * stat [@f label, stat]
| Coq_stat_block of stat list [@f stats]
| Coq_stat_var_decl of (string * expr option) list [@f decls]
| Coq_stat_if of expr * stat * stat option [@f cond, then_branch, else_branch]
| Coq_stat_do_while of label_set * stat * expr [@f labels, body, cond]
| Coq_stat_while of label_set * expr * stat [@f labels, cond, body]
| Coq_stat_with of expr * stat [@f obj, stat]
| Coq_stat_throw of expr [@f arg]
| Coq_stat_return of expr option [@f arg_opt]
| Coq_stat_break of label [@f label]
| Coq_stat_continue of label [@f label]
| Coq_stat_try of stat * (string * stat) option * stat option [@f body, catch_stats_opt, finally_opt]
| Coq_stat_for of label_set * expr option * expr option * expr option * stat [@f labels, init, cond, step, body]
| Coq_stat_for_var of label_set * (string * expr option) list * expr option * expr option * stat [@f labels, init, cond, step, body]
| Coq_stat_for_in of label_set * expr * expr * stat [@f labels, id, obj, body]
| Coq_stat_for_in_var of label_set * string * expr option * expr * stat [@f labels, id, init, obj, body]
| Coq_stat_debugger
| Coq_stat_switch of label_set * expr * switchbody [@f labels, arg, body]
and switchbody =
| Coq_switchbody_nodefault of switchclause list [@f clauses]
| Coq_switchbody_withdefault of switchclause list * stat list * switchclause list [@f clauses_before, clause_default, clauses_after]
and switchclause =
| Coq_switchclause_intro of expr * stat list [@f arg, stats]
and prog =
| Coq_prog_intro of strictness_flag * element list [@f strictness, elements]
and element =
| Coq_element_stat of stat [@f stat]
| Coq_element_func_decl of string * string list * funcbody [@f func_name, arg_names, body]

type propdefs = (propname * propbody) list

type elements = element list

type funcdecl = { funcdecl_name : string;
                  funcdecl_parameters : string list;
                  funcdecl_body : funcbody }

(** val funcdecl_name : funcdecl -> string **)

let funcdecl_name x = x.funcdecl_name

(** val funcdecl_parameters : funcdecl -> string list **)

let funcdecl_parameters x = x.funcdecl_parameters

(** val funcdecl_body : funcdecl -> funcbody **)

let funcdecl_body x = x.funcdecl_body

type mathop =
| Coq_mathop_abs

type native_error =
| Coq_native_error_eval
| Coq_native_error_range
| Coq_native_error_ref
| Coq_native_error_syntax
| Coq_native_error_type
| Coq_native_error_uri

(** Intrinsic Objects *)
type prealloc =
| Coq_prealloc_global
| Coq_prealloc_global_eval
| Coq_prealloc_global_parse_int
| Coq_prealloc_global_parse_float
| Coq_prealloc_global_is_finite
| Coq_prealloc_global_is_nan
| Coq_prealloc_global_decode_uri
| Coq_prealloc_global_decode_uri_component
| Coq_prealloc_global_encode_uri
| Coq_prealloc_global_encode_uri_component
| Coq_prealloc_object
| Coq_prealloc_object_get_proto_of
| Coq_prealloc_object_get_own_prop_descriptor
| Coq_prealloc_object_get_own_prop_name
| Coq_prealloc_object_create
| Coq_prealloc_object_define_prop
| Coq_prealloc_object_define_props
| Coq_prealloc_object_seal
| Coq_prealloc_object_freeze
| Coq_prealloc_object_prevent_extensions
| Coq_prealloc_object_is_sealed
| Coq_prealloc_object_is_frozen
| Coq_prealloc_object_is_extensible
| Coq_prealloc_object_keys
| Coq_prealloc_object_keys_call
| Coq_prealloc_object_proto                                   (** ObjectPrototype *)
| Coq_prealloc_object_proto_to_string
| Coq_prealloc_object_proto_value_of
| Coq_prealloc_object_proto_has_own_prop
| Coq_prealloc_object_proto_is_prototype_of
| Coq_prealloc_object_proto_prop_is_enumerable
| Coq_prealloc_function
| Coq_prealloc_function_proto
| Coq_prealloc_function_proto_to_string
| Coq_prealloc_function_proto_apply
| Coq_prealloc_function_proto_call
| Coq_prealloc_function_proto_bind
| Coq_prealloc_bool
| Coq_prealloc_bool_proto
| Coq_prealloc_bool_proto_to_string
| Coq_prealloc_bool_proto_value_of
| Coq_prealloc_number
| Coq_prealloc_number_proto
| Coq_prealloc_number_proto_to_string
| Coq_prealloc_number_proto_value_of
| Coq_prealloc_number_proto_to_fixed
| Coq_prealloc_number_proto_to_exponential
| Coq_prealloc_number_proto_to_precision
| Coq_prealloc_array
| Coq_prealloc_array_is_array
| Coq_prealloc_array_proto
| Coq_prealloc_array_proto_to_string
| Coq_prealloc_array_proto_join
| Coq_prealloc_array_proto_pop
| Coq_prealloc_array_proto_push
| Coq_prealloc_string
| Coq_prealloc_string_proto
| Coq_prealloc_string_proto_to_string
| Coq_prealloc_string_proto_value_of
| Coq_prealloc_string_proto_char_at
| Coq_prealloc_string_proto_char_code_at
| Coq_prealloc_math
| Coq_prealloc_date
| Coq_prealloc_regexp
| Coq_prealloc_error
| Coq_prealloc_error_proto
| Coq_prealloc_error_proto_to_string
| Coq_prealloc_throw_type_error
| Coq_prealloc_json
| Coq_prealloc_proxy                                          (** Proxy *)
| Coq_prealloc_proxy_revocable
| Coq_prealloc_mathop of mathop [@f mathop]
| Coq_prealloc_native_error of native_error [@f error]
| Coq_prealloc_native_error_proto of native_error [@f error]




(** Identities of implementations for Object Internal Methods *)
type call =
| Coq_call_default
| Coq_call_after_bind
| Coq_call_prealloc of prealloc [@f prealloc]
| Coq_call_proxy

type construct =
| Coq_construct_default
| Coq_construct_after_bind
| Coq_construct_prealloc of prealloc [@f prealloc]
| Coq_construct_proxy

type builtin_has_instance =
| Coq_builtin_has_instance_function
| Coq_builtin_has_instance_after_bind

type builtin_get =
| Coq_builtin_get_default
| Coq_builtin_get_function
| Coq_builtin_get_args_obj
| Coq_builtin_get_proxy

type builtin_get_own_prop =
| Coq_builtin_get_own_prop_default
| Coq_builtin_get_own_prop_args_obj
| Coq_builtin_get_own_prop_string
| Coq_builtin_get_own_prop_proxy

(* FIXME: REMOVED IN ES7 *)
type builtin_get_prop =
| Coq_builtin_get_prop_default

(* FIXME: RENAMED TO SET IN ES7 *)
type builtin_put =
| Coq_builtin_put_default

(* FIXME: REMOVED IN ES7 *)
type builtin_can_put =
| Coq_builtin_can_put_default

type builtin_has_prop =
| Coq_builtin_has_prop_default
| Coq_builtin_has_prop_proxy

type builtin_delete =
| Coq_builtin_delete_default
| Coq_builtin_delete_args_obj
| Coq_builtin_delete_proxy

(* FIXME: REMOVED IN ES7 *)
type builtin_default_value =
| Coq_builtin_default_value_default

type builtin_define_own_prop =
| Coq_builtin_define_own_prop_default
| Coq_builtin_define_own_prop_array
| Coq_builtin_define_own_prop_args_obj
| Coq_builtin_define_own_prop_proxy

type builtin_get_prototype_of =
| Coq_builtin_get_prototype_of_default
| Coq_builtin_get_prototype_of_proxy

type builtin_set_prototype_of =
| Coq_builtin_set_prototype_of_default
| Coq_builtin_set_prototype_of_proxy

type builtin_is_extensible =
| Coq_builtin_is_extensible_default
| Coq_builtin_is_extensible_proxy

type builtin_prevent_extensions =
| Coq_builtin_prevent_extensions_default
| Coq_builtin_prevent_extensions_proxy

type builtin_set =
| Coq_builtin_set_default
| Coq_builtin_set_proxy

type builtin_own_property_keys =
| Coq_builtin_own_property_keys_default
| Coq_builtin_own_property_keys_proxy





type object_loc =
| Coq_object_loc_normal of int [@f address]
| Coq_object_loc_prealloc of prealloc [@f prealloc]

type prim =
| Coq_prim_undef
| Coq_prim_null
| Coq_prim_bool of bool [@f value]
| Coq_prim_number of JsNumber.number [@f value]
| Coq_prim_string of string [@f value]

type value =
| Coq_value_prim of prim [@f value]
| Coq_value_object of object_loc [@f value]

type coq_type =
| Coq_type_undef
| Coq_type_null
| Coq_type_bool
| Coq_type_number
| Coq_type_string
| Coq_type_object

type attributes_data = { attributes_data_value : value;
                         attributes_data_writable : bool;
                         attributes_data_enumerable : bool;
                         attributes_data_configurable : bool }

(** val attributes_data_value : attributes_data -> value **)

let attributes_data_value x = x.attributes_data_value

(** val attributes_data_writable : attributes_data -> bool **)

let attributes_data_writable x = x.attributes_data_writable

(** val attributes_data_enumerable : attributes_data -> bool **)

let attributes_data_enumerable x = x.attributes_data_enumerable

(** val attributes_data_configurable : attributes_data -> bool **)

let attributes_data_configurable x = x.attributes_data_configurable

type attributes_accessor = { attributes_accessor_get : value;
                             attributes_accessor_set : value;
                             attributes_accessor_enumerable : bool;
                             attributes_accessor_configurable : bool }

(** val attributes_accessor_get : attributes_accessor -> value **)

let attributes_accessor_get x = x.attributes_accessor_get

(** val attributes_accessor_set : attributes_accessor -> value **)

let attributes_accessor_set x = x.attributes_accessor_set

(** val attributes_accessor_enumerable : attributes_accessor -> bool **)

let attributes_accessor_enumerable x = x.attributes_accessor_enumerable

(** val attributes_accessor_configurable : attributes_accessor -> bool **)

let attributes_accessor_configurable x = x.attributes_accessor_configurable

type attributes =
| Coq_attributes_data_of of attributes_data [@f value]
| Coq_attributes_accessor_of of attributes_accessor [@f value]

type descriptor = { descriptor_value : value option;
                    descriptor_writable : bool option;
                    descriptor_get : value option;
                    descriptor_set : value option;
                    descriptor_enumerable : bool option;
                    descriptor_configurable : bool option }

(** val descriptor_value : descriptor -> value option **)

let descriptor_value x = x.descriptor_value

(** val descriptor_writable : descriptor -> bool option **)

let descriptor_writable x = x.descriptor_writable

(** val descriptor_get : descriptor -> value option **)

let descriptor_get x = x.descriptor_get

(** val descriptor_set : descriptor -> value option **)

let descriptor_set x = x.descriptor_set

(** val descriptor_enumerable : descriptor -> bool option **)

let descriptor_enumerable x = x.descriptor_enumerable

(** val descriptor_configurable : descriptor -> bool option **)

let descriptor_configurable x = x.descriptor_configurable

type full_descriptor =
| Coq_full_descriptor_undef
| Coq_full_descriptor_some of attributes [@f value]

type mutability =
| Coq_mutability_uninitialized_immutable
| Coq_mutability_immutable
| Coq_mutability_nondeletable
| Coq_mutability_deletable

type decl_env_record = (string, mutability * value) Heap.heap

type provide_this_flag = bool

type env_record =
| Coq_env_record_decl of decl_env_record [@f value]
| Coq_env_record_object of object_loc * provide_this_flag [@f value, provide_this]

type env_loc = int

(** val env_loc_global_env_record : env_loc **)

let env_loc_global_env_record = 0

type lexical_env = env_loc list

type execution_ctx = { execution_ctx_lexical_env : lexical_env;
                       execution_ctx_variable_env : lexical_env;
                       execution_ctx_this_binding : value;
                       execution_ctx_strict : strictness_flag }

(** val execution_ctx_lexical_env : execution_ctx -> lexical_env **)

let execution_ctx_lexical_env x = x.execution_ctx_lexical_env

(** val execution_ctx_variable_env : execution_ctx -> lexical_env **)

let execution_ctx_variable_env x = x.execution_ctx_variable_env

(** val execution_ctx_this_binding : execution_ctx -> value **)

let execution_ctx_this_binding x = x.execution_ctx_this_binding

(** val execution_ctx_strict : execution_ctx -> strictness_flag **)

let execution_ctx_strict x = x.execution_ctx_strict

type prop_name = string

type ref_base_type =
| Coq_ref_base_type_value of value [@f value]
| Coq_ref_base_type_env_loc of env_loc [@f value]

type ref = { ref_base : ref_base_type; ref_name : prop_name;
             ref_strict : bool }

(** val ref_base : ref -> ref_base_type **)

let ref_base x = x.ref_base

(** val ref_name : ref -> prop_name **)

let ref_name x = x.ref_name

(** val ref_strict : ref -> bool **)

let ref_strict x = x.ref_strict

type class_name = string

type object_properties_type = (prop_name, attributes) Heap.heap


(** Object type definition
    es-id: sec-object-type *)
(* FIXME: ES7 defines internal slots as being able to take the undefined value *)
type coq_object = { object_proto_ : value;
                    object_class_ : class_name;
                    object_extensible_ : bool;
                    object_prim_value_ : value option;
                    object_properties_ : object_properties_type;
                    object_get_prototype_of_ : builtin_get_prototype_of;
                    object_set_prototype_of_ : builtin_set_prototype_of;
                    object_is_extensible_ : builtin_is_extensible;
                    object_prevent_extensions_ : builtin_prevent_extensions;
                    object_get_own_prop_ : builtin_get_own_prop;
                    object_has_prop_ : builtin_has_prop;
                    object_get_ : builtin_get;
                    object_get_prop_ : builtin_get_prop;
                    object_put_ : builtin_put;
                    object_can_put_ : builtin_can_put;
                    object_delete_ : builtin_delete;
                    object_default_value_ : builtin_default_value;
                    object_define_own_prop_ : builtin_define_own_prop;
                    object_own_property_keys_ : builtin_own_property_keys;
                    object_construct_ : construct option;
                    object_call_ : call option;
                    object_has_instance_ : builtin_has_instance option;
                    object_scope_ : lexical_env option;
                    object_formal_parameters_ : string list option;
                    object_code_ : funcbody option;
                    object_target_function_ : object_loc option;
                    object_bound_this_ : value option;
                    object_bound_args_ : value list option;
                    object_parameter_map_ : object_loc option;
                    object_revocable_proxy_ : value option;
                    object_proxy_target_ : value option;
                    object_proxy_handler_ : value option }

(** val object_proto_ : coq_object -> value **)

let object_proto_ x = x.object_proto_
let object_prototype_ x = x.object_proto_

(** val object_class_ : coq_object -> class_name **)

let object_class_ x = x.object_class_

(** val object_extensible_ : coq_object -> bool **)

let object_extensible_ x = x.object_extensible_

(** val object_prim_value_ : coq_object -> value option **)

let object_prim_value_ x = x.object_prim_value_

(** val object_properties_ : coq_object -> object_properties_type **)

let object_properties_ x = x.object_properties_

let object_get_prototype_of_ x = x.object_get_prototype_of_
let object_set_prototype_of_ x = x.object_set_prototype_of_
let object_is_extensible_ x = x.object_is_extensible_
let object_prevent_extensions_ x = x.object_prevent_extensions_

(** val object_get_ : coq_object -> builtin_get **)

let object_get_ x = x.object_get_

(** val object_get_own_prop_ : coq_object -> builtin_get_own_prop **)

let object_get_own_prop_ x = x.object_get_own_prop_

(** val object_get_prop_ : coq_object -> builtin_get_prop **)

let object_get_prop_ x = x.object_get_prop_

(** val object_put_ : coq_object -> builtin_put **)

let object_put_ x = x.object_put_

(** val object_can_put_ : coq_object -> builtin_can_put **)

let object_can_put_ x = x.object_can_put_

(** val object_has_prop_ : coq_object -> builtin_has_prop **)

let object_has_prop_ x = x.object_has_prop_

(** val object_delete_ : coq_object -> builtin_delete **)

let object_delete_ x = x.object_delete_

(** val object_default_value_ : coq_object -> builtin_default_value **)

let object_default_value_ x = x.object_default_value_

(** val object_define_own_prop_ : coq_object -> builtin_define_own_prop **)

let object_define_own_prop_ x = x.object_define_own_prop_

(** val object_construct_ : coq_object -> construct option **)

let object_construct_ x = x.object_construct_

(** val object_call_ : coq_object -> call option **)

let object_call_ x = x.object_call_

(** val object_has_instance_ : coq_object -> builtin_has_instance option **)

let object_has_instance_ x = x.object_has_instance_

(** val object_scope_ : coq_object -> lexical_env option **)

let object_scope_ x = x.object_scope_

(** val object_formal_parameters_ : coq_object -> string list option **)

let object_formal_parameters_ x = x.object_formal_parameters_

(** val object_code_ : coq_object -> funcbody option **)

let object_code_ x = x.object_code_

(** val object_target_function_ : coq_object -> object_loc option **)

let object_target_function_ x = x.object_target_function_

(** val object_bound_this_ : coq_object -> value option **)

let object_bound_this_ x = x.object_bound_this_

(** val object_bound_args_ : coq_object -> value list option **)

let object_bound_args_ x = x.object_bound_args_

(** val object_parameter_map_ : coq_object -> object_loc option **)

let object_parameter_map_ x = x.object_parameter_map_

type event =
| Coq_delete_event of object_loc * prop_name * object_loc option [@f loc, name, locopt]
| Coq_mutateproto_event of object_loc * (object_loc * prop_name) list * (object_loc * prop_name) list [@f loc, fields]
| Coq_enumchange_event of object_loc * prop_name [@f loc, name]

type state = { state_object_heap : (object_loc, coq_object) Heap.heap;
               state_env_record_heap : (env_loc, env_record) Heap.heap;
               state_fresh_locations : int }

(** val state_object_heap : state -> (object_loc, coq_object) Heap.heap **)

let state_object_heap x = x.state_object_heap

(** val state_env_record_heap : state -> (env_loc, env_record) Heap.heap **)

let state_env_record_heap x = x.state_env_record_heap

type restype =
| Coq_restype_normal
| Coq_restype_break
| Coq_restype_continue
| Coq_restype_return
| Coq_restype_throw

type resvalue =
| Coq_resvalue_empty
| Coq_resvalue_value of value [@f value]
| Coq_resvalue_ref of ref [@f ref]

type res = { res_type : restype; res_value : resvalue; res_label : label }

(** val res_type : res -> restype **)

let res_type x = x.res_type

(** val res_value : res -> resvalue **)

let res_value x = x.res_value

(** val res_label : res -> label **)

let res_label x = x.res_label

(** val res_ref : ref -> res **)

let res_ref r =
  { res_type = Coq_restype_normal; res_value = (Coq_resvalue_ref r);
    res_label = Coq_label_empty }

(** val res_val : value -> res **)

let res_val v =
  { res_type = Coq_restype_normal; res_value = (Coq_resvalue_value v);
    res_label = Coq_label_empty }

(** val res_normal : resvalue -> res **)

let res_normal rv =
  { res_type = Coq_restype_normal; res_value = rv; res_label =
    Coq_label_empty }

(** val res_empty : res **)

let res_empty =
  { res_type = Coq_restype_normal; res_value = Coq_resvalue_empty;
    res_label = Coq_label_empty }

(** val res_break : label -> res **)

let res_break labo =
  { res_type = Coq_restype_break; res_value = Coq_resvalue_empty; res_label =
    labo }

(** val res_continue : label -> res **)

let res_continue labo =
  { res_type = Coq_restype_continue; res_value = Coq_resvalue_empty;
    res_label = labo }

(** val res_return : resvalue -> res **)

let res_return v =
  { res_type = Coq_restype_return; res_value = v; res_label =
    Coq_label_empty }

(** val res_throw : resvalue -> res **)

let res_throw v =
  { res_type = Coq_restype_throw; res_value = v; res_label =
    Coq_label_empty }

type out =
| Coq_out_div
| Coq_out_ter of state * res [@f state, res]

(** val out_void : state -> out **)

let out_void s =
  Coq_out_ter (s, res_empty)

type 't specret =
| Coq_specret_val of state * 't [@f state, res]
| Coq_specret_out of out [@f out]

type codetype =
| Coq_codetype_func
| Coq_codetype_global
| Coq_codetype_eval

(*
let thebound = 
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           ((fun p -> 1. +. (2. *. p))
                                           1.)))))))))))))))))))))))))))))))

*)
