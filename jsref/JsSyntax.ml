(*open JsNumber*)
open Heap
open Shared
open LibOption

type unary_op =
| Unary_op_delete
| Unary_op_void
| Unary_op_typeof
| Unary_op_post_incr
| Unary_op_post_decr
| Unary_op_pre_incr
| Unary_op_pre_decr
| Unary_op_add
| Unary_op_neg
| Unary_op_bitwise_not
| Unary_op_not

type binary_op =
| Binary_op_mult
| Binary_op_div
| Binary_op_mod
| Binary_op_add
| Binary_op_sub
| Binary_op_left_shift
| Binary_op_right_shift
| Binary_op_unsigned_right_shift
| Binary_op_lt
| Binary_op_gt
| Binary_op_le
| Binary_op_ge
| Binary_op_instanceof
| Binary_op_in
| Binary_op_equal
| Binary_op_disequal
| Binary_op_strict_equal
| Binary_op_strict_disequal
| Binary_op_bitwise_and
| Binary_op_bitwise_or
| Binary_op_bitwise_xor
| Binary_op_and
| Binary_op_or
| Binary_op_coma

type literal =
| Literal_null
| Literal_bool of bool [@f value]
| Literal_number of JsNumber.number [@f value]
| Literal_string of string [@f value]

type label =
| Label_empty
| Label_string of string [@f value]

type label_set = label list

type strictness_flag = bool

(** val strictness_false : strictness_flag **)

let strictness_false =
  false

type propname =
| Propname_identifier of string [@f value]
| Propname_string of string [@f value]
| Propname_number of JsNumber.number [@f value]

type expr =
| Expr_this
| Expr_identifier of string [@f name]
| Expr_literal of literal [@f value]
| Expr_object of (propname * propbody) list [@f fields]
| Expr_array of expr option list [@f elements]
| Expr_function of string option * string list * funcbody [@f func_name_opt, arg_names, body]
| Expr_access of expr * expr [@f obj, field]
| Expr_member of expr * string [@f obj, field_name]
| Expr_new of expr * expr list [@f func, args]
| Expr_call of expr * expr list [@f func, args]
| Expr_unary_op of unary_op * expr [@f op, arg]
| Expr_binary_op of expr * binary_op * expr [@f arg1, op, arg2]
| Expr_conditional of expr * expr * expr [@f cond, then_branch, else_branch]
| Expr_assign of expr * binary_op option * expr [@f left_expr, op_opt, right_expr]
and propbody =
| Propbody_val of expr [@f expr]
| Propbody_get of funcbody [@f body]
| Propbody_set of string list * funcbody [@f names, body]
and funcbody =
| Funcbody_intro of prog * string [@f prog, source]
and stat =
| Stat_expr of expr [@f expr]
| Stat_label of string * stat [@f label, stat]
| Stat_block of stat list [@f stats]
| Stat_var_decl of (string * expr option) list [@f decls]
| Stat_if of expr * stat * stat option [@f cond, then_branch, else_branch]
| Stat_do_while of label_set * stat * expr [@f labels, body, cond]
| Stat_while of label_set * expr * stat [@f labels, cond, body]
| Stat_with of expr * stat [@f obj, stat]
| Stat_throw of expr [@f arg]
| Stat_return of expr option [@f arg_opt]
| Stat_break of label [@f label]
| Stat_continue of label [@f label]
| Stat_try of stat * (string * stat) option * stat option [@f body, catch_stats_opt, finally_opt]
| Stat_for of label_set * expr option * expr option * expr option * stat [@f labels, init, cond, step, body]
| Stat_for_var of label_set * (string * expr option) list * expr option * expr option * stat [@f labels, init, cond, step, body]
| Stat_for_in of label_set * expr * expr * stat [@f labels, id, obj, body]
| Stat_for_in_var of label_set * string * expr option * expr * stat [@f labels, id, init, obj, body]
| Stat_debugger
| Stat_switch of label_set * expr * switchbody [@f labels, arg, body]
and switchbody =
| Switchbody_nodefault of switchclause list [@f clauses]
| Switchbody_withdefault of switchclause list * stat list * switchclause list [@f clauses_before, clause_default, clauses_after]
and switchclause =
| Switchclause_intro of expr * stat list [@f arg, stats]
and prog =
| Prog_intro of strictness_flag * element list [@f strictness, elements]
and element =
| Element_stat of stat [@f stat]
| Element_func_decl of string * string list * funcbody [@f func_name, arg_names, body]

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
| Mathop_abs

type native_error =
| Native_error_eval
| Native_error_range
| Native_error_ref
| Native_error_syntax
| Native_error_type
| Native_error_uri

(** Intrinsic Objects *)
type prealloc =
| Prealloc_global
| Prealloc_global_eval
| Prealloc_global_parse_int
| Prealloc_global_parse_float
| Prealloc_global_is_finite
| Prealloc_global_is_nan
| Prealloc_global_decode_uri
| Prealloc_global_decode_uri_component
| Prealloc_global_encode_uri
| Prealloc_global_encode_uri_component
| Prealloc_object
| Prealloc_object_get_proto_of
| Prealloc_object_get_own_prop_descriptor
| Prealloc_object_get_own_prop_name
| Prealloc_object_set_proto_of
| Prealloc_object_create
| Prealloc_object_define_prop
| Prealloc_object_define_props
| Prealloc_object_seal
| Prealloc_object_freeze
| Prealloc_object_prevent_extensions
| Prealloc_object_is_sealed
| Prealloc_object_is_frozen
| Prealloc_object_is_extensible
| Prealloc_object_keys
| Prealloc_object_proto                                   (** ObjectPrototype *)
| Prealloc_object_proto_to_string
| Prealloc_object_proto_value_of
| Prealloc_object_proto_has_own_prop
| Prealloc_object_proto_is_prototype_of
| Prealloc_object_proto_prop_is_enumerable
| Prealloc_function
| Prealloc_function_proto
| Prealloc_function_proto_to_string
| Prealloc_function_proto_apply
| Prealloc_function_proto_call
| Prealloc_function_proto_bind
| Prealloc_bool
| Prealloc_bool_proto
| Prealloc_bool_proto_to_string
| Prealloc_bool_proto_value_of
| Prealloc_number
| Prealloc_number_proto
| Prealloc_number_proto_to_string
| Prealloc_number_proto_value_of
| Prealloc_number_proto_to_fixed
| Prealloc_number_proto_to_exponential
| Prealloc_number_proto_to_precision
| Prealloc_array
| Prealloc_array_is_array
| Prealloc_array_proto
| Prealloc_array_proto_to_string
| Prealloc_array_proto_join
| Prealloc_array_proto_pop
| Prealloc_array_proto_push
| Prealloc_string
| Prealloc_string_proto
| Prealloc_string_proto_to_string
| Prealloc_string_proto_value_of
| Prealloc_string_proto_char_at
| Prealloc_string_proto_char_code_at
| Prealloc_math
| Prealloc_date
| Prealloc_regexp
| Prealloc_error
| Prealloc_error_proto
| Prealloc_error_proto_to_string
| Prealloc_throw_type_error
| Prealloc_json
| Prealloc_proxy                                          (** Proxy *)
| Prealloc_proxy_revocable
| Builtin_proxy_revocation
| Prealloc_reflect                                        (** Reflect *)
| Prealloc_reflect_apply
| Prealloc_reflect_construct
| Prealloc_reflect_define_property
| Prealloc_reflect_delete_property
| Prealloc_reflect_get
| Prealloc_reflect_get_own_property_descriptor
| Prealloc_reflect_get_prototype_of
| Prealloc_reflect_has
| Prealloc_reflect_is_extensible
| Prealloc_reflect_own_keys
| Prealloc_reflect_prevent_extensions
| Prealloc_reflect_set
| Prealloc_reflect_set_prototype_of
| Prealloc_mathop of mathop [@f mathop]
| Prealloc_native_error of native_error [@f error]
| Prealloc_native_error_proto of native_error [@f error]




(** Identities of implementations for Object Internal Methods *)
type call =
| Call_default
| Call_after_bind
| Call_prealloc of prealloc [@f prealloc]
| Call_proxy

type construct =
| Construct_default
| Construct_after_bind
| Construct_prealloc of prealloc [@f prealloc]
| Construct_proxy

type builtin_has_instance =
| Builtin_has_instance_function
| Builtin_has_instance_after_bind

type builtin_get =
| Builtin_get_default
| Builtin_get_args_obj
| Builtin_get_proxy

type builtin_get_own_prop =
| Builtin_get_own_prop_default
| Builtin_get_own_prop_args_obj
| Builtin_get_own_prop_string
| Builtin_get_own_prop_proxy

(* FIXME: REMOVED IN ES7 *)
type builtin_get_prop =
| Builtin_get_prop_default

type builtin_has_prop =
| Builtin_has_prop_default
| Builtin_has_prop_proxy

type builtin_delete =
| Builtin_delete_default
| Builtin_delete_args_obj
| Builtin_delete_proxy

(* FIXME: REMOVED IN ES7 *)
type builtin_default_value =
| Builtin_default_value_default

type builtin_define_own_prop =
| Builtin_define_own_prop_default
| Builtin_define_own_prop_array
| Builtin_define_own_prop_args_obj
| Builtin_define_own_prop_proxy

type builtin_get_prototype_of =
| Builtin_get_prototype_of_default
| Builtin_get_prototype_of_proxy

type builtin_set_prototype_of =
| Builtin_set_prototype_of_default
| Builtin_set_prototype_of_proxy

type builtin_is_extensible =
| Builtin_is_extensible_default
| Builtin_is_extensible_proxy

type builtin_prevent_extensions =
| Builtin_prevent_extensions_default
| Builtin_prevent_extensions_proxy

type builtin_set =
| Builtin_set_default
| Builtin_set_proxy

type builtin_own_property_keys =
| Builtin_own_property_keys_default
| Builtin_own_property_keys_proxy





type object_loc =
| Object_loc_normal of int [@f address]
| Object_loc_prealloc of prealloc [@f prealloc]

type value =
| Value_undef
| Value_null
| Value_bool of bool [@f value]
| Value_number of JsNumber.number [@f value]
| Value_string of string [@f value]
| Value_object of object_loc [@f value]

type coq_type =
| Type_undef
| Type_null
| Type_bool
| Type_number
| Type_string
| Type_object

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
| Attributes_data_of of attributes_data [@f value]
| Attributes_accessor_of of attributes_accessor [@f value]

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

(** Representing the option type [undefined | descriptor]
  used when undefined returned from [[GetOwnProperty]] *)
type undef_descriptor =
| Descriptor_undef
| Descriptor of descriptor [@f descriptor]

(** @deprecated Raw attributes should not be returned by GetOwnProperty *)
type full_descriptor =
| Full_descriptor_undef
| Full_descriptor_some of attributes [@f value]


type mutability =
| Mutability_uninitialized_immutable
| Mutability_immutable
| Mutability_nondeletable
| Mutability_deletable

type decl_env_record = (string, mutability * value) Heap.heap

type provide_this_flag = bool

type env_record =
| Env_record_decl of decl_env_record [@f value]
| Env_record_object of object_loc * provide_this_flag [@f value, provide_this]

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


(** {3 Reference Specification Type (Definitions)}
    @essec 6.2.4
    @esid sec-reference-specification-type *)
type prop_name = string

type ref_base_type =
| Ref_base_type_value of value [@f value]
| Ref_base_type_env_loc of env_loc [@f value]

type ref = { ref_base : ref_base_type;
             ref_name : prop_name;
             ref_strict : bool;
             (** A ref with a thisValue shall never have a base type of env_loc *)
             ref_this_value : value option
           }

(** val ref_base : ref -> ref_base_type **)

let ref_base x = x.ref_base

(** val ref_name : ref -> prop_name **)

let ref_name x = x.ref_name

(** val ref_strict : ref -> bool **)

let ref_strict x = x.ref_strict

let ref_this_value x =
  unsome_error x.ref_this_value

type class_name = string

type object_properties_type = (prop_name, attributes) Heap.heap


(** Object type definition
    @esid: sec-object-type *)
(* FIXME: ES7 defines internal slots as being able to take the undefined value *)
(* FIXME: prototype, extensible are now optional slots. class, default_value, others to be removed. *)
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
                    object_set_ : builtin_set;
                    object_get_prop_ : builtin_get_prop;
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

(* Object projection functions *)
let object_proto_ x = x.object_proto_
let object_prototype_ x = x.object_proto_
let object_class_ x = x.object_class_
let object_extensible_ x = x.object_extensible_
let object_prim_value_ x = x.object_prim_value_
let object_properties_ x = x.object_properties_
let object_get_prototype_of_ x = x.object_get_prototype_of_
let object_set_prototype_of_ x = x.object_set_prototype_of_
let object_is_extensible_ x = x.object_is_extensible_
let object_prevent_extensions_ x = x.object_prevent_extensions_
let object_get_ x = x.object_get_
let object_set_ x = x.object_set_
let object_get_own_prop_ x = x.object_get_own_prop_
let object_get_prop_ x = x.object_get_prop_
let object_has_prop_ x = x.object_has_prop_
let object_delete_ x = x.object_delete_
let object_default_value_ x = x.object_default_value_
let object_define_own_prop_ x = x.object_define_own_prop_
let object_own_property_keys_ x = x.object_own_property_keys_
let object_construct_ x = x.object_construct_
let object_call_ x = x.object_call_
let object_has_instance_ x = x.object_has_instance_
let object_scope_ x = x.object_scope_
let object_formal_parameters_ x = x.object_formal_parameters_
let object_code_ x = x.object_code_
let object_target_function_ x = x.object_target_function_
let object_bound_this_ x = x.object_bound_this_
let object_bound_args_ x = x.object_bound_args_
let object_parameter_map_ x = x.object_parameter_map_
let object_revocable_proxy_ x = x.object_revocable_proxy_
let object_proxy_target_ x = x.object_proxy_target_
let object_proxy_handler_ x = x.object_proxy_handler_

type event =
| Delete_event of object_loc * prop_name * object_loc option [@f loc, name, locopt]
| Mutateproto_event of object_loc * (object_loc * prop_name) list * (object_loc * prop_name) list [@f loc, fields]
| Enumchange_event of object_loc * prop_name [@f loc, name]

type state = { state_object_heap : (object_loc, coq_object) Heap.heap;
               state_env_record_heap : (env_loc, env_record) Heap.heap;
               state_fresh_locations : int }

(** val state_object_heap : state -> (object_loc, coq_object) Heap.heap **)

let state_object_heap x = x.state_object_heap

(** val state_env_record_heap : state -> (env_loc, env_record) Heap.heap **)

let state_env_record_heap x = x.state_env_record_heap

type restype =
| Restype_normal
| Restype_break
| Restype_continue
| Restype_return
| Restype_throw

type resvalue =
| Resvalue_empty
| Resvalue_value of value [@f value]
| Resvalue_ref of ref [@f ref]

type resvalue_type =
| Type_resvalue_empty
| Type_resvalue_value
| Type_resvalue_ref

type res = { res_type : restype; res_value : resvalue; res_label : label }

(** val res_type : res -> restype **)

let res_type x = x.res_type

(** val res_value : res -> resvalue **)

let res_value x = x.res_value

(** val res_label : res -> label **)

let res_label x = x.res_label

(** val res_ref : ref -> res **)

let res_ref r =
  { res_type = Restype_normal; res_value = (Resvalue_ref r);
    res_label = Label_empty }

(** val res_val : value -> res **)

let res_val v =
  { res_type = Restype_normal; res_value = (Resvalue_value v);
    res_label = Label_empty }

(** val res_normal : resvalue -> res **)

let res_normal rv =
  { res_type = Restype_normal; res_value = rv; res_label =
    Label_empty }

(** val res_empty : res **)

let res_empty =
  { res_type = Restype_normal; res_value = Resvalue_empty;
    res_label = Label_empty }

(** val res_break : label -> res **)

let res_break labo =
  { res_type = Restype_break; res_value = Resvalue_empty; res_label =
    labo }

(** val res_continue : label -> res **)

let res_continue labo =
  { res_type = Restype_continue; res_value = Resvalue_empty;
    res_label = labo }

(** val res_return : resvalue -> res **)

let res_return v =
  { res_type = Restype_return; res_value = v; res_label =
    Label_empty }

(** val res_throw : resvalue -> res **)

let res_throw v =
  { res_type = Restype_throw; res_value = v; res_label =
    Label_empty }

(** Return types from specification functions *)
type 't specret =
| Specret_val of state * 't [@f state, value] (** A pure/specification value *)
| Specret_out of state * res [@f state, res]  (** A completion record (possibly abrupt) *)

type codetype =
| Codetype_func
| Codetype_global
| Codetype_eval
