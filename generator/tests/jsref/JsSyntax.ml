open JsNumber
open LibHeap
open LibReflect
open LibStream
open Shared

module Heap = HeapGen(HeapList)

type unary_op =
| Coq_unary_op_delete [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_void [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_typeof [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_post_incr [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_post_decr [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_pre_incr [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_pre_decr [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_add [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_neg [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_bitwise_not [@f]  (** Auto Generated Attributes **)
| Coq_unary_op_not [@f]  (** Auto Generated Attributes **)

type binary_op =
| Coq_binary_op_mult [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_div [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_mod [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_add [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_sub [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_left_shift [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_right_shift [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_unsigned_right_shift [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_lt [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_gt [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_le [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_ge [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_instanceof [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_in [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_equal [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_disequal [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_strict_equal [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_strict_disequal [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_bitwise_and [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_bitwise_or [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_bitwise_xor [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_and [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_or [@f]  (** Auto Generated Attributes **)
| Coq_binary_op_coma [@f]  (** Auto Generated Attributes **)

type literal =
| Coq_literal_null [@f]  (** Auto Generated Attributes **)
| Coq_literal_bool  [@f label0] of bool (** Auto Generated Attributes **)
| Coq_literal_number  [@f label0] of number (** Auto Generated Attributes **)
| Coq_literal_string  [@f label0] of string (** Auto Generated Attributes **)

type label =
| Coq_label_empty [@f]  (** Auto Generated Attributes **)
| Coq_label_string  [@f label0] of string (** Auto Generated Attributes **)

type label_set = label list

type strictness_flag = bool

(** val strictness_false : strictness_flag **)

let strictness_false =
  false

type propname =
| Coq_propname_identifier  [@f label0] of string (** Auto Generated Attributes **)
| Coq_propname_string  [@f label0] of string (** Auto Generated Attributes **)
| Coq_propname_number  [@f label0] of number (** Auto Generated Attributes **)

type expr =
| Coq_expr_this [@f]  (** Auto Generated Attributes **)
| Coq_expr_identifier  [@f label0] of string (** Auto Generated Attributes **)
| Coq_expr_literal  [@f label0] of literal (** Auto Generated Attributes **)
| Coq_expr_object  [@f label0] of (propname * propbody) list (** Auto Generated Attributes **)
| Coq_expr_array  [@f label0] of expr option list (** Auto Generated Attributes **)
| Coq_expr_function  [@f label0, label1, label2] of string option * string list * funcbody (** Auto Generated Attributes **)
| Coq_expr_access  [@f label0, label1] of expr * expr (** Auto Generated Attributes **)
| Coq_expr_member  [@f label0, label1] of expr * string (** Auto Generated Attributes **)
| Coq_expr_new  [@f label0, label1] of expr * expr list (** Auto Generated Attributes **)
| Coq_expr_call  [@f label0, label1] of expr * expr list (** Auto Generated Attributes **)
| Coq_expr_unary_op  [@f label0, label1] of unary_op * expr (** Auto Generated Attributes **)
| Coq_expr_binary_op  [@f label0, label1, label2] of expr * binary_op * expr (** Auto Generated Attributes **)
| Coq_expr_conditional  [@f label0, label1, label2] of expr * expr * expr (** Auto Generated Attributes **)
| Coq_expr_assign  [@f label0, label1, label2] of expr * binary_op option * expr (** Auto Generated Attributes **)
and propbody =
| Coq_propbody_val  [@f label0] of expr (** Auto Generated Attributes **)
| Coq_propbody_get  [@f label0] of funcbody (** Auto Generated Attributes **)
| Coq_propbody_set  [@f label0, label1] of string list * funcbody (** Auto Generated Attributes **)
and funcbody =
| Coq_funcbody_intro  [@f label0, label1] of prog * string (** Auto Generated Attributes **)
and stat =
| Coq_stat_expr  [@f label0] of expr (** Auto Generated Attributes **)
| Coq_stat_label  [@f label0, label1] of string * stat (** Auto Generated Attributes **)
| Coq_stat_block  [@f label0] of stat list (** Auto Generated Attributes **)
| Coq_stat_var_decl  [@f label0] of (string * expr option) list (** Auto Generated Attributes **)
| Coq_stat_if  [@f label0, label1, label2] of expr * stat * stat option (** Auto Generated Attributes **)
| Coq_stat_do_while  [@f label0, label1, label2] of label_set * stat * expr (** Auto Generated Attributes **)
| Coq_stat_while  [@f label0, label1, label2] of label_set * expr * stat (** Auto Generated Attributes **)
| Coq_stat_with  [@f label0, label1] of expr * stat (** Auto Generated Attributes **)
| Coq_stat_throw  [@f label0] of expr (** Auto Generated Attributes **)
| Coq_stat_return  [@f label0] of expr option (** Auto Generated Attributes **)
| Coq_stat_break  [@f label0] of label (** Auto Generated Attributes **)
| Coq_stat_continue  [@f label0] of label (** Auto Generated Attributes **)
| Coq_stat_try  [@f label0, label1, label2] of stat * (string * stat) option * stat option (** Auto Generated Attributes **)
| Coq_stat_for  [@f label0, label1, label2, label3, label4] of label_set * expr option * expr option * expr option * stat (** Auto Generated Attributes **)
| Coq_stat_for_var  [@f label0, label1, label2, label3, label4] of label_set * (string * expr option) list * expr option * expr option * stat (** Auto Generated Attributes **)
| Coq_stat_for_in  [@f label0, label1, label2, label3] of label_set * expr * expr * stat (** Auto Generated Attributes **)
| Coq_stat_for_in_var  [@f label0, label1, label2, label3, label4] of label_set * string * expr option * expr * stat (** Auto Generated Attributes **)
| Coq_stat_debugger [@f]  (** Auto Generated Attributes **)
| Coq_stat_switch  [@f label0, label1, label2] of label_set * expr * switchbody (** Auto Generated Attributes **)
and switchbody =
| Coq_switchbody_nodefault  [@f label0] of switchclause list (** Auto Generated Attributes **)
| Coq_switchbody_withdefault  [@f label0, label1, label2] of switchclause list * stat list * switchclause list (** Auto Generated Attributes **)
and switchclause =
| Coq_switchclause_intro  [@f label0, label1] of expr * stat list (** Auto Generated Attributes **)
and prog =
| Coq_prog_intro  [@f label0, label1] of strictness_flag * element list (** Auto Generated Attributes **)
and element =
| Coq_element_stat  [@f label0] of stat (** Auto Generated Attributes **)
| Coq_element_func_decl  [@f label0, label1, label2] of string * string list * funcbody (** Auto Generated Attributes **)

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
| Coq_mathop_abs [@f]  (** Auto Generated Attributes **)

type native_error =
| Coq_native_error_eval [@f]  (** Auto Generated Attributes **)
| Coq_native_error_range [@f]  (** Auto Generated Attributes **)
| Coq_native_error_ref [@f]  (** Auto Generated Attributes **)
| Coq_native_error_syntax [@f]  (** Auto Generated Attributes **)
| Coq_native_error_type [@f]  (** Auto Generated Attributes **)
| Coq_native_error_uri [@f]  (** Auto Generated Attributes **)

type prealloc =
| Coq_prealloc_global [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_eval [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_parse_int [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_parse_float [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_is_finite [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_is_nan [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_decode_uri [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_decode_uri_component [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_encode_uri [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_global_encode_uri_component [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_get_proto_of [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_get_own_prop_descriptor [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_get_own_prop_name [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_create [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_define_prop [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_define_props [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_seal [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_freeze [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_prevent_extensions [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_is_sealed [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_is_frozen [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_is_extensible [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_keys [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_keys_call [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_proto_value_of [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_proto_has_own_prop [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_proto_is_prototype_of [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_object_proto_prop_is_enumerable [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_function [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_function_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_function_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_function_proto_apply [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_function_proto_call [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_function_proto_bind [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_bool [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_bool_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_bool_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_bool_proto_value_of [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number_proto_value_of [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number_proto_to_fixed [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number_proto_to_exponential [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_number_proto_to_precision [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array_is_array [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array_proto_join [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array_proto_pop [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_array_proto_push [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_string_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_string_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_string_proto_value_of [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_string_proto_char_at [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_string_proto_char_code_at [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_math [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_mathop  [@f label0] of mathop (** Auto Generated Attributes **)
| Coq_prealloc_date [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_regexp [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_error [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_error_proto [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_native_error  [@f label0] of native_error (** Auto Generated Attributes **)
| Coq_prealloc_native_error_proto  [@f label0] of native_error (** Auto Generated Attributes **)
| Coq_prealloc_error_proto_to_string [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_throw_type_error [@f]  (** Auto Generated Attributes **)
| Coq_prealloc_json [@f]  (** Auto Generated Attributes **)

type call =
| Coq_call_default [@f]  (** Auto Generated Attributes **)
| Coq_call_after_bind [@f]  (** Auto Generated Attributes **)
| Coq_call_prealloc  [@f label0] of prealloc (** Auto Generated Attributes **)

type construct =
| Coq_construct_default [@f]  (** Auto Generated Attributes **)
| Coq_construct_after_bind [@f]  (** Auto Generated Attributes **)
| Coq_construct_prealloc  [@f label0] of prealloc (** Auto Generated Attributes **)

type builtin_has_instance =
| Coq_builtin_has_instance_function [@f]  (** Auto Generated Attributes **)
| Coq_builtin_has_instance_after_bind [@f]  (** Auto Generated Attributes **)

type builtin_get =
| Coq_builtin_get_default [@f]  (** Auto Generated Attributes **)
| Coq_builtin_get_function [@f]  (** Auto Generated Attributes **)
| Coq_builtin_get_args_obj [@f]  (** Auto Generated Attributes **)

type builtin_get_own_prop =
| Coq_builtin_get_own_prop_default [@f]  (** Auto Generated Attributes **)
| Coq_builtin_get_own_prop_args_obj [@f]  (** Auto Generated Attributes **)
| Coq_builtin_get_own_prop_string [@f]  (** Auto Generated Attributes **)

type builtin_get_prop =
| Coq_builtin_get_prop_default [@f]  (** Auto Generated Attributes **)

type builtin_put =
| Coq_builtin_put_default [@f]  (** Auto Generated Attributes **)

type builtin_can_put =
| Coq_builtin_can_put_default [@f]  (** Auto Generated Attributes **)

type builtin_has_prop =
| Coq_builtin_has_prop_default [@f]  (** Auto Generated Attributes **)

type builtin_delete =
| Coq_builtin_delete_default [@f]  (** Auto Generated Attributes **)
| Coq_builtin_delete_args_obj [@f]  (** Auto Generated Attributes **)

type builtin_default_value =
| Coq_builtin_default_value_default [@f]  (** Auto Generated Attributes **)

type builtin_define_own_prop =
| Coq_builtin_define_own_prop_default [@f]  (** Auto Generated Attributes **)
| Coq_builtin_define_own_prop_array [@f]  (** Auto Generated Attributes **)
| Coq_builtin_define_own_prop_args_obj [@f]  (** Auto Generated Attributes **)

type object_loc =
| Coq_object_loc_normal  [@f label0] of int (** Auto Generated Attributes **)
| Coq_object_loc_prealloc  [@f label0] of prealloc (** Auto Generated Attributes **)

type prim =
| Coq_prim_undef [@f]  (** Auto Generated Attributes **)
| Coq_prim_null [@f]  (** Auto Generated Attributes **)
| Coq_prim_bool  [@f label0] of bool (** Auto Generated Attributes **)
| Coq_prim_number  [@f label0] of number (** Auto Generated Attributes **)
| Coq_prim_string  [@f label0] of string (** Auto Generated Attributes **)

type value =
| Coq_value_prim  [@f label0] of prim (** Auto Generated Attributes **)
| Coq_value_object  [@f label0] of object_loc (** Auto Generated Attributes **)

type coq_type =
| Coq_type_undef [@f]  (** Auto Generated Attributes **)
| Coq_type_null [@f]  (** Auto Generated Attributes **)
| Coq_type_bool [@f]  (** Auto Generated Attributes **)
| Coq_type_number [@f]  (** Auto Generated Attributes **)
| Coq_type_string [@f]  (** Auto Generated Attributes **)
| Coq_type_object [@f]  (** Auto Generated Attributes **)

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
| Coq_attributes_data_of [@f label0] of attributes_data (** Auto Generated Attributes **)
| Coq_attributes_accessor_of [@f label0] of attributes_accessor (** Auto Generated Attributes **)

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
| Coq_full_descriptor_undef [@f]  (** Auto Generated Attributes **)
| Coq_full_descriptor_some  [@f label0] of attributes (** Auto Generated Attributes **)

type mutability =
| Coq_mutability_uninitialized_immutable [@f]  (** Auto Generated Attributes **)
| Coq_mutability_immutable [@f]  (** Auto Generated Attributes **)
| Coq_mutability_nondeletable [@f]  (** Auto Generated Attributes **)
| Coq_mutability_deletable [@f]  (** Auto Generated Attributes **)

type decl_env_record = (string, mutability * value) Heap.heap

type provide_this_flag = bool

type env_record =
| Coq_env_record_decl  [@f label0] of decl_env_record (** Auto Generated Attributes **)
| Coq_env_record_object  [@f label0, label1] of object_loc * provide_this_flag (** Auto Generated Attributes **)

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
| Coq_ref_base_type_value  [@f label0] of value (** Auto Generated Attributes **)
| Coq_ref_base_type_env_loc  [@f label0] of env_loc (** Auto Generated Attributes **)

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

type coq_object = { object_proto_ : value; object_class_ : class_name;
                    object_extensible_ : bool;
                    object_prim_value_ : value option;
                    object_properties_ : object_properties_type;
                    object_get_ : builtin_get;
                    object_get_own_prop_ : builtin_get_own_prop;
                    object_get_prop_ : builtin_get_prop;
                    object_put_ : builtin_put;
                    object_can_put_ : builtin_can_put;
                    object_has_prop_ : builtin_has_prop;
                    object_delete_ : builtin_delete;
                    object_default_value_ : builtin_default_value;
                    object_define_own_prop_ : builtin_define_own_prop;
                    object_construct_ : construct option;
                    object_call_ : call option;
                    object_has_instance_ : builtin_has_instance option;
                    object_scope_ : lexical_env option;
                    object_formal_parameters_ : string list option;
                    object_code_ : funcbody option;
                    object_target_function_ : object_loc option;
                    object_bound_this_ : value option;
                    object_bound_args_ : value list option;
                    object_parameter_map_ : object_loc option }

(** val object_proto_ : coq_object -> value **)

let object_proto_ x = x.object_proto_

(** val object_class_ : coq_object -> class_name **)

let object_class_ x = x.object_class_

(** val object_extensible_ : coq_object -> bool **)

let object_extensible_ x = x.object_extensible_

(** val object_prim_value_ : coq_object -> value option **)

let object_prim_value_ x = x.object_prim_value_

(** val object_properties_ : coq_object -> object_properties_type **)

let object_properties_ x = x.object_properties_

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
| Coq_delete_event  [@f label0, label1, label2] of object_loc * prop_name * object_loc option (** Auto Generated Attributes **)
| Coq_mutateproto_event  [@f label0, label1] of object_loc * (object_loc * prop_name) list (** Auto Generated Attributes **)
   * (object_loc * prop_name) list
| Coq_enumchange_event  [@f label0, label1] of object_loc * prop_name (** Auto Generated Attributes **)

type state = { state_object_heap : (object_loc, coq_object) Heap.heap;
               state_env_record_heap : (env_loc, env_record) Heap.heap;
               state_fresh_locations : int;
               state_event_list : event list }

(** val state_object_heap : state -> (object_loc, coq_object) Heap.heap **)

let state_object_heap x = x.state_object_heap

(** val state_env_record_heap : state -> (env_loc, env_record) Heap.heap **)

let state_env_record_heap x = x.state_env_record_heap

type restype =
| Coq_restype_normal [@f]  (** Auto Generated Attributes **)
| Coq_restype_break [@f]  (** Auto Generated Attributes **)
| Coq_restype_continue [@f]  (** Auto Generated Attributes **)
| Coq_restype_return [@f]  (** Auto Generated Attributes **)
| Coq_restype_throw [@f]  (** Auto Generated Attributes **)

type resvalue =
| Coq_resvalue_empty [@f]  (** Auto Generated Attributes **)
| Coq_resvalue_value  [@f label0] of value (** Auto Generated Attributes **)
| Coq_resvalue_ref  [@f label0] of ref (** Auto Generated Attributes **)

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
| Coq_out_div [@f]  (** Auto Generated Attributes **)
| Coq_out_ter  [@f label0, label1] of state * res (** Auto Generated Attributes **)

(** val out_void : state -> out **)

let out_void s =
  Coq_out_ter (s, res_empty)

type 't specret =
| Coq_specret_val  [@f label0, label1] of state * 't (** Auto Generated Attributes **)
| Coq_specret_out  [@f label0] of out (** Auto Generated Attributes **)

type codetype =
| Coq_codetype_func [@f]  (** Auto Generated Attributes **)
| Coq_codetype_global [@f]  (** Auto Generated Attributes **)
| Coq_codetype_eval [@f]  (** Auto Generated Attributes **)

