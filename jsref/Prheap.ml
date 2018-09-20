open Shared
open JsSyntax
open JsInit

let prbool = function
	| true -> "true"
	| false -> "false"

let proption f = function
	| None -> "None"
	| Some x -> "Some (" ^ f x ^ ")"

let prprealloc = function
  | Prealloc_global -> "Prealloc_global"
  | Prealloc_global_eval -> "Prealloc_global_eval"
  | Prealloc_global_parse_int -> "Prealloc_global_parse_int"
  | Prealloc_global_parse_float -> "Prealloc_global_parse_float"
  | Prealloc_global_is_finite -> "Prealloc_global_is_finite"
  | Prealloc_global_is_nan -> "Prealloc_global_is_nan"
  | Prealloc_global_decode_uri -> "Prealloc_global_decode_uri"
  | Prealloc_global_decode_uri_component -> "Prealloc_global_decode_uri_component"
  | Prealloc_global_encode_uri -> "Prealloc_global_encode_uri"
  | Prealloc_global_encode_uri_component -> "Prealloc_global_encode_uri_component"
  | Prealloc_object -> "Prealloc_object"
  | Prealloc_object_get_proto_of -> "Prealloc_object_get_proto_of"
  | Prealloc_object_get_own_prop_descriptor -> "Prealloc_object_get_own_prop_descriptor"
  | Prealloc_object_get_own_prop_name -> "Prealloc_object_get_own_prop_name"
  | Prealloc_object_create -> "Prealloc_object_create"
  | Prealloc_object_define_prop -> "Prealloc_object_define_prop"
  | Prealloc_object_define_props -> "Prealloc_object_define_props"
  | Prealloc_object_seal -> "Prealloc_object_seal"
  | Prealloc_object_freeze -> "Prealloc_object_freeze"
  | Prealloc_object_prevent_extensions -> "Prealloc_object_prevent_extensions"
  | Prealloc_object_is_sealed -> "Prealloc_object_is_sealed"
  | Prealloc_object_is_frozen -> "Prealloc_object_is_frozen"
  | Prealloc_object_is_extensible -> "Prealloc_object_is_extensible"
  | Prealloc_object_keys -> "Prealloc_object_keys"
  | Prealloc_object_proto -> "Prealloc_object_proto"
  | Prealloc_object_proto_to_string -> "Prealloc_object_proto_to_string"
  | Prealloc_object_proto_value_of -> "Prealloc_object_proto_value_of"
  | Prealloc_object_proto_has_own_prop -> "Prealloc_object_proto_has_own_prop"
  | Prealloc_object_proto_is_prototype_of -> "Prealloc_object_proto_is_prototype_of"
  | Prealloc_object_proto_prop_is_enumerable -> "Prealloc_object_proto_prop_is_enumerable"
  | Prealloc_function -> "Prealloc_function"
  | Prealloc_function_proto -> "Prealloc_function_proto"
  | Prealloc_function_proto_to_string -> "Prealloc_function_proto_to_string"
  | Prealloc_function_proto_apply -> "Prealloc_function_proto_apply"
  | Prealloc_function_proto_bind -> "Prealloc_function_proto_bind"
  | Prealloc_function_proto_call -> "Prealloc_function_proto_call"
  | Prealloc_bool -> "Prealloc_bool"
  | Prealloc_bool_proto -> "Prealloc_bool_proto"
  | Prealloc_bool_proto_to_string -> "Prealloc_bool_proto_to_string"
  | Prealloc_bool_proto_value_of -> "Prealloc_bool_proto_value_of"
  | Prealloc_number -> "Prealloc_number"
  | Prealloc_number_proto -> "Prealloc_number_proto"
  | Prealloc_number_proto_to_string -> "Prealloc_number_proto_to_string"
  | Prealloc_number_proto_value_of -> "Prealloc_number_proto_value_of"
  | Prealloc_number_proto_to_fixed -> "Prealloc_number_proto_to_fixed"
  | Prealloc_number_proto_to_exponential -> "Prealloc_number_proto_to_exponential"
  | Prealloc_number_proto_to_precision -> "Prealloc_number_proto_to_precision"
  | Prealloc_array -> "Prealloc_array"
  | Prealloc_array_is_array -> "Prealloc_array_is_array"
  | Prealloc_array_proto -> "Prealloc_array_proto"
  | Prealloc_array_proto_join -> "Prealloc_array_proto_join"
  | Prealloc_array_proto_pop -> "Prealloc_array_proto_pop"
  | Prealloc_array_proto_push -> "Prealloc_array_proto_push"
  | Prealloc_array_proto_to_string -> "Prealloc_array_proto_to_string"
  | Prealloc_string -> "Prealloc_string"
  | Prealloc_string_proto -> "Prealloc_string_proto"
  | Prealloc_string_proto_to_string -> "Prealloc_string_proto_to_string"
  | Prealloc_string_proto_value_of -> "Prealloc_string_proto_value_of"
  | Prealloc_string_proto_char_at -> "Prealloc_string_proto_char_at"
  | Prealloc_string_proto_char_code_at -> "Prealloc_string_proto_char_code_at"
  | Prealloc_math -> "Prealloc_math"
  | Prealloc_mathop _ -> "Prealloc_mathop"
  | Prealloc_date -> "Prealloc_date"
  | Prealloc_regexp -> "Prealloc_regexp"
  | Prealloc_error -> "Prealloc_error"
  | Prealloc_native_error Native_error_eval -> "Prealloc_native_error_eval"
  | Prealloc_native_error Native_error_range -> "Prealloc_native_error_range"
  | Prealloc_native_error Native_error_ref -> "Prealloc_native_error_ref"
  | Prealloc_native_error Native_error_syntax -> "Prealloc_native_error_syntax"
  | Prealloc_native_error Native_error_type -> "Prealloc_native_error_type"
  | Prealloc_native_error Native_error_uri -> "Prealloc_native_error_uri"
  | Prealloc_native_error_proto Native_error_eval -> "Prealloc_native_error_proto_eval"
  | Prealloc_native_error_proto Native_error_range -> "Prealloc_native_error_proto_range"
  | Prealloc_native_error_proto Native_error_ref -> "Prealloc_native_error_proto_ref"
  | Prealloc_native_error_proto Native_error_syntax -> "Prealloc_native_error_proto_syntax"
  | Prealloc_native_error_proto Native_error_type -> "Prealloc_native_error_proto_type"
  | Prealloc_native_error_proto Native_error_uri -> "Prealloc_native_error_proto_uri"
  | Prealloc_throw_type_error -> "Prealloc_throw_type_error"
  | Prealloc_error_proto -> "Prealloc_error_proto"
  | Prealloc_error_proto_to_string -> "Prealloc_error_proto_to_string"
  | Prealloc_json -> "Prealloc_json"
  | Prealloc_proxy -> "Prealloc_proxy"
  | Prealloc_proxy_revocable -> "Prealloc_proxy_revocable"

let prcall = function
  | Call_default -> "Call_default"
  | Call_proxy -> "Call_proxy"
  | Call_after_bind -> "Call_after_bind"
  | Call_prealloc pa -> "Call_prealloc " ^ prprealloc pa

let prconstruct = function
  | Construct_default -> "Construct_default"
  | Construct_proxy -> "Construct_proxy"
  | Construct_after_bind -> "Construct_after_bind"
  | Construct_prealloc pa -> "Construct_prealloc " ^ prprealloc pa

let prhas_instance = function
  | Builtin_has_instance_function -> "Builtin_has_instance_function"
  | Builtin_has_instance_after_bind -> "Builtin_has_instance_after_bind"

let prget = function
  | Builtin_get_default -> "Builtin_get_default"
  | Builtin_get_proxy -> "Builtin_get_proxy"
  | Builtin_get_args_obj -> "Builtin_get_args_obj"

let prdelete = function
  | Builtin_delete_proxy -> "Builtin_delete_proxy"
  | Builtin_delete_default -> "Builtin_delete_default"
  | Builtin_delete_args_obj -> "Builtin_delete_args_obj"

let prloc = function
  | Object_loc_normal i -> "@" ^ string_of_int i
  | Object_loc_prealloc builtinid ->
		prprealloc builtinid

let prmutability = function
	| Mutability_uninitialized_immutable -> "Mutability_uninitialized_immutable"
	| Mutability_immutable -> "Mutability_immutable"
	| Mutability_nondeletable -> "Mutability_nondeletable"
	| Mutability_deletable -> "Mutability_deletable"

let prenv_loc i =
	"#" ^ string_of_int i

let prlexical_env l =
    String.concat ", " (List.map prenv_loc l)

let string_of_char_list cl = cl

let prprop_name = string_of_char_list

let prlabel = function
    | Label_empty -> "<empty>"
    | Label_string l -> "label:" ^ string_of_char_list l

let prlabel_set s =
    "{ " ^ String.concat "; " (List.map prlabel s) ^ " }"

let char_list_of_string s =
	let rec acc_ch acc n =
		if n < 0 then acc else acc_ch ((String.get s n)::acc) (n-1)
	in
	acc_ch [] ((String.length s) - 1)

let prbinary_op = function
	| Binary_op_add -> "+"
	| Binary_op_mult -> "*"
	| Binary_op_div -> "/"
	| Binary_op_equal -> "==="
	| Binary_op_instanceof -> "instanceof"
	| Binary_op_in -> "in"
	| _ -> "Binary Op NIY"

let prliteral = function
	| Literal_null -> "null"
	| Literal_bool b -> string_of_bool b
	| Literal_number f -> string_of_float f
	| Literal_string cl -> string_of_char_list cl

let prvalue = function
  | Value_undef -> "undefined"
  | Value_null -> "null"
  | Value_bool b -> string_of_bool b
  | Value_number f -> string_of_float f
  | Value_string cl -> "\"" ^ string_of_char_list cl ^ "\""
  | Value_object ol -> prloc ol

let prattributes = function
  | Attributes_data_of d ->
	Printf.sprintf "{ value: %s, writable: %s, enum: %s, config: %s }"
	  (prvalue (attributes_data_value d))
	  (prbool (attributes_data_writable d))
	  (prbool (attributes_data_enumerable d))
	  (prbool (attributes_data_configurable d))
  | Attributes_accessor_of a ->
	Printf.sprintf "{ get: %s, set: %s, enum: %s, config: %s }"
	  (prvalue (attributes_accessor_get a))
	  (prvalue (attributes_accessor_set a))
	  (prbool (attributes_accessor_enumerable a))
	  (prbool (attributes_accessor_configurable a))

let prfull_descriptor = function
	| Full_descriptor_undef -> "undef"
	| Full_descriptor_some a -> "attribute: " ^ prattributes a

let prdescriptor desc =
	Printf.sprintf "{ value : %s ; writable : %s ; get : %s  ; set : %s ; enumerable : %s ; configurable : %s }"
	  (proption prvalue desc.descriptor_value)
	  (proption prbool desc.descriptor_writable)
	  (proption prvalue desc.descriptor_get)
	  (proption prvalue desc.descriptor_set)
	  (proption prbool desc.descriptor_enumerable)
	  (proption prbool desc.descriptor_configurable)

let remove_siblings l =
    let l' = List.stable_sort (fun (k1, _) (k2, _) -> compare k1 k2) l in
    let rec aux = function
    | [] -> []
    | (k1, v) :: (k2, _) :: l when k1 = k2 ->
        aux ((k1, v) :: l)
    | a :: l -> a :: aux l
    in aux l'

let heap_to_list h =
    remove_siblings (Heap.to_list () h)

let probject_properties_aux skip_init old str p =
	String.concat "" (List.fold_left
		(fun acc (x, a) ->
			if skip_init
			  && option_case false (fun old0 ->
				List.mem (x, a) old0) old
			then acc
			else Printf.sprintf "\t %s %s = %s;\n"
                  str
				  (string_of_char_list x)
				  (prattributes a)
				    :: acc) []
		(heap_to_list p))

let probject_properties = probject_properties_aux false None ""

let prfieldmap (old : (prop_name * attributes) list option) skip_init loc obj =
    probject_properties_aux skip_init old (prloc loc ^ " .") (obj.object_properties_)


let prheap =
  let list_heap_init = heap_to_list object_heap_initial in
  fun skip_init heap ->
  "digraph g{\n" ^
  "node [shape=record];\n" ^
  "rankdir=LR;\n" ^
  (String.concat ""
  	  (List.rev (List.map (fun (key, v) ->
        let str =
          let old =
              try Some (List.assoc key list_heap_init)
              with Not_found -> None in
  	      prfieldmap (option_case None (fun obj ->
  	      		Some (heap_to_list (object_properties_ obj))) old)
  	      	skip_init key v ^
          String.concat "" (
              let pr s p g =
                if p (g v) = "" || (skip_init
                  && option_case false (fun obj ->
                    g v = g obj) old)
                then ""
                else
                    Printf.sprintf "\t %s @ %s = %s;\n"
                      (prloc key) s (p (g v)) in [
                  pr "proto" prvalue object_proto_ ;
                  pr "class" string_of_char_list object_class_ ;
                  pr "this" (option_case "" prvalue) (fun obj -> obj.object_bound_this_) ;
                  pr "call" (option_case "" prcall) object_call_ ;
                  pr "construct" (option_case "" prconstruct) object_construct_ ;
                  pr "has_instance" (option_case "" prhas_instance) object_has_instance_ ;
                  pr "prim_value" (option_case "" prvalue) object_prim_value_ ;
                  pr "extensible" prbool object_extensible_ ;
                  pr "get" prget object_get_ ;
                  pr "delete" prdelete (fun obj -> obj.object_delete_) ;
                  pr "scope" (option_case "" prlexical_env) object_scope_ ;
              ]) in
        if str = "" then Printf.sprintf "\t %s, an object;\n" (prloc key)
        else str) (heap_to_list heap)
  	))) ^
  "}"

let prenv_record r =
  String.concat "\n"
  	  (List.rev (List.map (fun (loc, er) ->
		  prenv_loc loc ^ " -> " ^
		  match er with
		  | Env_record_decl der ->
				String.concat "\n" (List.rev (List.map (fun (x, (mu, v)) ->
					"\t\"" ^ string_of_char_list x ^ "\" -> " ^
					prmutability mu ^ ", " ^ prvalue v
				) (heap_to_list der)))
		  | Env_record_object (o, this) ->
				prloc o ^ " with provide this = " ^ prbool this
	    ) (heap_to_list r)
  	))

let prstate skip s =
	"State:\n" ^
	"\tHeap:\n" ^ prheap skip (state_object_heap s) ^
	"\n\tEnv. Record:\n" ^ prenv_record (state_env_record_heap s)

let formatterstate formatter state =
  Format.fprintf formatter "%s" (prstate false state)
  

let prrestype = function
  | Restype_normal -> "normal"
  | Restype_break -> "break"
  | Restype_continue -> "continue"
  | Restype_return -> "return"
  | Restype_throw -> "throw"

let prref_base_type = function
  | Ref_base_type_value v -> "value: " ^ prvalue v
  | Ref_base_type_env_loc l -> "env_loc: " ^ prenv_loc l

let prref { ref_base = rb ; ref_name = rn ; ref_strict = str } =
    (if str then "strict " else "") ^ "ref: (" ^ prref_base_type rb ^ ") . " ^ prprop_name rn

let prresvalue = function
  | Resvalue_empty -> "Resvalue_empty"
  | Resvalue_value v -> "Resvalue_value: " ^ prvalue v
  | Resvalue_ref r -> "Resvalue_ref: " ^ prref r

(*
module M1 = Map.Make (struct type t = loc let compare = Pervasives.compare end)
module M2 = Map.Make (struct type t = field let compare = Pervasives.compare end)

let id = ref 0

let new_id () =
	incr id; "__" ^ (string_of_int (!id))
let print_to_file f h=
	let oc = open_out f in
	output_string oc (main h);
	close_out oc
 *)


let dump_expr_step = function
  | Expr_this -> "Expr_this"
  | Expr_identifier _ -> "Expr_identifier"
  | Expr_literal _ -> "Expr_literal"
  | Expr_object _ -> "Expr_object"
  | Expr_array _ -> "Expr_array"
  | Expr_function _ -> "Expr_function"
  | Expr_access _ -> "Expr_access"
  | Expr_member _ -> "Expr_member"
  | Expr_new _ -> "Expr_new"
  | Expr_call _ -> "Expr_call"
  | Expr_unary_op _ -> "Expr_unary_op"
  | Expr_binary_op _ -> "Expr_binary_op"
  | Expr_conditional _ -> "Expr_conditional"
  | Expr_assign _ -> "Expr_assign"

let dump_propbody_step = function
  | Propbody_val _ -> "Propbody_val"
  | Propbody_get _ -> "Propbody_get"
  | Propbody_set _ -> "Propbody_set"

let dump_funcbody_step = function
  | Funcbody_intro _ -> "Funcbody_intro"

let dump_stat_step = function
  | Stat_expr _ -> "Stat_expr"
  | Stat_block _ -> "Stat_block"
  | Stat_label (l, _) -> "Stat_label: " ^ string_of_char_list l
  | Stat_var_decl _ -> "Stat_var_decl"
  | Stat_if _ -> "Stat_if"
  | Stat_while _ -> "Stat_while"
  | Stat_do_while _ -> "Stat_do_while"
  | Stat_with _ -> "Stat_with"
  | Stat_throw _ -> "Stat_throw"
  | Stat_return _ -> "Stat_return"
  | Stat_break _ -> "Stat_break"
  | Stat_continue _ -> "Stat_continue"
  | Stat_try _ -> "Stat_try"
  | Stat_for _ -> "Stat_for"
  | Stat_for_var _ -> "Stat_for_var"
  | Stat_for_in _ -> "Stat_for_in"
  | Stat_for_in_var _ -> "Stat_for_in_var"
  | Stat_debugger -> "Stat_debugger"
  | Stat_switch (_, _, _) -> "Stat_switch"

let dump_prog_step = function
  | Prog_intro (b, es) ->
		String.concat " ; "
		  (List.map (function
			| Element_stat _ -> "Element_stat"
			| Element_func_decl _ -> "Element_func_decl") es)

