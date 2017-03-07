open Datatypes
open JsCommon
open JsCommonAux
open JsInit
open JsInterpreterMonads
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open LibProd
open Shared

(** ECMAScript Reference Interpreter Implementation

    @esurl     https://tc39.github.io/ecma262/
    @esversion 2017
*)

(* Basically, the ordering of the functions in this file are random. They need to be sorted. *)

(*------------JS preliminary -----------*)

(** val convert_prim_to_number : prim -> number **)

let convert_prim_to_number _foo_ = match _foo_ with
| Coq_value_undef -> JsNumber.nan
| Coq_value_null -> JsNumber.zero
| Coq_value_bool b -> if b then JsNumber.one else JsNumber.zero
| Coq_value_number n -> n
| Coq_value_string s -> JsNumber.from_string s
| _ -> assert false

(** val convert_number_to_integer : number -> number **)

let convert_number_to_integer n =
  if JsNumber.isnan n
  then JsNumber.zero
  else if   (JsNumber.isposzero n)
         || (JsNumber.isnegzero n)
         || (n === JsNumber.infinity)
         || (n === JsNumber.neg_infinity)
       then n
       else  (JsNumber.sign n) *. (JsNumber.floor (JsNumber.absolute n))

(** val convert_bool_to_string : bool -> string **)

let convert_bool_to_string _foo_ = match _foo_ with
| true -> "true"
| false -> "false"

(** val convert_prim_to_string : prim -> string **)

let convert_prim_to_string _foo_ = match _foo_ with
| Coq_value_undef -> "undefined"
| Coq_value_null -> "null"
| Coq_value_bool b -> convert_bool_to_string b
| Coq_value_number n -> JsNumber.to_string n
| Coq_value_string s -> s
| _ -> assert false

(** val equality_test_for_same_type : coq_type -> value -> value -> bool **)

let equality_test_for_same_type ty v1 v2 =
  match ty with
  | Coq_type_undef -> true
  | Coq_type_null -> true
  | Coq_type_bool -> value_compare v1 v2
  | Coq_type_number ->
    (match v1 with
     | Coq_value_undef -> false
     | Coq_value_null -> false
     | Coq_value_bool b -> false
     | Coq_value_string s -> false
     | Coq_value_number n1 ->
       (match v2 with
        | Coq_value_undef -> false
        | Coq_value_null -> false
        | Coq_value_bool b -> false
        | Coq_value_string s -> false
        | Coq_value_number n2 ->
          if JsNumber.isnan n1
          then false
          else if JsNumber.isnan n2
               then false
               else if   (JsNumber.isposzero n1)
                      && (JsNumber.isnegzero n2)
                    then true
                    else if   (JsNumber.isnegzero n1)
                          &&  (JsNumber.isposzero n2)
                         then true
                         else n1 === n2
        | Coq_value_object o -> false)
     | Coq_value_object o -> false)
  | Coq_type_string -> value_compare v1 v2
  | Coq_type_object -> value_compare v1 v2

(** val strict_equality_test : value -> value -> bool **)

let strict_equality_test v1 v2 =
  let ty1 = type_of v1 in
  let ty2 = type_of v2 in
  if type_compare ty1 ty2
  then equality_test_for_same_type ty1 v1 v2
  else false

(** val inequality_test_number : number -> number -> prim **)

let inequality_test_number n1 n2 =
  if (JsNumber.isnan n1) || (JsNumber.isnan n2)
  then Coq_value_undef else if n1 === n2
       then Coq_value_bool false
       else if   (JsNumber.isposzero n1)
              && (JsNumber.isnegzero n2)
            then Coq_value_bool false
            else if (JsNumber.isnegzero n1)
                 && (JsNumber.isposzero n2)
                 then Coq_value_bool false
                 else if n1 === JsNumber.infinity
                      then Coq_value_bool false
                      else if n2 === JsNumber.infinity
                           then Coq_value_bool true
                           else if n2 === JsNumber.neg_infinity
                                then Coq_value_bool false
                                else if n1 === JsNumber.neg_infinity
                                     then Coq_value_bool true
                                     else Coq_value_bool (n1 < n2)

(** val inequality_test_string : string -> string -> bool **)

(* ARTHUR hack 
let rec inequality_test_string s1 s2 =
  match s1 with
  | [] ->
    (match s2 with
     | [] -> false
     | a::s -> true)
  | c1::s1_2 ->
    (match s2 with
     | [] -> false
     | c2::s2_2 ->
       if ascii_comparable c1 c2
       then inequality_test_string s1_2 s2_2
       else lt_int_decidable (int_of_char c1) (int_of_char c2))
*)
let inequality_test_string s1 s2 = string_lt s1 s2


(** val inequality_test_primitive : prim -> prim -> prim **)

let inequality_test_primitive w1 w2 =
  match w1 with
  | Coq_value_undef ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_value_null ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_value_bool b ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_value_number n ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_value_string s1 ->
    (match w2 with
     | Coq_value_undef ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_value_null ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_value_bool b ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_value_number n ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_value_string s2 -> Coq_value_bool (inequality_test_string s1 s2)
     | _ -> assert false)
  | _ -> assert false

(** val typeof_prim : prim -> string **)

let typeof_prim _foo_ = match _foo_ with
| Coq_value_undef -> "undefined"
| Coq_value_null -> "object"
| Coq_value_bool b -> "boolean"
| Coq_value_number n -> "number"
| Coq_value_string s -> "string"
| _ -> assert false

(** val string_of_propname : propname -> prop_name **)

let string_of_propname _foo_ = match _foo_ with
| Coq_propname_identifier s -> s
| Coq_propname_string s -> s
| Coq_propname_number n -> JsNumber.to_string n

(*---------------------------------*)


(** Fetches a given object slot value (using proj) from the object l in state s
    FIXME: The name is very confusing. *)
let run_object_method proj s l =
  LibOption.map proj (object_binds_option s l)

(*---DEBUG
  let run_object_method proj s l =
   let opt = object_binds_option s l in
     begin match opt with
       | None -> Debug.run_object_method l
       | _ -> ()
     end;
     LibOption.map proj opt
*)

(** val run_object_heap_set :
    (coq_object -> a' -> coq_object) -> state -> object_loc -> a' -> state option **)
(** Updates an object's internal field with the given update function [prj].
    (Update functions are defined in JsSyntaxAux) *)

let run_object_set_internal prj s l v =
  LibOption.map (fun o -> object_write s l (prj o v)) (object_binds_option s l)

(** val run_object_heap_set_extensible :
    bool -> state -> object_loc -> state option **)

let run_object_heap_set_extensible b s l =
  run_object_set_internal object_set_extensible s l b

(* DEBUG
   let run_object_heap_set_extensible b s l =
   let opt = object_binds_option s l in
     begin match opt with
       | None -> Debug.run_object_heap_set_extensible l
       | _ -> ()
     end;
     LibOption.map (fun o -> object_write s l (object_set_extensible o b)) opt
*)

let rec build_error s c vproto vmsg : 'a specret resultof =
  let o = object_new vproto ("Error") in
  let (l, s_2) = object_alloc s o in
  if value_compare vmsg Coq_value_undef
  then res_out s_2 (res_val (Coq_value_object l))
  else (
    let%value (s_3, vstr) = to_string s_2 c vmsg in
    let a = { attributes_data_value = vstr; attributes_data_writable = true;
      attributes_data_enumerable = false; attributes_data_configurable = true } in
    let%success (s_4, rv) = object_define_own_prop s_3 c l "message" (descriptor_of_attributes (Coq_attributes_data_of a)) throw_true in
    res_out s_4 (res_val (Coq_value_object l))
  )

and run_error s c ne : 'a specret resultof =
  let%object (s_2, l) = (build_error s c (Coq_value_object (Coq_object_loc_prealloc
                                                            (Coq_prealloc_native_error_proto ne))) Coq_value_undef) in
  res_out s_2 (res_throw (Coq_resvalue_value (Coq_value_object l)))

and run_error_no_c s ne =
  (* FIXME: Try and run c *)
  run_error s (execution_ctx_initial true) ne

(*****************************************************************************)
(*****************************************************************************)
(************* START OF THE BIG RECURSIVE INTERPRETER FUNCTION ***************)
(*****************************************************************************)
(*****************************************************************************)

(** New ES6 Spec functions here, writting in the specification in CamelCase()
    will all be prefixed with spec_ *)

(** {4 Object Internal Methods and Internal Slots }
    @esid sec-object-internal-methods-and-internal-slots
    @essec 6.1.7.2 *)
(** Function to implement the specification text "O has a [[X]] internal slot"
    @param l    The location of O
    @param prj  The object projection function corresponding to [[X]] *)
and object_has_internal_slot s l prj =
  let slot_value = run_object_method prj s l in
  match slot_value with
  | None -> assert false
  | Some v -> match v with
    | Some _ -> true
    | None -> false

(** Function to implement the specification text of "O has a [[X]] internal method" *)
and object_has_internal_method s l prj =
  object_has_internal_slot s l prj

(** {5 Object Internal Method Dispatch Functions }
    Functions in this section are used to dispatch Internal Methods to the
    correct implementation based upon the value of the internal slot

    All these functions accept a parameter o, which is the {i location} of the object.
 *)

(** Function to dispatch calls to O.[[GetPrototypeOf]]() *)
and object_internal_get_prototype_of s o =
  let%some internal_method = (run_object_method object_get_prototype_of_ s o) in
  match internal_method with
  | Coq_builtin_get_prototype_of_default -> ordinary_object_internal_get_prototype_of s o
  | Coq_builtin_get_prototype_of_proxy   -> proxy_object_internal_get_prototype_of s o

(** Function to dispatch calls to O.[[GetPrototypeOf]](V) *)
and object_internal_set_prototype_of s o v =
  let%some internal_method = (run_object_method object_set_prototype_of_ s o) in
  match internal_method with
  | Coq_builtin_set_prototype_of_default -> ordinary_object_internal_set_prototype_of s o v
  | Coq_builtin_set_prototype_of_proxy   -> proxy_object_internal_set_prototype_of s o v

(** Function to dispatch calls to O.[[IsExtensible]]() *)
and object_internal_is_extensible s o =
  let%some internal_method = run_object_method object_is_extensible_ s o in
  match internal_method with
  | Coq_builtin_is_extensible_default -> ordinary_object_internal_is_extensible s o
  | Coq_builtin_is_extensible_proxy   -> proxy_object_internal_is_extensible s o

(** Function to dispatch calls to O.[[PreventExtensions]]() *)
and object_internal_prevent_extensions s o =
  let%some internal_method = run_object_method object_prevent_extensions_ s o in
  match internal_method with
  | Coq_builtin_prevent_extensions_default -> ordinary_object_internal_prevent_extensions s o
  | Coq_builtin_prevent_extensions_proxy   -> proxy_object_internal_prevent_extensions s o

(** Function to dispatch calls to O.[[GetOwnProperty]](P) *)
and object_internal_get_own_property s o p =
  let%some internal_method = (run_object_method object_get_own_prop_ s o) in
  match internal_method with
  | Coq_builtin_get_own_prop_default  -> ordinary_object_internal_get_own_property s o p
  | Coq_builtin_get_own_prop_args_obj -> Coq_result_not_yet_implemented (* TODO *)
  | Coq_builtin_get_own_prop_string   -> Coq_result_not_yet_implemented (* TODO *)
  | Coq_builtin_get_own_prop_proxy    -> Coq_result_not_yet_implemented (* TODO *)

(** Function to dispatch calls to O.[[DefineOwnProperty]](P, desc) *)
and object_internal_define_own_property s o p desc =
  let%some internal_method = (run_object_method object_define_own_prop_ s o) in
  match internal_method with
  | Coq_builtin_define_own_prop_default  -> ordinary_object_internal_define_own_property s o p desc
  | Coq_builtin_define_own_prop_array    -> Coq_result_not_yet_implemented (* TODO *)
  | Coq_builtin_define_own_prop_args_obj -> Coq_result_not_yet_implemented (* TODO *)
  | Coq_builtin_define_own_prop_proxy    -> Coq_result_not_yet_implemented (* TODO *)

(** Function to dispatch calls to O.[[HasProperty]](P) *)
and object_internal_has_property s o p =
  let%some b = (run_object_method object_has_prop_ s o) in
  match b with
  | Coq_builtin_has_prop_default -> ordinary_has_property s o p
  | Coq_builtin_has_prop_proxy   -> Coq_result_not_yet_implemented (* TODO *)

(** Function to dispatch calls to O.[[Get]](P, receiver) *)
and object_internal_get s o p receiver =
  let%some internal_method = (run_object_method object_get_ s o) in
  let dispatch_es5 _ =
    match p with
    (* FIXME: ES5 HACK CONTEXT *)
    | Coq_value_string x -> object_get_builtin s (execution_ctx_initial true) internal_method (Coq_value_object o) o x
    | _ -> assert false
  in match internal_method with
  (* FIXME: Dispatch to new versions of functions *)
  | Coq_builtin_get_default  -> ordinary_object_internal_get s o p receiver
  | Coq_builtin_get_function -> dispatch_es5 ()
  | Coq_builtin_get_args_obj -> dispatch_es5 ()
  | Coq_builtin_get_proxy    -> Coq_result_not_yet_implemented

(** Function to dispatch calls to O.[[Set]](P, V, Receiver) *)
and object_internal_set s o p v receiver =
  let%some internal_method = (run_object_method object_set_ s o) in
  match internal_method with
  | Coq_builtin_set_default -> ordinary_object_internal_set s o p v receiver
  | Coq_builtin_set_proxy   -> Coq_result_not_yet_implemented

(** @deprecated In favour of potentially the same [object_internal_set] ES6 method *)
and object_put s c l p v str = object_internal_set s l (Coq_value_string p) v (Coq_value_object l)

(** Function to dispatch calls to O.[[Delete]](P) *)
and object_internal_delete s o p =
  let%some internal_method = run_object_method object_delete_ s o in
  match internal_method with
  | Coq_builtin_delete_default  -> ordinary_object_internal_delete s o p
  | Coq_builtin_delete_args_obj -> assert false
  | Coq_builtin_delete_proxy    -> Coq_result_not_yet_implemented

(** @deprecated ES5 *)
and object_delete_default s c l x str =
  let%bool s, b = ordinary_object_internal_delete s l (Coq_value_string x) in
  if str && (not b) then
    run_error_no_c s Coq_native_error_type
  else
    res_ter s (res_val (Coq_value_bool b))

and object_internal_own_property_keys s o =
  let%some internal_method = run_object_method object_own_property_keys_ s o in
  match internal_method with
  | Coq_builtin_own_property_keys_default -> ordinary_object_internal_own_property_keys s o
  | Coq_builtin_own_property_keys_proxy   -> Coq_result_not_yet_implemented

(** Function to dispatch calls to O.[[Call]](thisArgument, argumentsList) *)
and object_internal_call s o thisArgument argumentsList =
  (* FIXME: ES5 HACK CONTEXT *)
  run_call s (execution_ctx_initial true) o thisArgument argumentsList

(** {3 The Reference Specification Type}
    @essec 6.2.4
    @esid sec-reference-specification-type *)

and get_base = ref_base

(* Note: may prefer this to return a raw string? *)
and get_referenced_name r =
  Coq_value_string (ref_name r)

and is_strict_reference = ref_strict

(* TODO: Copied ES5 code, tidy up *)
and has_primitive_base r =
  ref_kind_comparable (ref_kind_of r) Coq_ref_kind_primitive_base

and is_property_reference r =
  (ref_kind_of r === Coq_ref_kind_object) || has_primitive_base r

and is_unresolvable_reference r =
  ref_kind_of r === Coq_ref_kind_undef

and is_super_reference r =
  is_some r.ref_this_value

(** GetValue(V)

    Note: Although the type of [v] is a resultof, its state is disregarded
    and must be explicitly passed to prevent issues with the value being
    computed prior to other state mutations.

    @esid sec-getvalue
    @essec 6.2.4.1
*)
and get_value s v =
  let%success (_, v') = v in
  if not (type_of_resvalue v' === Type_resvalue_ref) then v
  else
    let v = ref_of_resvalue v' in
    let base = get_base v in
    if is_unresolvable_reference v then
      run_error_no_c s Coq_native_error_ref
    else if is_property_reference v then
      let base = value_of_ref_base_type base in
      let%ret (s, base) =
        if has_primitive_base v then
          let%assert_ret _ = not (base === Coq_value_undef) || (base === Coq_value_null) in
          let%OBJECT_ret (s, base) = to_object s base in
          Continue (s, base)
        else
          Continue (s, loc_of_value base)
      in object_internal_get s base (get_referenced_name v) (get_this_value v)
    else
      let base = env_loc_of_ref_base_type base in
      get_binding_value s base (get_referenced_name v) (is_strict_reference v)

(** PutValue(V,W)

    Note: Although the types of [v] and [w] are resultof, their states are
    disregarded and must be explicitly passed, as it is not clear in which
    order they were calculated.

    @esid sec-putvalue
    @essec 6.2.4.2
*)
and put_value s c v w =
  let%success (_, v) = v in
  let%value (_, w) = w in
  if not (type_of_resvalue v === Type_resvalue_ref) then
    run_error_no_c s Coq_native_error_ref
  else
  let v = ref_of_resvalue v in
  let base = get_base v in
  if is_unresolvable_reference v then
    if is_strict_reference v then
      run_error_no_c s Coq_native_error_ref
    else
      let globalObj = get_global_object s c in
      set s globalObj (get_referenced_name v) w (Coq_value_bool false)
  else if is_property_reference v then
    let base = value_of_ref_base_type base in
    let%ret s, base =
      if has_primitive_base v then
        let%assert_ret _ = not (base === Coq_value_null || base === Coq_value_undef) in
        let%OBJECT_ret s, base = to_object s base in
        Continue (s, base)
      else
        Continue (s, loc_of_value base)
    in
    let%bool s, succeeded = object_internal_set s base (get_referenced_name v) w (get_this_value v) in
    if (not succeeded) && (is_strict_reference v) then
      run_error_no_c s Coq_native_error_type
    else
      res_void s
  else
    let base = env_loc_of_ref_base_type base in
    set_mutable_binding s base (get_referenced_name v) w (Coq_value_bool (is_strict_reference v))

(** @esid sec-getthisvalue
    @essec 6.2.4.3 *)
and get_this_value v =
  let asrt = assert (is_property_reference v) in
  if is_super_reference v then
    ref_this_value v
  else
    value_of_ref_base_type (get_base v)


(** {3 The Property Descriptor Specification Type}
    @esid sec-property-descriptor-specification-type
    @essec 6.2.5 *)

(** @essec 6.2.5.1
    @esid sec-isaccessordescriptor *)
and is_accessor_descriptor desc =
  match desc with
  | Descriptor_undef -> false
  | Descriptor desc -> not
    (  (option_compare value_compare desc.descriptor_get None)
    && (option_compare value_compare desc.descriptor_set None))

(** @essec 6.2.5.2
    @esid sec-isdatadescriptor *)
and is_data_descriptor desc =
  match desc with
  | Descriptor_undef -> false
  | Descriptor desc -> not
    (  (option_compare value_compare desc.descriptor_value None)
    && (option_compare bool_eq desc.descriptor_writable None))

(** @essec 6.2.5.3
    @esid sec-isgenericdescriptor *)
and is_generic_descriptor desc =
  match desc with
  | Descriptor_undef -> false
  | Descriptor _ -> (not (is_accessor_descriptor desc)) && (not (is_data_descriptor desc))

(** @esid sec-topropertydescriptor
    @essec 6.2.5.5 *)
and to_property_descriptor s _foo_ =
  match _foo_ with
  | Coq_value_object l ->
    let desc = descriptor_intro_empty in
    let%bool (s, hasEnumerable) = has_property s _foo_ (Coq_value_string "enumerable") in
    let%spec (s, desc) =
      if hasEnumerable
      then
        let%value (s, v) = get s _foo_ (Coq_value_string "enumerable") in
        let enum = to_boolean v in
        res_spec s (descriptor_with_enumerable desc (Some enum))
      else res_spec s desc
    in
    let%bool (s, hasConfigurable) = has_property s _foo_ (Coq_value_string "configurable") in
    let%spec (s, desc) =
      if hasConfigurable
      then
        let%value (s, v) = get s _foo_ (Coq_value_string "configurable") in
        let conf = to_boolean v in
        res_spec s (descriptor_with_configurable desc (Some conf))
      else res_spec s desc
    in
    let%bool (s, hasValue) = has_property s _foo_ (Coq_value_string "value") in
    let%spec (s, desc) =
      if hasValue
      then
        let%value (s, value) = get s _foo_ (Coq_value_string "value") in
        res_spec s (descriptor_with_value desc (Some value))
      else res_spec s desc
    in
    let%bool (s, hasWritable) = has_property s _foo_ (Coq_value_string "writable") in
    let%spec (s, desc) =
      if hasValue
      then
        let%value (s, v) = get s _foo_ (Coq_value_string "writable") in
        let writable = to_boolean v in
        res_spec s (descriptor_with_writable desc (Some writable))
      else res_spec s desc
    in
    let%bool (s, hasGet) = has_property s _foo_ (Coq_value_string "get") in
    let%spec (s, desc) =
      if hasGet
      then
        let%value (s, getter) = get s _foo_ (Coq_value_string "get") in
        if (not (is_callable s getter)) && (not (type_of getter === Coq_type_undef))
        then run_error_no_c s Coq_native_error_type
        else res_spec s (descriptor_with_get desc (Some getter))
      else res_spec s desc
    in
    let%bool (s, hasSet) = has_property s _foo_ (Coq_value_string "set") in
    let%spec (s, desc) =
      if hasSet
      then
        let%value (s, setter) = get s _foo_ (Coq_value_string "set") in
        if (not (is_callable s setter)) && (not (type_of setter === Coq_type_undef))
        then run_error_no_c s Coq_native_error_type
        else res_spec s (descriptor_with_set desc (Some setter))
      else res_spec s desc
    in
    if ((not (desc.descriptor_get === None)) || (not (desc.descriptor_set === None)))
      && ((not (desc.descriptor_value === None)) || (not (desc.descriptor_writable === None)))
    then run_error_no_c s Coq_native_error_type
    else res_spec s desc
  | _ -> throw_result (run_error_no_c s Coq_native_error_type)

(** @essec 6.2.5.6
    @esid sec-completepropertydescriptor *)
and complete_property_descriptor s desc =
  let desc = descriptor_get_defined desc in
  let like = { descriptor_value        = Some Coq_value_undef;
               descriptor_writable     = Some false;
               descriptor_get          = Some Coq_value_undef;
               descriptor_set          = Some Coq_value_undef;
               descriptor_enumerable   = Some false;
               descriptor_configurable = Some false }
  in let desc =
    if is_generic_descriptor (Descriptor desc) || is_data_descriptor (Descriptor desc)
    then
      let desc = descriptor_with_value desc like.descriptor_value in
      descriptor_with_writable desc like.descriptor_writable
    else
      let desc = descriptor_with_get desc like.descriptor_get in
      descriptor_with_set desc like.descriptor_set
  in let desc =
    if option_compare bool_eq desc.descriptor_enumerable None
    then descriptor_with_enumerable desc like.descriptor_enumerable
    else desc
  in let desc =
    if option_compare bool_eq desc.descriptor_configurable None
    then descriptor_with_configurable desc like.descriptor_configurable
    else desc
  in desc

(** {1 Abstract Operations }
    @essec 7
    @esid sec-abstract-operations *)

(** {2 Type Conversion }
    @essec 7.1
    @esid sec-type-conversion *)

(** @essec 7.1.2
    @esid sec-toboolean *)
(* FIXME: _foo_ = argument *)
and to_boolean _foo_ = match _foo_ with
| Coq_value_undef -> false
| Coq_value_null -> false
| Coq_value_bool b -> b
| Coq_value_number n ->
  if (JsNumber.isposzero n) || (JsNumber.isnegzero n) || (JsNumber.isnan n) then false
  else true
| Coq_value_string s -> if string_eq s "" then false else true
(* | Coq_value_symbol s -> true *)
| Coq_value_object o -> true

(** {2 Testing and Comparison Operations }
    @essec 7.2
    @esid sec-testing-and-comparison-operations *)

(** @essec 7.2.3
    @esid sec-iscallable *)
and is_callable s argument =
  match argument with
  | Coq_value_object l -> object_has_internal_method s l object_call_
  | _ -> false

(** @essec 7.2.5
    @esid sec-isextensible-o *)
and is_extensible s o =
  let%assert _ = match type_of o with Coq_type_object -> true | _ -> false in
  match o with
  | Coq_value_object l -> object_internal_is_extensible s l
  | _ -> assert false

(** @essec 7.2.7
    @esid sec-ispropertykey *)
and is_property_key argument =
  (type_of argument) === Coq_type_string (* FIXME: || (type_of argument) === Coq_type_symbol *)

(** @essec 7.2.9
    @esid sec-samevalue *)
and same_value x y =
  if not (type_compare (type_of x) (type_of y))
  then false
  else match type_of x with
  | Coq_type_number ->
    (match x with
    | Coq_value_number n_x ->
      (match y with
      | Coq_value_number n_y ->
        if (JsNumber.isnan n_x) && (JsNumber.isnan n_y) then true
        else if (JsNumber.isposzero n_x) && (JsNumber.isnegzero n_y) then true
        else if (JsNumber.isnegzero n_x) && (JsNumber.isposzero n_y) then true
        else n_x === n_y
      | _ -> assert false)
    | _ -> assert false)
  | _ -> same_value_non_number x y

(** @essec 7.2.11
    @esid sec-samevaluenonnumber *)
and same_value_non_number x y =
  let (*%assert*) asrt = assert (not (type_compare (type_of x) Coq_type_number)) in
  let (*%assert*) asrt = assert (type_compare (type_of x) (type_of y)) in
  match x with
  | Coq_value_undef      -> true
  | Coq_value_null       -> true
  | Coq_value_string s_x ->
    (match y with
    | Coq_value_string s_y -> string_eq s_x s_y
    | _ -> assert false)
  | Coq_value_bool b_x   ->
    (match y with
     | Coq_value_bool b_y -> bool_eq b_x b_y
     | _ -> assert false)
  (* FIXME: Symbol
  | Coq_value_symbol s_x ->
     (match y with
     | Coq_value_symbol s_y -> symbol_compare s_x s_y
     | _ -> assert false)
  *)
  | Coq_value_object l_x ->
    (match y with
    | Coq_value_object l_y -> object_loc_compare l_x l_y
    | _ -> assert false)
  | _ -> assert false

(** {2 Operations on Objects }
    @essec 7.3
    @esid sec-operations-on-objects *)

(** @essec 7.3.1
    @esid sec-get-o-p *)
and get s o p =
  let%assert _ = (type_of o) === Coq_type_object in
  let%assert _ = is_property_key p in
  match o with
  | Coq_value_object l -> object_internal_get s l p o
  | _ -> assert false

(** @essec 7.3.2
    @esid sec-getv *)
and get_v s v p =
  let%assert _ = is_property_key p in
  let%object (s1, l) = to_object s v in
  object_internal_get s1 l p v

(** @essec 7.3.3
    @esid sec-set-o-p-v-throw *)
and set s o p v throw =
  let%assert _ = (type_of o) === Coq_type_object in
  let l = loc_of_value o in
  let%assert _ = is_property_key p in
  let%assert _ = (type_of throw) === Coq_type_bool in
  let throw = bool_of_value throw in
  let%bool s, success = object_internal_set s l p v o in
  if (not success) && throw then
    run_error_no_c s Coq_native_error_type
  else
    res_ter s (res_val (Coq_value_bool success))

(** @essec 7.3.4
    @esid sec-createdataproperty *)
and create_data_property s o p v =
  let%assert _ = type_of o === Coq_type_object in
  let o = loc_of_value o in
  let%assert _ = is_property_key p in
  let newDesc = descriptor_intro_data v true true true in
  object_internal_define_own_property s o p newDesc

(** @essec 7.3.9
    @esid sec-getmethod *)
and get_method s v p =
  let%assert _ = is_property_key p in
  let%value (s1, func) = get_v s v p in
  match type_of func with
  | Coq_type_undef -> res_out s1 (res_val Coq_value_undef)
  | Coq_type_null  -> res_out s1 (res_val Coq_value_undef)
  | _ ->
    let callable = is_callable s1 func in
    if not callable
    then run_error_no_c s1 Coq_native_error_type
    else res_out s1 (res_val func)

(** @essec 7.3.10
    @esid sec-hasproperty *)
and has_property s o p =
  let%assert _ = (type_of o) === Coq_type_object in
  let%assert _ = is_property_key p in
  match o with
  | Coq_value_object l -> object_internal_has_property s l p
  | _ -> assert false

(** @essec 7.3.12
    @esid sec-call *)
and call s f v argumentList =
  if_some_or_apply_default argumentList [] (fun argumentList ->
    let callable = is_callable s f in
    if not callable then run_error_no_c s Coq_native_error_type
    else match f with
    | Coq_value_object l -> object_internal_call s l v argumentList
    | _ -> assert false
  )

(** {1 Executable Code and Execution Contexts}
    @essec 8
    @esid sec-executable-code-and-execution-contexts *)
(** {2 Lexical Environments}
    @essec 8.1
    @esid sec-lexical-environments *)
(** {3 Environment Records}
    @essec 8.1.1
    @esid sec-environment-records *)
(** {4 Abstract Methods}
    Dynamic dispatch functions for environment record abstract methods.

    @esid table-15
*)
(*
and has_binding s e n =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_has_binding s e n
  | Coq_env_record_object (l, this) -> object_env_record_has_binding s l this n

and create_mutable_binding s e n d =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_create_mutable_binding s e n d
  | Coq_env_record_object (l, this) -> object_env_record_create_mutable_binding s l this n d

and create_immutable_binding s e n str =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_create_immutable_binding s e n str
  | Coq_env_record_object (l, this) -> object_env_record_create_immutable_binding s l this n str

and initialize_binding s e n v =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_initialize_binding s e n v
  | Coq_env_record_object (l, this) -> object_env_record_initialize_binding s l this n v
*)

and set_mutable_binding s l n v str =
  let%some e = env_record_binds_option s l in
  match e with
  | Coq_env_record_decl e           -> decl_env_record_set_mutable_binding s l e n v str
  | Coq_env_record_object (l, this) -> object_env_record_set_mutable_binding s (Coq_value_object l) this n v str

and get_binding_value s l n str =
  let%some e = env_record_binds_option s l in
  match e with
  | Coq_env_record_decl e           -> decl_env_record_get_binding_value s e n str
  | Coq_env_record_object (l, this) -> object_env_record_get_binding_value s (Coq_value_object l) this n str

(*
and delete_binding s e n =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_delete_binding s e n
  | Coq_env_record_object (l, this) -> object_env_record_delete_binding s l this n

and has_this_binding s e =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_has_this_binding s e
  | Coq_env_record_object (l, this) -> object_env_record_has_this_binding s l this

and has_super_binding s e =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_has_super_binding s e
  | Coq_env_record_object (l, this) -> object_env_record_has_super_binding s l this

and with_base_object s e =
  match e with
  | Coq_env_record_decl e           -> decl_env_record_with_base_object s e
  | Coq_env_record_object (l, this) -> object_env_record_with_base_object s l this
*)

(**
    {4 Declarative Environment Records}
    @essec 8.1.1.1
    @esid sec-declarative-environment-records
*)

(* FIXME: Move to a bindings data structure section. Apply to mutable bindings also. *)
and binding_is_uninitialized binding =
  let (mutability, unused) = binding in
  mutability_compare mutability Coq_mutability_uninitialized_immutable

and binding_is_mutable binding =
  let (mutability, unused) = binding in
  (mutability === Coq_mutability_nondeletable) || (mutability === Coq_mutability_deletable)

(** @essec 8.1.1.1.2
    @esid sec-declarative-environment-records-createmutablebinding-n-d *)
and decl_env_record_create_mutable_binding s l envRec n' d =
  let n = string_of_value n' in
  let d = bool_of_value d in
  let%assert _ = not (HeapStr.indom_dec envRec n) in
  let s = env_record_write_decl_env s l n (mutability_of_bool d) Coq_value_undef in (* FIXME: Uninitialized field *)
  res_void s

(** @essec 8.1.1.1.4
    @esid sec-declarative-environment-records-initializebinding-n-v *)
and decl_env_record_initialize_binding s l envRec n' v =
  let n = string_of_value n' in
  let%some binding = HeapStr.read_option envRec n in
  let (mutability, unused) = binding in (* TODO: Tidy up *)
  let%assert _ = binding_is_uninitialized binding in
  (* Update mutability / initialization flag *)
  let s = env_record_write_decl_env s l n mutability v in
  res_void s

(** @essec 8.1.1.1.5
    @esid sec-declarative-environment-records-setmutablebinding-n-v-s *)
and decl_env_record_set_mutable_binding s l envRec n' v str =
  let n = string_of_value n' in
  let str = bool_of_value str in
  if not (HeapStr.indom_dec envRec n) then
    if str then
      run_error_no_c s Coq_native_error_ref
    else
      let%success s, _ = decl_env_record_create_mutable_binding s l envRec n' (Coq_value_bool true) in
      let%success s, _ = decl_env_record_initialize_binding s l envRec n' v in
      res_void s
  else
  let%some binding = HeapStr.read_option envRec n in
  let (mutability, unused) = binding in
  (* FIXME: Implement strictness for bindings: let str = if binding.ref_strict then true else str in *)
  if binding_is_uninitialized binding then
    run_error_no_c s Coq_native_error_ref
  else let%ret s =
    if binding_is_mutable binding then
      Continue (env_record_write_decl_env s l n mutability v)
    else
      if str then
        Return (run_error_no_c s Coq_native_error_type)
      else
        Continue s
    in
    res_void s

(** @essec 8.1.1.1.6
    @esid sec sec-declarative-environment-records-getbindingvalue-n-s *)
and decl_env_record_get_binding_value s envRec n str =
  let n = string_of_value n in
  let%assert _ = HeapStr.indom_dec envRec n in
  let%some binding = HeapStr.read_option envRec n in
  let (mutability, v) = binding in
  if mutability_compare mutability Coq_mutability_uninitialized_immutable (* FIXME: Need to handle mutable uninitialized also *)
  then run_error_no_c s Coq_native_error_ref
  else res_ter s (res_val v)

(**
    {4 Object Environment Records}
    @essec 8.1.1.2
    @esid sec-object-environment-records
*)

(** @essec 8.1.1.1.2.5
    @edid sec-object-environment-records-setmutablebinding-n-v-s *)
and object_env_record_set_mutable_binding s bindings this n v str =
  set s bindings n v str

(** @essec 8.1.1.1.2.6
    @esid sec-object-environment-records-getbindingvalue-n-s *)
and object_env_record_get_binding_value s bindings this n str =
  let%bool s, value = has_property s bindings n in
  if not value then
    if not str
    then res_ter s (res_val Coq_value_undef)
    else run_error_no_c s Coq_native_error_ref
  else
    get s bindings n

(** {2 Execution Contexts}
    @essec 8.3
    @esid sec-execution-contexts *)

(** @esid sec-getglobalobject
    @essec 8.3.6 *)
and get_global_object s ctx =
  (* FIXME: ES5 hack (realms required) *)
  let e = unsome_error (env_record_binds_option s env_loc_global_env_record) in
  match e with
  | Coq_env_record_object (l, this) -> Coq_value_object l
  |  _ -> assert false

(** {1 Ordinary and Exotic Objects Behaviours }
    @essec 9
    @esid sec-ordinary-and-exotic-objects-behaviours *)

(**
    {2 Ordinary Object Internal Methods and Internal Slots }
    @essec 9.1
    @esid sec-ordinary-object-internal-methods-and-internal-slots *)

(** [[GetPrototypeOf]]()
    @essec 9.1.1
    @esid sec-ordinary-object-internal-methods-and-internal-slots-getprototypeof *)
and ordinary_object_internal_get_prototype_of s o =
  let%value (s1, v) = ordinary_get_prototype_of s o in
  res_out s (res_val v)

(** @essec 9.1.1.1
    @esid sec-ordinarygetprototypeof *)
and ordinary_get_prototype_of s o =
  let%some v = run_object_method object_proto_ s o in
  res_spec s (res_val v)

(** [[SetPrototypeOf]](V)
    @essec 9.1.2
    @esid sec-ordinary-object-internal-methods-and-internal-slots-setprototypeof-v *)
and ordinary_object_internal_set_prototype_of s o v =
  let%value (s1, v) = ordinary_set_prototype_of s o v in
  res_out s (res_val v)

(** @essec 9.1.2.1
    @esid sec-ordinarysetprototypeof *)
and ordinary_set_prototype_of s o v =
  let%assert _ = (match type_of v with Coq_type_object -> true | Coq_type_null -> true | _ -> false) in
  let%some extensible = run_object_method object_extensible_ s o in
  let%some current = run_object_method object_prototype_ s o in
  let sv = same_value v current in
  if sv then res_spec s (res_val (Coq_value_bool true))
  else if not extensible then res_spec s (res_val (Coq_value_bool false))
  else
    let rec repeat p done_ = begin
      if not done_ then
        (match p with
        | Coq_value_null -> repeat p true
        | Coq_value_object p_l ->
          if same_value p (Coq_value_object o)
          then res_spec s (res_val (Coq_value_bool false))
          else
            let%some gpo = run_object_method object_get_prototype_of_ s p_l in
            (match gpo with
            | Coq_builtin_get_prototype_of_default -> (
              let%some prototype = run_object_method object_prototype_ s p_l in
              repeat prototype false)
            | _ -> repeat p true)
        | _ -> assert false)
      else
        (* Set the value of the [[Prototype]] internal slot of O to V *)
        let%some s' = run_object_set_internal object_set_proto s o v in
        res_spec s' (res_val (Coq_value_bool true))
    end
    in repeat v false

(** @essec 9.1.3
    @esid sec-ordinary-object-internal-methods-and-internal-slots-isextensible *)
and ordinary_object_internal_is_extensible s o =
  let%value (s1, v) = ordinary_is_extensible s o in
  res_out s (res_val v)

(** @essec 9.1.3.1
    @esid sec-ordinaryisextensible *)
and ordinary_is_extensible s o =
  let%some b = run_object_method object_extensible_ s o in
  res_out s (res_val (Coq_value_bool b))

(** @essec 9.1.4
    @esid sec-ordinary-object-internal-methods-and-internal-slots-preventextensions *)
and ordinary_object_internal_prevent_extensions s o =
  let%value (s1, v) = ordinary_prevent_extensions s o in
  res_out s (res_val v)

(** @essec 9.1.4.1
    @esid sec-ordinarypreventextensions *)
and ordinary_prevent_extensions s o =
  let%some s' = run_object_set_internal object_set_extensible s o false in
  res_spec s (res_val (Coq_value_bool true))

(** @essec 9.1.5
    @esid sec-ordinary-object-internal-methods-and-internal-slots-getownproperty-p *)
and ordinary_object_internal_get_own_property s o p =
  let%spec s, d = ordinary_get_own_property s o p in
  res_spec s d

(** @essec 9.1.5.1
    @esid sec-ordinarygetownproperty *)
and ordinary_get_own_property s o p =
  let%assert _ = is_property_key p in
  let p = string_of_value p in
  if not (object_property_exists s o p) then res_spec s Descriptor_undef
  else let d = descriptor_intro_empty in
  let%some x = object_retrieve_property s o p in
  let d = match x with
  | Coq_attributes_data_of x ->
      { d with descriptor_value    = (Some x.attributes_data_value);
               descriptor_writable = (Some x.attributes_data_writable) }
  | Coq_attributes_accessor_of x ->
      { d with descriptor_get = (Some x.attributes_accessor_get);
               descriptor_set = (Some x.attributes_accessor_set) } in
  let d = { d with descriptor_enumerable   = (Some (attributes_enumerable x)) } in
  let d = { d with descriptor_configurable = (Some (attributes_configurable x)) } in
  res_spec s (Descriptor d)

(** @essec 9.1.6
    @esid sec-ordinary-object-internal-methods-and-internal-slots-defineownproperty-p-desc *)
and ordinary_object_internal_define_own_property s o p desc =
  ordinary_define_own_property s o p desc

(** @essec 9.1.6.1
    @esid sec-ordinarydefineownproperty *)
and ordinary_define_own_property s o p desc =
  let%spec (s, current) = object_internal_get_own_property s o p in
  let%some extensible = run_object_method object_extensible_ s o in
  validate_and_apply_property_descriptor s (Coq_value_object o) p extensible desc current

(** @essec 9.1.6.2
    @esid sec-iscompatiblepropertydescriptor *)
and is_compatible_property_descriptor s extensible desc current =
  validate_and_apply_property_descriptor s Coq_value_undef Coq_value_undef extensible desc current

(** @essec 9.1.6.3
    @esid sec-validateandapplypropertydescriptor *)
and validate_and_apply_property_descriptor s o p extensible desc current =
  (* FIXME: o, p type mismatch, specified as object, property key but undefined passed *)
  (* A -> B === !A || B *)
  let%assert _ = (value_compare o Coq_value_undef) || (is_property_key p) in
  let p = string_of_value p in (* FIXME: Will break with Symbols *)
  match current with
  (* Three types of descriptors: full, attributes: accessor and data...
     Spec assumes one with variable field definitions, move to this? (equiv. our full)
   *)
  | Descriptor_undef ->
    if not extensible then res_out s (res_val (Coq_value_bool false))
    else
      let%assert _ = extensible in (* SPEC: This assert assumes extensible is any value, but is strictly typed for us *)
      let%some s =
        match o with
        | Coq_value_object l ->
          if (is_generic_descriptor (Descriptor desc)) || (is_data_descriptor (Descriptor desc))
          then object_set_property s l p (Coq_attributes_data_of (attributes_data_of_descriptor desc))
          else object_set_property s l p (Coq_attributes_accessor_of (attributes_accessor_of_descriptor desc))
        | Coq_value_undef -> Some s
        | _ -> None
      in res_ter s (res_val (Coq_value_bool true))

  | Descriptor current ->
    (* The following two steps 3 & 4 of the spec are implied by the rest of the function, except in the
       case of NaN values for [[Value]] which may change internal representation.
       Otherwise, they are only an optimisation as far as I can tell. *)
    (* Step 3 (also implied by step 4) *)
    let%ret s = if descriptor_is_empty desc
      then Return (res_ter s (res_val (Coq_value_bool true)))
      else Continue s

    (* Step 4 *)
    in let%ret s =
    if descriptor_contained_by desc current same_value
      then Return (res_ter s (res_val (Coq_value_bool true)))
      else Continue s

    (* Steps 5 *)
    in let%ret s =
    if (option_compare bool_eq current.descriptor_configurable (Some false))
    then
      if (option_compare bool_eq desc.descriptor_configurable (Some true))
      then Return (res_ter s (res_val (Coq_value_bool false)))
      else if ((is_some desc.descriptor_enumerable) &&
        not (some_compare bool_eq current.descriptor_enumerable desc.descriptor_enumerable))
      then Return (res_ter s (res_val (Coq_value_bool false)))
      else Continue s
    else Continue s

    (* Step 6 if IsGenericDescriptor(Desc) is true, implied by following conditions *)
    in let%ret s =
    if is_generic_descriptor (Descriptor desc)
    then Continue s

    (* Step 7 *)
    else if not (bool_eq (is_data_descriptor (Descriptor current)) (is_data_descriptor (Descriptor desc)))
    then
      (* 7a *)
      if (option_compare bool_eq current.descriptor_configurable (Some false))
        then Return (res_ter s (res_val (Coq_value_bool false)))
      (* 7b *)
      else if is_data_descriptor (Descriptor current)
        then
          let s = unsome_default s (match o with
          | Coq_value_object l -> object_map_property s l p attributes_accessor_of_attributes_data
          | _ -> Some s)
          in Continue s
        else
          let s = unsome_default s (match o with
          | Coq_value_object l -> object_map_property s l p attributes_data_of_attributes_accessor
          | _ -> Some s)
          in Continue s

    (* Step 8 *)
    else if (is_data_descriptor (Descriptor current)) && (is_data_descriptor (Descriptor desc))
    then
      (* Step 8a *)
      if option_compare bool_eq current.descriptor_configurable (Some false)
      then
        (* Step 8ai *)
        if (option_compare bool_eq current.descriptor_writable (Some false))
           && (option_compare bool_eq desc.descriptor_writable (Some true))
        then Return (res_ter s (res_val (Coq_value_bool false)))

        (* Step 8aii *)
        else if option_compare bool_eq current.descriptor_writable (Some false)
        then
          (* Step 8aii1 *)
          if (is_some desc.descriptor_value)
             && not (option_compare same_value desc.descriptor_value current.descriptor_value)
          then Return (res_ter s (res_val (Coq_value_bool false)))
          else Continue s
        else Continue s

      (* Step 8b *)
      else if not (option_compare bool_eq current.descriptor_configurable (Some true))
        then Return (spec_assertion_failure ())
        else Continue s

    (* Step 9 *)
    else if not ((is_accessor_descriptor (Descriptor current)) && (is_accessor_descriptor (Descriptor desc)))
      then Return (spec_assertion_failure ())
      else if option_compare bool_eq current.descriptor_configurable (Some false)
      then
        if (is_some desc.descriptor_set) &&
            not (option_compare same_value desc.descriptor_set current.descriptor_set)
        then Return (res_ter s (res_val (Coq_value_bool false)))
        else if (is_some desc.descriptor_get) &&
                not (option_compare same_value desc.descriptor_get current.descriptor_get)
        then Return (res_ter s (res_val (Coq_value_bool false)))
        else Continue s
      else Continue s

    (* Step 10 *)
    in let%some s = match o with
    | Coq_value_object l ->
       object_map_property s l p (fun a -> attributes_update a desc)
    | _ -> Some s

    (* Step 11 *)
    in res_ter s (res_val (Coq_value_bool true))

(** @essesc 9.1.7
    @esid sec-ordinary-object-internal-methods-and-internal-slots-hasproperty-p *)
and ordinary_object_internal_has_property s o p =
  ordinary_has_property s o p

(** @essec 9.1.7.1
    @esid sec-ordinaryhasproperty *)
and ordinary_has_property s o p =
  let%assert _ = is_property_key p in
  let%spec s, hasOwn = object_internal_get_own_property s o p in
  if not (hasOwn === Descriptor_undef) then res_ter s (res_val (Coq_value_bool true))
  else let%value s, parent = object_internal_get_prototype_of s o in
  if not (parent === Coq_value_null) then object_internal_has_property s (loc_of_value parent) p
  else res_ter s (res_val (Coq_value_bool false))

(** @essec 9.1.8
    @esid sec-ordinary-object-internal-methods-and-internal-slots-get-p-receiver *)
and ordinary_object_internal_get s o p receiver =
  ordinary_get s o p receiver

(** @essec 9.1.8.1
    @esid sec-ordinaryget *)
and ordinary_get s o p receiver =
  let%assert _ = is_property_key p in
  let%spec s, desc = object_internal_get_own_property s o p in
  if desc === Descriptor_undef then
    let%value s, parent = object_internal_get_prototype_of s o in
    if parent === Coq_value_null then
      res_ter s (res_val Coq_value_undef)
    else
      let parent = loc_of_value parent in
      object_internal_get s parent p receiver
  else if is_data_descriptor desc then
    let desc = descriptor_get_defined desc in
    let%some value = descriptor_value desc in
    res_ter s (res_val value)
  else
    let%assert _ = is_accessor_descriptor desc in
    let desc = descriptor_get_defined desc in
    let%some getter = descriptor_get desc in (* None value should be invalid as it represents "absent" in spec *)
    if getter === Coq_value_undef then
      res_ter s (res_val Coq_value_undef)
    else
      call s getter receiver None

(** @essec 9.1.9
    @esid sec-ordinary-object-internal-methods-and-internal-slots-set-p-v-receiver *)
and ordinary_object_internal_set s o p v receiver =
  ordinary_set s o p v receiver

(** @essec 9.1.9.1
    @esid sec-ordinaryset *)
and ordinary_set s o p v receiver =
  let%assert _ = is_property_key p in
  let%spec s, ownDesc = object_internal_get_own_property s o p in
  let%ret ownDesc =
    if ownDesc === Descriptor_undef then begin
      let%value_ret s, parent = object_internal_get_prototype_of s o in
      if not (parent === Coq_value_null) then
        let parent = loc_of_value parent in
        Return (object_internal_set s parent p v receiver)
      else Continue (Descriptor {
        descriptor_value = Some Coq_value_undef;
        descriptor_writable = Some false;
        descriptor_get = None;
        descriptor_set = None;
        descriptor_enumerable = Some true;
        descriptor_configurable = Some true
      })
    end else Continue ownDesc
  in
  if is_data_descriptor ownDesc then begin
    let ownDesc = descriptor_get_defined ownDesc in
    let%some writable = ownDesc.descriptor_writable in
    if writable then res_ter s (res_val (Coq_value_bool false))
    else if not (type_of receiver === Coq_type_object) then res_ter s (res_val (Coq_value_bool false))
    else
      let receiver = loc_of_value receiver in
      let%spec s, existingDescriptor = object_internal_get_own_property s receiver p in
      if not (existingDescriptor === Descriptor_undef) then begin
        if is_accessor_descriptor existingDescriptor then res_ter s (res_val (Coq_value_bool false))
        else
          let existingDescriptor = descriptor_get_defined existingDescriptor in
          let%some w = existingDescriptor.descriptor_writable in
          if not w then res_ter s (res_val (Coq_value_bool false))
          else
            let valueDesc = (descriptor_with_value descriptor_intro_empty (Some v)) in
            object_internal_define_own_property s receiver p valueDesc
      end else
        create_data_property s (Coq_value_object receiver) p v
  end else
    let%assert _ = is_accessor_descriptor ownDesc in
    let ownDesc = descriptor_get_defined ownDesc in
    let%some setter = ownDesc.descriptor_set in
    if setter === Coq_value_undef then res_ter s (res_val (Coq_value_bool false))
    else
      let%spec s, _ = call s setter receiver (Some [v]) in
      res_ter s (res_val (Coq_value_bool true))

(** @essec 9.1.10
    @esid sec-ordinary-object-internal-methods-and-internal-slots-delete-p *)
and ordinary_object_internal_delete s o p =
  ordinary_delete s o p

(** @essec 9.1.10.1
    @esid sec-ordinarydelete *)
and ordinary_delete s o p =
  let%assert _ = is_property_key p in
  let%spec s, desc = object_internal_get_own_property s o p in
  if desc === Descriptor_undef then
    res_ter s (res_val (Coq_value_bool true))
  else
  let desc = descriptor_get_defined desc in
  if some_compare (===) (descriptor_configurable desc) (Some true) then
    let p = string_of_value p in
    let%some s = run_object_heap_map_properties s o (fun props -> HeapStr.rem props p)  in
    res_ter s (res_val (Coq_value_bool true))
  else
    res_ter s (res_val (Coq_value_bool false))

and ordinary_object_internal_own_property_keys s o =
  let%spec s, k = ordinary_own_property_keys s o in
  res_spec s k

(** @essec 9.1.11.1
    @esid sec-ordinaryownpropertykeys *)
and ordinary_own_property_keys s o =
  let%some keys = object_properties_keys_as_list_option s o in
  (* FIXME: Precise key ordering is to be implemented here! *)
  res_spec s keys

(** {2 Proxy Object Internal Methods and Internal Slots}
    @essec 9.5
    @esid sec-proxy-object-internal-methods-and-internal-slots *)

(*
  outline implementation:

(** @essec 9.5.
    @esid  *)
and proxy_object_internal_ s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Coq_value_null then run_error_no_c s Coq_native_error_type
  else
  let%assert _ = (type_of handler) === Coq_type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Coq_value_string "") in
  if trap === Coq_value_undef then

*)

(** @essec 9.5.1
    @esid sec-proxy-object-internal-methods-and-internal-slots-getprototypeof *)
and proxy_object_internal_get_prototype_of s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Coq_value_null then run_error_no_c s Coq_native_error_type
  else
  let%assert _ = (type_of handler) === Coq_type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Coq_value_string "getPrototypeOf") in
  if trap === Coq_value_undef then
    object_internal_get_prototype_of s (loc_of_value target)
  else
  let%value s, handlerProto = call s trap handler (Some [target]) in
  if not ((type_of handlerProto === Coq_type_object) || (handlerProto === Coq_value_null)) then
    run_error_no_c s Coq_native_error_type
  else
  let%bool s, extensibleTarget = is_extensible s target in
  if extensibleTarget then res_ter s (res_val handlerProto)
  else
  let%value s, targetProto = object_internal_get_prototype_of s (loc_of_value target) in
  if not (same_value handlerProto targetProto) then
    run_error_no_c s Coq_native_error_type
  else
    res_ter s (res_val handlerProto)

(** @essec 9.5.2
    @esid sec-proxy-object-internal-methods-and-internal-slots-setprototypeof-v *)
and proxy_object_internal_set_prototype_of s o v =
  let%assert _ = (type_of v) === Coq_type_object || (type_of v) === Coq_type_null in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Coq_value_null then run_error_no_c s Coq_native_error_type
  else
  let%assert _ = (type_of handler) === Coq_type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Coq_value_string "") in
  if trap === Coq_value_undef then
    object_internal_set_prototype_of s (loc_of_value target) v
  else
  let%value s, tempVal = call s trap handler (Some [target; v]) in
  let booleanTrapResult = to_boolean tempVal in
  if not booleanTrapResult then res_ter s (res_val (Coq_value_bool false))
  else
  let%bool s, extensibleTarget = is_extensible s target in
  if extensibleTarget then res_ter s (res_val (Coq_value_bool true))
  else
  let%value s, targetProto = object_internal_get_prototype_of s (loc_of_value target) in
  if not (same_value v targetProto) then run_error_no_c s Coq_native_error_type
  else res_ter s (res_val (Coq_value_bool true))

(** @essec 9.5.3
    @esid sec-proxy-object-internal-methods-and-internal-slots-isextensible *)
and proxy_object_internal_is_extensible s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Coq_value_null then run_error_no_c s Coq_native_error_type
  else
  let%assert _ = (type_of handler) === Coq_type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Coq_value_string "isExtensible") in
  if trap === Coq_value_undef then
    object_internal_is_extensible s (loc_of_value target)
  else
  let%value s, tempVal = call s trap handler (Some [target]) in
  let booleanTrapResult = to_boolean tempVal in
  let%value s, targetResult = object_internal_is_extensible s (loc_of_value target) in
  if not (same_value (Coq_value_bool booleanTrapResult) targetResult) then run_error_no_c s Coq_native_error_type
  else res_ter s (res_val (Coq_value_bool booleanTrapResult))

(** @essec 9.5.4
    @esid sec-proxy-object-internal-methods-and-internal-slots-preventextensions *)
and proxy_object_internal_prevent_extensions s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Coq_value_null then run_error_no_c s Coq_native_error_type
  else
  let%assert _ = (type_of handler) === Coq_type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Coq_value_string "preventExtensions") in
  if trap === Coq_value_undef then
    object_internal_prevent_extensions s (loc_of_value target)
  else
  let%value s, tempVal = call s trap handler (Some [target]) in
  let booleanTrapResult = to_boolean tempVal in
  let%ret s =
  if booleanTrapResult then
    let%bool_ret s, targetIsExtensible = object_internal_is_extensible s (loc_of_value target) in
    if targetIsExtensible then Return (run_error_no_c s Coq_native_error_type)
    else Continue s
  else Continue s
  in
  res_ter s (res_val (Coq_value_bool booleanTrapResult))


(******** UNCHECKED ES5 IMPLEMENTATION CONTINUES BELOW ***********)

(** @deprecated Compatibility wrapper for ES5 code to ES6
                implementation use [has_property] instead *)
and object_has_prop s c l x =
  object_internal_has_property s l (Coq_value_string x)


(** val out_error_or_void :
    state -> strictness_flag -> native_error -> result **)

and out_error_or_void s c str ne =
  if str then run_error s c ne else res_void s

(** val out_error_or_cst :
    state -> strictness_flag -> native_error -> value -> result **)

and out_error_or_cst s c str ne v =
  if str then run_error s c ne else res_out s (res_val v)

(** val object_get_builtin :
    state -> execution_ctx -> builtin_get -> value -> object_loc
    -> prop_name -> result

    @deprecated ES5 version of function. New versions need to be written and
                dispatched to in {!object_internal_get}.
**)
and object_get_builtin s c b vthis l x =
  let def s0 l0 =
    let%spec (s1, d) = (run_object_get_prop s0 c l0 x) in
    match d with
    | Coq_full_descriptor_undef ->
      res_ter s1 (res_val Coq_value_undef)
    | Coq_full_descriptor_some a ->
      (match a with
       | Coq_attributes_data_of ad ->
         res_ter s1 (res_val ad.attributes_data_value)
       | Coq_attributes_accessor_of aa ->
         (match aa.attributes_accessor_get with
          | Coq_value_undef ->
            res_ter s1 (res_val Coq_value_undef)
          | Coq_value_null -> Coq_result_impossible
          | Coq_value_bool b0 -> Coq_result_impossible
          | Coq_value_number n -> Coq_result_impossible
          | Coq_value_string s2 -> Coq_result_impossible
          | Coq_value_object lf -> run_call s1 c lf vthis [])) in
  let function0 s0 =
    let%value (s_2, v) = (def s0 l) in
    if spec_function_get_error_case_dec s_2 x v
    then run_error s_2 c Coq_native_error_type
    else res_ter s_2 (res_val v) in
  match b with
  | Coq_builtin_get_default -> def s l
  | Coq_builtin_get_function -> function0 s
  | Coq_builtin_get_args_obj -> (
    let%some lmapo = (run_object_method object_parameter_map_ s l) in
    let%some lmap = (lmapo) in
    let%spec (s0, d) = (run_object_get_own_prop s c lmap x) in
    match d with
    | Coq_full_descriptor_undef -> function0 s0
    | Coq_full_descriptor_some a -> run_object_get s0 c lmap x)
  | _ -> Coq_result_not_yet_implemented (* FIXME: Proxy *)

(** val run_object_get :
    state -> execution_ctx -> object_loc -> prop_name -> result

    @deprecated This is the ES5 version, replaced by {!object_internal_get}
    **)
and run_object_get s c l x =
  let%some b = (run_object_method object_get_ s l) in
  object_get_builtin s c b (Coq_value_object l) l x

(** val run_object_get_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

and run_object_get_prop s c l x =
  let%some b = (run_object_method object_get_prop_ s l) in
  match b with Coq_builtin_get_prop_default ->
    let%spec (s1, d) = (run_object_get_own_prop s c l x) in
    if full_descriptor_compare d Coq_full_descriptor_undef
    then let%some proto = (run_object_method object_proto_ s1 l) in
      match proto with
      | Coq_value_null -> res_spec s1 Coq_full_descriptor_undef
      | Coq_value_object lproto ->
        run_object_get_prop s1 c lproto x
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s1
          ("Found a non-object or null value as a prototype in [run_object_get_prop].")
    else res_spec s1 d

(** val object_proto_is_prototype_of :
    state -> object_loc -> object_loc -> result **)

and object_proto_is_prototype_of s l0 l =
  let%some b = (run_object_method object_proto_ s l) in
  match b with
  | Coq_value_null ->
    res_out s (res_val (Coq_value_bool false))
  | Coq_value_object l_2 ->
    if object_loc_compare l_2 l0
    then res_out s (res_val (Coq_value_bool true))
    else object_proto_is_prototype_of s l0 l_2
  | _ ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
      s
      ("[run_object_method] returned a non-object in [object_proto_is_prototype_of_body].")

(** val object_default_value :
    state -> execution_ctx -> object_loc -> preftype option ->
    result **)

and object_default_value s c l prefo =
  let%some b = (run_object_method object_default_value_ s l) in
  match b with Coq_builtin_default_value_default ->
    let gpref = unsome_default Coq_preftype_number prefo in
    let lpref = other_preftypes gpref in
    let sub0 s_2 x k =
      let%value (s1, vfo) = (run_object_get s_2 c l x) in
      let%some co = (run_callable s1 vfo) in
      match co with
      | Some b0 ->
        let%object (s2, lfunc) = (res_out s1 (res_val vfo)) in
        let%value (s3, v) = (run_call s2 c lfunc (Coq_value_object l) []) in begin
          match v with
          | Coq_value_object l0 -> k s3
          | _ -> res_out s3 (res_val v)
        end
      | None -> k s1 in
    let gmeth = (method_of_preftype gpref) in
    sub0 s gmeth (fun s_2 ->
        let lmeth = method_of_preftype lpref in
        sub0 s_2 lmeth (fun s_3 -> run_error s_3 c Coq_native_error_type))

(** val to_primitive :
    state -> execution_ctx -> value -> preftype option -> result **)

and to_primitive s c v prefo =
  match v with
  | Coq_value_object l ->
    let%prim (s0, r) = (object_default_value s c l prefo) in
    res_ter s0 (res_val r)
  | _ -> res_out s (res_val v)

(** val to_number :
    state -> execution_ctx -> value -> result **)

and to_number s c _foo_ = match _foo_ with
  | Coq_value_object l ->
    let%prim (s1, w) = (to_primitive s c (Coq_value_object l) (Some Coq_preftype_number)) in
    res_ter s1 (res_val (Coq_value_number (convert_prim_to_number w)))
  | _ ->
    res_out s (res_val (Coq_value_number (convert_prim_to_number _foo_)))

(** val to_integer :
    state -> execution_ctx -> value -> result **)

and to_integer s c v =
  let%number (s1, n) = to_number s c v in
  res_ter s1
    (res_val (Coq_value_number (convert_number_to_integer n)))

(** val to_int32 :
    state -> execution_ctx -> value -> float specres **)

and to_int32 s c v =
  let%number (s_2, n) = to_number s c v in res_spec s_2 (JsNumber.to_int32 n)

(** val to_uint32 :
    state -> execution_ctx -> value -> float specres **)

and to_uint32 s c v =
  let%number (s_2, n) = to_number s c v in res_spec s_2 (JsNumber.to_uint32 n)

(** val to_string :
    state -> execution_ctx -> value -> result **)

and to_string s c _foo_ = match _foo_ with
  | Coq_value_object l ->
    let%prim (s1, w) = (to_primitive s c (Coq_value_object l) (Some Coq_preftype_string)) in
    res_ter s1
      (res_val (Coq_value_string (convert_prim_to_string w)))
  | _ ->
    res_out s (res_val (Coq_value_string (convert_prim_to_string _foo_)))

(** val run_object_define_own_prop_array_loop :
    state -> execution_ctx -> object_loc -> float -> float ->
    descriptor -> bool -> bool -> (state -> prop_name -> descriptor ->
    strictness_flag -> __ specres) -> result **)

and run_object_define_own_prop_array_loop s c l newLen oldLen newLenDesc newWritable throwcont def =
  if newLen < oldLen
  then let oldLen_2 = (oldLen -. 1.) in
    let%string (s0, slen) = (to_string s c (Coq_value_number oldLen_2)) in
    let%bool (s1, deleteSucceeded) = (object_delete s0 c l slen false) in
    if not deleteSucceeded
    then let newLenDesc0 =
           (descriptor_with_value
              newLenDesc
              (Some (Coq_value_number (oldLen_2 +. 1.)))) in
      let newLenDesc1 = (if not newWritable
                         then descriptor_with_writable newLenDesc0 (Some false)
                         else newLenDesc0) in
      let%bool (s2, x) = (def s1 ("length")
                            newLenDesc1 false) in
      out_error_or_cst s2 c throwcont Coq_native_error_type
        (Coq_value_bool false)
    else run_object_define_own_prop_array_loop s1 c l
        newLen oldLen_2 newLenDesc newWritable throwcont def
  else if not newWritable
  then def s ("length")
      { descriptor_value = None; descriptor_writable = (Some false);
        descriptor_get = None; descriptor_set = None;
        descriptor_enumerable = None; descriptor_configurable = None }
      false
  else res_ter s (res_val (Coq_value_bool true))

(** val object_define_own_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    descriptor -> strictness_flag -> result **)

and object_define_own_prop s c l x desc throwcont =
  let reject s0 throwcont0 =
    out_error_or_cst
      s0 c throwcont0 Coq_native_error_type (Coq_value_bool false) in
  let def s p d _ = ordinary_define_own_property s l (Coq_value_string p) d in
  let%some b = (run_object_method object_define_own_prop_ s l) in
  match b with
  | Coq_builtin_define_own_prop_default ->
      object_internal_define_own_property s l (Coq_value_string x) desc (* ES6 hack *)
  | Coq_builtin_define_own_prop_array ->
    let%spec (s0, d) = (run_object_get_own_prop s c l ("length")) in
    begin
      match d with
      | Coq_full_descriptor_undef ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s0
          ("Array length property descriptor cannot be undefined.")
      | Coq_full_descriptor_some attr ->
        (match attr with
         | Coq_attributes_data_of a ->
           let  oldLen = (a.attributes_data_value) in begin
             match oldLen with
             | Coq_value_object l0 ->
               (Debug.impossible_with_heap_because __LOC__ s0
                  "Spec asserts length of array is number.";
                Coq_result_impossible)
             | _ ->
               let oldLen0 = (JsNumber.to_uint32 (convert_prim_to_number oldLen)) in
               let descValueOpt = (desc.descriptor_value) in
               if string_eq x ("length")
               then (match descValueOpt with
                   | Some descValue ->
                     let%spec (s1, newLen) = (to_uint32 s0 c descValue) in
                     let%number (s2, newLenN) = to_number s1 c descValue in
                     if not (newLen === newLenN)
                     then run_error s2 c Coq_native_error_range
                     else let newLenDesc =
                            (descriptor_with_value desc (Some (Coq_value_number newLen))) in
                       if le_int_decidable oldLen0 newLen
                       then def s2 ("length") newLenDesc throwcont
                       else if not a.attributes_data_writable
                       then reject s2 throwcont
                       else let newWritable = (match newLenDesc.descriptor_writable with
                           | Some b0 -> if b0 then true else false
                           | None -> true) in
                         let newLenDesc0 = (if not newWritable
                                            then descriptor_with_writable newLenDesc (Some true)
                                            else newLenDesc) in
                         let%bool (s3, succ) = (def s2 ("length") newLenDesc0 throwcont) in
                         if not succ
                         then res_ter s3 (res_val (Coq_value_bool false))
                         else run_object_define_own_prop_array_loop s3 c l newLen oldLen0 newLenDesc0 newWritable throwcont def
                   | None -> def s0 ("length") desc throwcont)
               else let%spec (s1, ilen) = (to_uint32 s0 c (Coq_value_string x)) in
                 let%string (s2, slen) = (to_string s1 c (Coq_value_number ilen)) in
                 if (string_eq x slen) && (not ( ilen = 4294967295.))
                 then let%spec (s3, index) = (to_uint32 s2 c (Coq_value_string x)) in
                   if  (le_int_decidable oldLen0 index) && (not a.attributes_data_writable)
                   then reject s3 throwcont
                   else let%bool (s4, b0) = (def s3 x desc false) in
                     if not b0
                     then reject s4 throwcont
                     else if le_int_decidable oldLen0 index
                     then let a0 =
                            descriptor_with_value (descriptor_of_attributes (Coq_attributes_data_of a))
                              (Some (Coq_value_number (index +. 1.))) in
                       def s4 ("length") a0 false
                     else res_ter s4 (res_val (Coq_value_bool true))
                 else def s2 x desc throwcont
           end
         | Coq_attributes_accessor_of a ->
           (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
             s0
             ("Array length property descriptor cannot be accessor."))
    end
  | Coq_builtin_define_own_prop_args_obj ->
    let%some lmapo = (run_object_method object_parameter_map_ s l) in
    let%some lmap = (lmapo) in
    let%spec (s0, d) = (run_object_get_own_prop s c lmap x) in
    let%bool (s1, b0) = (def s0 x desc false) in
    if b0
    then let follow s2 = res_ter s2 (res_val (Coq_value_bool true)) in
      match d with
      | Coq_full_descriptor_undef -> follow s1
      | Coq_full_descriptor_some a ->
        if is_accessor_descriptor (Descriptor desc)
        then let%bool (s2, x0) = (object_delete s1 c lmap x false) in follow s2
        else let follow0 s2 =
               if option_compare bool_eq desc.descriptor_writable (Some false)
               then let%bool (s3, x0) = (object_delete s2 c lmap x false) in
                 follow s3
               else follow s2 in
          match desc.descriptor_value with
          | Some v ->
            let%void s2 = (object_put s1 c lmap x v throwcont) in follow0 s2
          | None -> follow0 s1
    else reject s1 throwcont
  | _ -> Coq_result_not_yet_implemented (* FIXME: Proxy *)

(** val prim_new_object : state -> prim -> result **)

and prim_new_object s _foo_ = match _foo_ with
  | Coq_value_bool b ->
    let o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto)) ("Boolean")) in
    let o = (object_with_primitive_value o1 (Coq_value_bool b)) in
    let (l, s1) = object_alloc s o in
    res_out s1 (res_val (Coq_value_object l))
  | Coq_value_number n ->
    let o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto)) ("Number")) in
     let o = (object_with_primitive_value o1 (Coq_value_number n)) in
     let (l, s1) = object_alloc s o in
     res_out s1 (res_val (Coq_value_object l))
  | Coq_value_string s0 ->
    let o2 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string_proto)) ("String")) in
    let o1 = (object_with_get_own_property o2 Coq_builtin_get_own_prop_string) in
    let o = (object_with_primitive_value o1 (Coq_value_string s0)) in
    let (l, s1) = object_alloc s o in
    let%some s_2 = (run_object_heap_map_properties s1 l
      (fun p -> HeapStr.write p ("length")
         (Coq_attributes_data_of (attributes_data_intro_constant
         (Coq_value_number (number_of_int (strlength s0))))))) in
    res_ter s_2 (res_val (Coq_value_object l))
  | _ ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
      s
      ("[prim_new_object] received an null or undef.")

(** val to_object : state -> value -> result **)

and to_object s _foo_ = match _foo_ with
  | Coq_value_undef -> run_error_no_c s Coq_native_error_type
  | Coq_value_null -> run_error_no_c s Coq_native_error_type
  | Coq_value_bool b -> prim_new_object s _foo_
  | Coq_value_number n -> prim_new_object s _foo_
  | Coq_value_string s0 -> prim_new_object s _foo_
  | Coq_value_object l ->
    res_out s (res_val (Coq_value_object l))

(** val run_object_prim_value : state -> object_loc -> result **)

and run_object_prim_value s l =
  let%some ov = (run_object_method object_prim_value_ s l) in
      let%some v = (ov) in  res_ter s (res_val v)

(** val prim_value_get :
    state -> execution_ctx -> value -> prop_name -> result **)

and prim_value_get s c v x =
  let%object (s_2, l) = (to_object s v) in
      object_get_builtin s_2 c Coq_builtin_get_default v l x

(** val env_record_has_binding :
    state -> execution_ctx -> env_loc -> prop_name -> result **)

and env_record_has_binding s c l x =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        res_out s (res_val (Coq_value_bool (HeapStr.indom_dec ed x)))
      | Coq_env_record_object (l0, pt) -> object_has_prop s c l0 x

(** val lexical_env_get_identifier_ref :
    state -> execution_ctx -> lexical_env -> prop_name ->
    strictness_flag -> ref specres **)

and lexical_env_get_identifier_ref s c x x0 str =
  match x with
  | [] ->
    res_spec s (ref_create_value Coq_value_undef x0 str)
  | l :: x_2 ->
    let%bool (s1, has) = (env_record_has_binding s c l x0) in
        if has
        then res_spec s1 (ref_create_env_loc l x0 str)
        else lexical_env_get_identifier_ref s1 c x_2 x0 str

(** val object_delete :
    state -> execution_ctx -> object_loc -> prop_name ->
    strictness_flag -> result **)

and object_delete s c l x str =
  let%some b = (run_object_method object_delete_ s l) in
      match b with
      | Coq_builtin_delete_default -> object_delete_default s c l x str
      | Coq_builtin_delete_args_obj ->
        begin
          let%some mo = (run_object_method object_parameter_map_ s l) in
          let%some m = (mo) in
          let%spec (s1, d) = (run_object_get_own_prop s c m x) in
          let%bool (s2, b0) = (object_delete_default s1 c l x str) in
          if b0 then (match d with
            | Coq_full_descriptor_undef ->
              res_ter s2 (res_val (Coq_value_bool b0))
            | Coq_full_descriptor_some a ->
              let%bool (s3, b_2) = (object_delete s2 c m x false) in
              res_ter s3 (res_val (Coq_value_bool b0)))
          else res_ter s2 (res_val (Coq_value_bool b0))
        end
      | _ -> Coq_result_not_yet_implemented (* FIXME: Proxy *)

(** val env_record_delete_binding :
    state -> execution_ctx -> env_loc -> prop_name -> result **)

and env_record_delete_binding s c l x =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        (match HeapStr.read_option ed x with
         | Some p ->
           let (mu, v) = p in
           (match mu with
            | Coq_mutability_uninitialized_immutable ->
              res_out s (res_val (Coq_value_bool false))
            | Coq_mutability_immutable ->
              res_out s (res_val (Coq_value_bool false))
            | Coq_mutability_nondeletable ->
              res_out s (res_val (Coq_value_bool false))
            | Coq_mutability_deletable ->
              let s_2 =
                env_record_write s l (Coq_env_record_decl (decl_env_record_rem ed x))
              in
              res_out s_2 (res_val (Coq_value_bool true)))
         | None ->
           res_out s (res_val (Coq_value_bool true)))
      | Coq_env_record_object (l0, pt) ->
        object_delete s c l0 x throw_false

(** val env_record_implicit_this_value : state -> env_loc -> value option **)

and env_record_implicit_this_value s l =
  ifx_some_or_default (env_record_binds_option s l) None (fun e ->
      Some
        (match e with
         | Coq_env_record_decl ed -> Coq_value_undef
         | Coq_env_record_object (l0, provide_this) ->
           if provide_this
           then Coq_value_object l0
           else Coq_value_undef))

(** val identifier_resolution :
    state -> execution_ctx -> prop_name -> ref specres **)

and identifier_resolution s c x =
  let x0 = c.execution_ctx_lexical_env in
  let str = c.execution_ctx_strict in
  lexical_env_get_identifier_ref s c x0 x str

(** @deprecated ES5 *)
and env_record_get_binding_value s c l x str =
  get_binding_value s l (Coq_value_string x) str

(** @deprecated ES5 *)
and ref_get_value s c _foo_ =
  let%value s, v = get_value s (res_ter s (res_normal _foo_)) in
  res_spec s v

(** @deprecated ES5 *)
and run_expr_get_value s c e =
  let%value s, v = get_value s (run_expr s c e) in
  res_spec s v

(** val env_record_set_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> value ->
    strictness_flag -> result_void **)

and env_record_set_mutable_binding s c l x v str =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        let%some rm = (HeapStr.read_option ed x) in
            let (mu, v_old) = rm in
            if not (mutability_compare mu Coq_mutability_immutable)
            then res_void (env_record_write_decl_env s l x mu v)
            else out_error_or_void s c str Coq_native_error_type
      | Coq_env_record_object (l0, pt) -> object_put s c l0 x v str

(** @deprecated ES5 *)
and ref_put_value s c rv v =
  put_value s c (res_ter s (res_normal rv)) (res_ter s (res_val v))

(** val env_record_create_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> bool
    option -> result_void **)

and env_record_create_mutable_binding s c l x deletable_opt =
  let  deletable = (unsome_default false deletable_opt) in
      let%some e = (env_record_binds_option s l) in
          match e with
          | Coq_env_record_decl ed ->
            if HeapStr.indom_dec ed x
            then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s
                ("Already declared environnment record in [env_record_create_mutable_binding].")
            else let s_2 = (env_record_write_decl_env s l x
                              (mutability_of_bool deletable) Coq_value_undef) in
              res_void s_2
          | Coq_env_record_object (l0, pt) ->
            let%bool (s1, has) = (object_has_prop s c l0 x) in
              if has
              then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                    s1
                    ("Already declared binding in [env_record_create_mutable_binding].")
              else let  a = ({ attributes_data_value = Coq_value_undef; attributes_data_writable = true;
                                   attributes_data_enumerable = true;
                                   attributes_data_configurable = deletable }) in
                    let%success
                       (s2, rv) = (object_define_own_prop s1 c l0 x
                         (descriptor_of_attributes (Coq_attributes_data_of a))
                         throw_true) in  res_void s2

(** val env_record_create_set_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> bool
    option -> value -> strictness_flag -> result_void **)

and env_record_create_set_mutable_binding s c l x deletable_opt v str =
  let%void
    s0 = (env_record_create_mutable_binding s c l x deletable_opt) in  env_record_set_mutable_binding s0 c l x v str

(** val env_record_create_immutable_binding :
    state -> env_loc -> prop_name -> result_void **)

and env_record_create_immutable_binding s l x =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        if HeapStr.indom_dec ed x
        then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("Already declared environnment record in [env_record_create_immutable_binding].")
        else res_void
            (env_record_write_decl_env s l x
               Coq_mutability_uninitialized_immutable Coq_value_undef)
      | Coq_env_record_object (o, p) ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("[env_record_create_immutable_binding] received an environnment record object.")

(** val env_record_initialize_immutable_binding :
    state -> env_loc -> prop_name -> value -> result_void **)

and env_record_initialize_immutable_binding s l x v =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        let%some evs = (decl_env_record_option ed x) in
            if prod_compare mutability_compare value_compare evs
                (Coq_mutability_uninitialized_immutable, Coq_value_undef)
            then let s_2 = (env_record_write_decl_env s l x Coq_mutability_immutable v) in res_void s_2
            else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s
                ("Non suitable binding in [env_record_initialize_immutable_binding].")
      | Coq_env_record_object (o, p) ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("[env_record_initialize_immutable_binding] received an environnment record object.")

(** val call_object_new : state -> value -> result **)

and call_object_new s c v =
  match type_of v with
  | Coq_type_undef ->
    let o = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto))
      ("Object")) in
    let  p = object_alloc s o in
    let (l, s_2) = p in
    res_out s_2 (res_val (Coq_value_object l))
  | Coq_type_null ->
    let o = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto))
      ("Object")) in
    let  p = object_alloc s o in
    let (l, s_2) = p in
    res_out s_2 (res_val (Coq_value_object l))
  | Coq_type_bool -> to_object s v
  | Coq_type_number -> to_object s v
  | Coq_type_string -> to_object s v
  | Coq_type_object -> res_out s (res_val v)

(** val array_args_map_loop :
    state -> execution_ctx -> object_loc -> value list -> float
    -> result_void **)

and array_args_map_loop s c l args ind =
  match args with
  | [] -> res_void s
  | h :: rest ->
    let%some s_2 = (run_object_heap_map_properties s l (fun p ->
           HeapStr.write p (JsNumber.to_string ind)
             (Coq_attributes_data_of (attributes_data_intro_all_true h)))) in
             array_args_map_loop s_2 c l rest (ind +. 1.)

(** val run_construct_prealloc :
    state -> execution_ctx -> prealloc -> value list -> result **)

and run_construct_prealloc s c b args =
  match b with
  | Coq_prealloc_object ->
    let v = (get_arg 0 args) in call_object_new s c v
  | Coq_prealloc_bool ->
    let v = get_arg 0 args in
    let b0 = to_boolean v in
    let o1 = object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto))
      ("Boolean") in
    let o = object_with_primitive_value o1 (Coq_value_bool b0) in
    let  p = object_alloc s o in
    let (l, s_2) = p in
    res_out s_2 (res_val (Coq_value_object l))
  | Coq_prealloc_number ->
    let follow = (fun s_2 v ->
      let o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto))
        ("Number")) in
      let o = object_with_primitive_value o1 v in
      let (l, s1) = object_alloc s_2 o in
      res_out s1 (res_val (Coq_value_object l))) in
    if list_eq_nil_decidable args
    then follow s (Coq_value_number JsNumber.zero)
    else
      let v = get_arg 0 args in
      let%number (x, x0) = (to_number s c v) in
      follow x (Coq_value_number x0)
  | Coq_prealloc_array ->
    let o_2 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_array_proto))
      ("Array")) in
    let o = (object_for_array o_2 Coq_builtin_define_own_prop_array) in
    let p = (object_alloc s o) in
    let (l, s_2) = p in
    let follow = (fun s_3 length0 ->
      let%some s0 = (run_object_heap_map_properties s_3 l (fun p0 ->
        HeapStr.write p0 ("length") (Coq_attributes_data_of
          { attributes_data_value = (Coq_value_number length0);
            attributes_data_writable = true;
            attributes_data_enumerable = false;
            attributes_data_configurable = false }))) in
      res_ter s0 (res_val (Coq_value_object l))) in
    let arg_len = (LibList.length args) in
    if nat_eq arg_len 1
    then let v = get_arg 0 args in
    match v with
    | Coq_value_undef ->
      let%some s0 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Coq_attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s0 1.0
    | Coq_value_null ->
      let%some s0 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Coq_attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s0 1.0
    | Coq_value_bool b0 ->
      let%some s0 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Coq_attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s0 1.0
    | Coq_value_number vlen ->
      let%spec (s0, ilen) = (to_uint32 s_2 c (Coq_value_number vlen)) in
      if ilen === vlen
      then follow s0 ilen
      else run_error s0 c Coq_native_error_range
    | Coq_value_string s0 ->
      let%some s1 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Coq_attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s1 1.0
    | Coq_value_object o0 ->
      let%some
         s0 = (run_object_heap_map_properties s_2 l
           (fun p0 ->
              HeapStr.write p0 ("0") (Coq_attributes_data_of
                                     (attributes_data_intro_all_true v)))) in
            follow s0 1.0
                          else let%some
                               s0 = (run_object_heap_map_properties s_2 l
                                 (fun p0 ->
                                    HeapStr.write p0
                                      ("length")
                                      (Coq_attributes_data_of { attributes_data_value =
                                                                  (Coq_value_number (number_of_int arg_len));
                                                                attributes_data_writable = true;
                                                                attributes_data_enumerable = false;
                                                                attributes_data_configurable = false }))) in
                                  let%void
                                    s1 = (array_args_map_loop s0 c l args 0.) in  res_ter s1 (res_val (Coq_value_object l))
  | Coq_prealloc_string ->
    let
       o2 = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                       Coq_prealloc_string_proto))
         ("String")) in
          let

            o1 = (object_with_get_own_property o2 Coq_builtin_get_own_prop_string) in
               let  follow = (fun s0 s1 ->
                   let
                      o = (object_with_primitive_value o1 (Coq_value_string s1)) in
                         let (l, s2) = object_alloc s0 o in
                         let lenDesc = (attributes_data_intro_constant (Coq_value_number (number_of_int (strlength s1)))) in
                              let%some
                                 s_2 = (run_object_heap_map_properties s2 l (fun p ->
                                     HeapStr.write p ("length")
                                       (Coq_attributes_data_of lenDesc))) in
                                    res_ter s_2 (res_val (Coq_value_object l))) in
                   let  arg_len = (LibList.length args) in
                       if nat_eq arg_len 0
                       then follow s ""
                       else let  arg = (get_arg 0 args) in
                           let%string (s0, s1) = (to_string s c arg) in
                               follow s0 s1
  | Coq_prealloc_error ->
    let  v = (get_arg 0 args) in
        build_error s c (Coq_value_object (Coq_object_loc_prealloc
                                           Coq_prealloc_error_proto)) v
  | Coq_prealloc_native_error ne ->
    let  v = (get_arg 0 args) in
        build_error s c (Coq_value_object (Coq_object_loc_prealloc
                                           (Coq_prealloc_native_error_proto ne))) v
  | _ ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_not_yet_implemented)
      (strappend
         ("Construct prealloc_")
         (strappend (string_of_prealloc b)
            (" not yet implemented.")))

(** val run_construct_default :
    state -> execution_ctx -> object_loc -> value list -> __
    specres **)

and run_construct_default s c l args =
  let%value

    (s1, v1) = (run_object_get s c l
       ("prototype")) in
       let
          vproto = (if type_compare (type_of v1) Coq_type_object
          then v1
          else Coq_value_object (Coq_object_loc_prealloc
                                   Coq_prealloc_object_proto)) in
             let

               o = (object_new vproto ("Object")) in
                  let  p = (object_alloc s1 o) in
                      let (l_2, s2) = p in
                      let%value
                        (s3, v2) = (run_call s2 c l (Coq_value_object l_2) args) in
                           let
                              vr = (if type_compare (type_of v2) Coq_type_object
                              then v2
                              else Coq_value_object l_2) in res_ter s3 (res_val vr)

(** val run_construct :
    state -> execution_ctx -> construct -> object_loc -> value
    list -> result **)

and run_construct s c co l args =
  match co with
  | Coq_construct_default -> run_construct_default s c l args
  | Coq_construct_after_bind ->
    let%some otrg = run_object_method object_target_function_ s l in
    let%some target = (otrg) in
    let%some oco = run_object_method object_construct_ s target in begin
      match oco with
      | Some co0 ->
        let%some oarg = run_object_method object_bound_args_ s l in
        let%some boundArgs = oarg in
        let  arguments_ = (LibList.append boundArgs args) in
            run_construct s c co0 target arguments_
      | None -> run_error s c Coq_native_error_type
    end
  | Coq_construct_prealloc b -> run_construct_prealloc s c b args
  | _ -> Coq_result_not_yet_implemented (* FIXME: Proxy *)

(** val run_call_default :
    state -> execution_ctx -> object_loc -> result **)

and run_call_default s c lf =
  let def = res_out s (res_val Coq_value_undef) in
  let%some oC = (run_object_method object_code_ s lf) in
  match oC with
  | Some bd ->
    if list_eq_nil_decidable (prog_elements (funcbody_prog bd))
    then def
    else ifx_success_or_return (run_prog s c (funcbody_prog bd))
      (fun s_2 -> res_out s_2 (res_val Coq_value_undef))
      (fun s_2 rv -> res_out s_2 (res_normal rv))
  | None -> def

(** val creating_function_object_proto :
    state -> execution_ctx -> object_loc -> result **)

and creating_function_object_proto s c l =
  let%object
    (s1, lproto) = (run_construct_prealloc s c Coq_prealloc_object []) in
       let  a1 = ({ attributes_data_value = (Coq_value_object l);
                     attributes_data_writable = true; attributes_data_enumerable = false;
                     attributes_data_configurable = true }) in
           let%bool

             (s2, b) = (object_define_own_prop s1 c lproto
                ("constructor")
                (descriptor_of_attributes (Coq_attributes_data_of a1)) false) in
                let  a2 = ({ attributes_data_value = (Coq_value_object lproto);
                              attributes_data_writable = true; attributes_data_enumerable =
                                                                 false; attributes_data_configurable = false }) in
                    object_define_own_prop s2 c l
                      ("prototype")
                      (descriptor_of_attributes (Coq_attributes_data_of a2)) false

(** val creating_function_object :
    state -> execution_ctx -> string list -> funcbody ->
    lexical_env -> strictness_flag -> result **)

and creating_function_object s c names bd x str =
  let
     o = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                     Coq_prealloc_function_proto))
       ("Function")) in
        let  o1 = (object_with_get o Coq_builtin_get_function) in
            let o2 = (object_with_invokation o1 (Some Coq_construct_default) (Some
                                                                         Coq_call_default) (Some Coq_builtin_has_instance_function)) in
             let o3 = (object_with_details o2 (Some x) (Some names) (Some bd) None None None None) in
             let p = (object_alloc s o3) in
             let (l, s1) = p in
             let a1 = ({
               attributes_data_value = (Coq_value_number (number_of_int (LibList.length names)));
               attributes_data_writable = false; attributes_data_enumerable = false;
               attributes_data_configurable = false
             }) in
             let%bool (s2, b2) = (object_define_own_prop s1 c l ("length")
               (descriptor_of_attributes (Coq_attributes_data_of a1)) false) in
             let%bool (s3, b3) = (creating_function_object_proto s2 c l) in
             if not str
             then res_ter s3 (res_val (Coq_value_object l))
             else
               let vthrower = (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_throw_type_error)) in
               let a2 = ({ attributes_accessor_get = vthrower;
                           attributes_accessor_set = vthrower;
                           attributes_accessor_enumerable = false;
                           attributes_accessor_configurable = false }) in
               let%bool (s4, b4) = (object_define_own_prop s3 c l ("caller")
                 (descriptor_of_attributes (Coq_attributes_accessor_of a2)) false) in
               let%bool (s5, b5) = (object_define_own_prop s4 c l ("arguments")
                 (descriptor_of_attributes (Coq_attributes_accessor_of a2)) false) in
               res_ter s5 (res_val (Coq_value_object l))

(** val binding_inst_formal_params :
    state -> execution_ctx -> env_loc -> value list -> string
    list -> strictness_flag -> result_void **)

and binding_inst_formal_params s c l args names str =
  match names with
  | [] -> res_void s
  | argname :: names_2 ->
    let  v = (hd Coq_value_undef args) in
        let  args_2 = (tl args) in
            let%bool (s1, hb) = (env_record_has_binding s c l argname) in
                let
                  follow = (fun s_2 ->
                    let%void

                      s_3= (env_record_set_mutable_binding s_2 c l argname v str) in
                         binding_inst_formal_params s_3 c l args_2 names_2 str) in
                     if hb
                     then follow s1
                     else let%void
                          s2 = (env_record_create_mutable_binding s1 c l argname
                            None) in  follow s2

(** val binding_inst_function_decls :
    state -> execution_ctx -> env_loc -> funcdecl list ->
    strictness_flag -> bool -> result_void **)

and binding_inst_function_decls s c l fds str bconfig =

  match fds with
  | [] -> res_void s
  | fd :: fds_2 ->
    let  fbd = (fd.funcdecl_body) in
        let  str_fd = (funcbody_is_strict fbd) in
            let  fparams = (fd.funcdecl_parameters) in
                let  fname = (fd.funcdecl_name) in
                    let%object
                       (s1, fo) = (creating_function_object s c fparams fbd
                         c.execution_ctx_variable_env str_fd) in
                          let
                            follow = (fun s2 ->
                              let%void
                                 s3= (env_record_set_mutable_binding s2 c l fname
                                   (Coq_value_object fo) str) in
                                    binding_inst_function_decls s3 c l fds_2 str bconfig) in
                               let%bool
                                 (s2, has) = (env_record_has_binding s1 c l fname) in
                                    if has
                                    then if nat_eq l env_loc_global_env_record
                                      then let%spec
                                           (s3, d) = (run_object_get_prop s2 c
                                             (Coq_object_loc_prealloc Coq_prealloc_global)
                                             fname) in
                                              match d with
                                              | Coq_full_descriptor_undef ->
                                                (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                                                  s3
                                                  ("Undefined full descriptor in [binding_inst_function_decls].")
                                              | Coq_full_descriptor_some a ->
                                                if attributes_configurable a
                                                then let  a_2 = ({ attributes_data_value =
                                                                     Coq_value_undef;
                                                                   attributes_data_writable = true;
                                                                   attributes_data_enumerable = true;
                                                                   attributes_data_configurable =
                                                                     bconfig }) in
                                                    let%bool
                                                       (s0, x) = (object_define_own_prop s3 c
                                                         (Coq_object_loc_prealloc
                                                            Coq_prealloc_global) fname
                                                         (descriptor_of_attributes
                                                            (Coq_attributes_data_of a_2))
                                                         true) in follow s0
                                                else if
                                                       (is_accessor_descriptor (Descriptor (descriptor_of_attributes a)))
                                                    || (not (attributes_writable a))
                                                    || (not (attributes_enumerable a))
                                                then run_error s3 c Coq_native_error_type
                                                else follow s3
                                      else follow s2
                                    else let%void
                                         s3 = (env_record_create_mutable_binding s2 c l
                                           fname (Some bconfig)) in  follow s3

(** val make_arg_getter :
    state -> execution_ctx -> prop_name -> lexical_env -> result **)

and make_arg_getter s c x x0 =
  let xbd =
    strappend ("return ")
      (strappend x (";"))
  in
  let bd = Coq_funcbody_intro ((Coq_prog_intro (true, ((Coq_element_stat
                                                          (Coq_stat_return (Some (Coq_expr_identifier x)))) :: []))), xbd)
  in
  creating_function_object s c [] bd x0 true

(** val make_arg_setter :
    state -> execution_ctx -> prop_name -> lexical_env -> result **)

and make_arg_setter s c x x0 =
  let xparam = strappend x ("_arg") in
  let xbd =
    strappend x (strappend (" = ") (strappend xparam ";"))
  in
  let bd = Coq_funcbody_intro ((Coq_prog_intro (true, ((Coq_element_stat
                                                          (Coq_stat_expr (Coq_expr_assign ((Coq_expr_identifier x), None,
                                                                                           (Coq_expr_identifier xparam))))) :: []))), xbd)
  in
  creating_function_object s c (xparam :: []) bd x0 true

(** val arguments_object_map_loop :
    state -> execution_ctx -> object_loc -> string list ->
    int -> value list -> lexical_env -> strictness_flag -> object_loc ->
    string list -> result_void **)

and arguments_object_map_loop s c l xs len args x str lmap xsmap =
  (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
    (fun _ ->
       if list_eq_nil_decidable xsmap
       then res_void s
       else let%some o = (object_binds_option s l) in
           let
              o_2 = (object_for_args_object o lmap Coq_builtin_get_args_obj
                Coq_builtin_get_own_prop_args_obj
                Coq_builtin_define_own_prop_args_obj
                Coq_builtin_delete_args_obj) in
                 res_void (object_write s l o_2))
    (fun len_2 ->
       let  tdl = (take_drop_last args) in
           let (rmlargs, largs) = tdl in
           let  arguments_object_map_loop_2 = (fun s0 xsmap0 ->
               arguments_object_map_loop s0 c l xs len_2 rmlargs x str lmap
                 xsmap0) in
               let  a = (attributes_data_intro_all_true largs) in
                   let%bool

                     (s1, b) = (object_define_own_prop s c l
                        (convert_prim_to_string (Coq_value_number (number_of_int len_2)))
                        (descriptor_of_attributes (Coq_attributes_data_of a)) false) in
                        if ge_nat_decidable len_2 (LibList.length xs)
                        then arguments_object_map_loop_2 s1 xsmap
                        else let dummy = "" in
                          let  x0 = (nth_def dummy len_2 xs) in
                              if    (str)
                                 || (mem_decide string_eq x0 xsmap)
                              then arguments_object_map_loop_2 s1 xsmap
                              else let%object
                                  (s2, lgetter) = (make_arg_getter s1 c x0 x) in
                                     let%object
                                       (s3, lsetter) = (make_arg_setter s2 c x0 x) in
                                          let  a_2 = ({ attributes_accessor_get =
                                                          (Coq_value_object lgetter);
                                                        attributes_accessor_set = (Coq_value_object
                                                                                     lsetter); attributes_accessor_enumerable =
                                                                                                 false; attributes_accessor_configurable =
                                                                                                          true }) in
                                              let%bool

                                                (s4, b_2) = (object_define_own_prop s3 c lmap
                                                   (convert_prim_to_string (Coq_value_number (number_of_int len_2)))
                                                   (descriptor_of_attributes
                                                      (Coq_attributes_accessor_of a_2)) false) in
                                                   arguments_object_map_loop_2 s4 (x0 :: xsmap))
    len

(** val arguments_object_map :
    state -> execution_ctx -> object_loc -> string list ->
    value list -> lexical_env -> strictness_flag -> result_void **)

and arguments_object_map s c l xs args x str =
  let%object
    (s_2, lmap) = (run_construct_prealloc s c Coq_prealloc_object []) in
       arguments_object_map_loop s_2 c l xs (LibList.length args) args x
         str lmap []

(** val create_arguments_object :
    state -> execution_ctx -> object_loc -> string list ->
    value list -> lexical_env -> strictness_flag -> result **)

and create_arguments_object s c lf xs args x str =
  let
     o = (object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
                                                Coq_prealloc_object_proto))
       ("Arguments")
       Heap.empty) in
        let  p = (object_alloc s o) in
            let (l, s_2) = p in
            let  a = ({ attributes_data_value = (Coq_value_number (number_of_int (LibList.length args)));
                          attributes_data_writable = true; attributes_data_enumerable = false;
                          attributes_data_configurable = true }) in
                let%bool

                  (s1, b) = (object_define_own_prop s_2 c l
                     ("length")
                     (descriptor_of_attributes (Coq_attributes_data_of a)) false) in
                     let%void
                       s2= (arguments_object_map s1 c l xs args x str) in
                          if str
                          then let  vthrower = (Coq_value_object (Coq_object_loc_prealloc
                                                                Coq_prealloc_throw_type_error)) in
                              let  a0 = ({ attributes_accessor_get = vthrower;
                                            attributes_accessor_set = vthrower;
                                            attributes_accessor_enumerable = false;
                                            attributes_accessor_configurable = false }) in
                                  let%bool

                                    (s3, b_2) = (object_define_own_prop s2 c l
                                       ("caller")
                                       (descriptor_of_attributes
                                          (Coq_attributes_accessor_of a0)) false) in
                                       let%bool

                                         (s4, b_3) = (object_define_own_prop s3 c l
                                            ("callee")
                                            (descriptor_of_attributes
                                               (Coq_attributes_accessor_of a0)) false) in
                                            res_ter s4 (res_val (Coq_value_object l))
                          else let  a0 = ({ attributes_data_value = (Coq_value_object lf);
                                             attributes_data_writable = true;
                                             attributes_data_enumerable = false;
                                             attributes_data_configurable = true }) in
                              let%bool
                                 (s3, b_2) = (object_define_own_prop s2 c l
                                   ("callee")
                                   (descriptor_of_attributes (Coq_attributes_data_of a0))
                                   false) in
                                    res_ter s3 (res_val (Coq_value_object l))

(** val binding_inst_arg_obj :
    state -> execution_ctx -> object_loc -> prog -> string
    list -> value list -> env_loc -> result_void **)

and binding_inst_arg_obj s c lf p xs args l =
  let arguments_ =
    "arguments"
  in
  let  str = (prog_intro_strictness p) in
      let%object
         (s1, largs) = (create_arguments_object s c lf xs args
           c.execution_ctx_variable_env str) in
            if str
            then let%void
                s2= (env_record_create_immutable_binding s1 l arguments_) in
                   env_record_initialize_immutable_binding s2 l arguments_
                     (Coq_value_object largs)
            else env_record_create_set_mutable_binding s1 c l arguments_ None
                (Coq_value_object largs) false

(** val binding_inst_var_decls :
    state -> execution_ctx -> env_loc -> string list -> bool
    -> strictness_flag -> result_void **)

and binding_inst_var_decls s c l vds bconfig str =
  match vds with
  | [] -> res_void s
  | vd :: vds_2 ->
    let  bivd = (fun s0 ->
        binding_inst_var_decls s0 c l vds_2 bconfig str) in
        let%bool (s1, has) = (env_record_has_binding s c l vd) in
            if has
            then bivd s1
            else let%void
                 s2 = (env_record_create_set_mutable_binding s1 c l vd (Some
                                                                    bconfig) Coq_value_undef str) in  bivd s2

(** val execution_ctx_binding_inst :
    state -> execution_ctx -> codetype -> object_loc option ->
    prog -> value list -> result_void **)

and execution_ctx_binding_inst s c ct funco p args =
  match c.execution_ctx_variable_env with
  | [] ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
      s
      ("Empty [execution_ctx_variable_env] in [execution_ctx_binding_inst].")
  | l :: l0 ->
    let  str = (prog_intro_strictness p) in
        let  follow = (fun s_2 names ->
            let
              bconfig = (codetype_compare ct Coq_codetype_eval) in
                 let  fds = (prog_funcdecl p) in
                     let%void

                       s1= (binding_inst_function_decls s_2 c l fds str bconfig) in
                          let%bool

                            (s2, bdefined) = (env_record_has_binding s1 c l
                               ("arguments")) in
                               let
                                 follow2 = (fun s10 ->
                                   let vds = prog_vardecl p in
                                   binding_inst_var_decls s10 c l vds bconfig str) in
                                    match ct with
                                    | Coq_codetype_func ->
                                      (match funco with
                                       | Some func ->
                                         if bdefined
                                         then follow2 s2
                                         else let%void
                                              s3 = (binding_inst_arg_obj s2 c func p names
                                                args l) in  follow2 s3
                                       | None ->
                                         if bdefined
                                         then follow2 s2
                                         else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                                             s2
                                             ("Weird `arguments\' object in [execution_ctx_binding_inst]."))
                                    | Coq_codetype_global -> follow2 s2
                                    | Coq_codetype_eval -> follow2 s2) in
            match ct with
            | Coq_codetype_func ->
              (match funco with
               | Some func ->
                 let%some
                   nameso = (run_object_method object_formal_parameters_ s func) in
                      let%some names = (nameso) in
                          let%void

                            s_2 = (binding_inst_formal_params s c l args names str) in  follow s_2 names
               | None ->
                 (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                   s
                   ("Non coherent functionnal code type in [execution_ctx_binding_inst]."))
            | Coq_codetype_global ->
              (match funco with
               | Some o ->
                 (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                   s
                   ("Non coherent non-functionnal code type in [execution_ctx_binding_inst].")
               | None -> follow s [])
            | Coq_codetype_eval ->
              (match funco with
               | Some o ->
                 (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                   s
                   ("Non coherent non-functionnal code type in [execution_ctx_binding_inst].")
               | None -> follow s [])

(** val entering_func_code :
    state -> execution_ctx -> object_loc -> value -> value list
    -> result **)

and entering_func_code s c lf vthis args =
  let%some bdo = (run_object_method object_code_ s lf) in
  let%some bd = (bdo) in
  let str = (funcbody_is_strict bd) in
  let follow = (fun s_2 vthis_2 ->
    let%some lexo = (run_object_method object_scope_ s_2 lf) in
    let%some lex = (lexo) in
    let p = (lexical_env_alloc_decl s_2 lex) in
    let (lex_2, s1) = p in
    let c_2 = (execution_ctx_intro_same lex_2 vthis_2 str) in
    let%void s2 = (execution_ctx_binding_inst s1 c_2 Coq_codetype_func
      (Some lf) (funcbody_prog bd) args) in
    run_call_default s2 c_2 lf)
  in
  if str
  then follow s vthis
  else (match vthis with
      | Coq_value_undef ->
        follow s (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global))
      | Coq_value_null ->
        follow s (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_global))
      | Coq_value_bool b -> let%value (s2, v) = (to_object s vthis) in follow s2 v
      | Coq_value_number n -> let%value (s2, v) = (to_object s vthis) in follow s2 v
      | Coq_value_string s0 -> let%value (s2, v) = (to_object s vthis) in follow s2 v
      | Coq_value_object lthis -> follow s vthis)

(** val run_object_get_own_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

and run_object_get_own_prop s c l x =
  let%some b = (run_object_method object_get_own_prop_ s l) in
  let def = (fun s_2 ->
    let%some p = (run_object_method object_properties_ s_2 l) in
    res_spec s_2 (ifx_some_or_default (convert_option_attributes (HeapStr.read_option p x))
      Coq_full_descriptor_undef (fun x -> x))
  ) in
  match b with
  | Coq_builtin_get_own_prop_default -> def s
  | Coq_builtin_get_own_prop_args_obj ->
    let%spec (s1, d) = (def s) in
    begin
      match d with
      | Coq_full_descriptor_undef -> res_spec s1 Coq_full_descriptor_undef
      | Coq_full_descriptor_some a ->
        let%some lmapo = (run_object_method object_parameter_map_ s1 l) in
        let%some lmap = (lmapo) in
        let%spec (s2, d0) = (run_object_get_own_prop s1 c lmap x) in
        let follow = (fun s_2 a0 -> res_spec s_2 (Coq_full_descriptor_some a0)) in
        match d0 with
        | Coq_full_descriptor_undef -> follow s2 a
        | Coq_full_descriptor_some amap ->
          let%value (s3, v) = (run_object_get s2 c lmap x) in
          match a with
          | Coq_attributes_data_of ad ->
            follow s3 (Coq_attributes_data_of (attributes_data_with_value ad v))
          | Coq_attributes_accessor_of aa ->
            (Debug.impossible_with_heap_because __LOC__ s3
              "[run_object_get_own_prop]:  received an accessor property descriptor in a point where the specification suppose it never happens.";
              Coq_result_impossible)
    end
  | Coq_builtin_get_own_prop_string ->
    let%spec (s0, d) = def s in
    (match d with
    | Coq_full_descriptor_undef ->
      let%spec (s1, k) = (to_int32 s0 c (Coq_value_string x)) in
      let%string (s2, s3) = (to_string s1 c (Coq_value_number (JsNumber.absolute k))) in
      if not (string_eq x s3)
      then res_spec s2 Coq_full_descriptor_undef
      else
        let%string (s4, str) = (run_object_prim_value s2 l) in
        let%spec (s5, k0) = (to_int32 s4 c (Coq_value_string x)) in
        let len = (number_of_int (strlength str)) in
        if le_int_decidable len k0
        then res_spec s5 Coq_full_descriptor_undef
        else
          let resultStr = string_sub str (int_of_number k0) 1 (* TODO: check k0 is not negative *) in
          let a = { attributes_data_value = (Coq_value_string resultStr);
                    attributes_data_writable = false; attributes_data_enumerable = true;
                    attributes_data_configurable = false } in
          res_spec s5 (Coq_full_descriptor_some (Coq_attributes_data_of a))
    | Coq_full_descriptor_some a -> res_spec s0 d)
  | _ -> Coq_result_not_yet_implemented (* FIXME: Proxy *)

(** val run_function_has_instance :
    state -> object_loc -> value -> result **)

and run_function_has_instance s c lv _foo_ =
  (match _foo_ with
  | Coq_value_object lo ->
    let%some vproto = (run_object_method object_proto_ s lv) in
      (match vproto with
        | Coq_value_null -> res_ter s (res_val (Coq_value_bool false))
        | Coq_value_object proto ->
          if object_loc_compare proto lo
          then res_ter s (res_val (Coq_value_bool true))
          else run_function_has_instance s c proto (Coq_value_object lo)
        | _ -> (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("Primitive found in the prototype chain in [run_object_has_instance_loop]."))
  | _ -> run_error s c Coq_native_error_type)

(** val run_object_has_instance :
    state -> execution_ctx -> builtin_has_instance -> object_loc
    -> value -> result **)

and run_object_has_instance s c b l v =
  match b with
  | Coq_builtin_has_instance_function ->
    (match v with
     | Coq_value_object lv ->
       let%value (s1, vproto) = (run_object_get s c l ("prototype")) in
       (match vproto with
       | Coq_value_object lproto -> run_function_has_instance s1 c lv (Coq_value_object lproto)
       | _ -> run_error s1 c Coq_native_error_type)
     | _ -> res_out s (res_val (Coq_value_bool false)))
  | Coq_builtin_has_instance_after_bind ->
    let%some ol = run_object_method object_target_function_ s l in
    let%some l0 = ol in
    let%some ob = run_object_method object_has_instance_ s l0 in
    match ob with
    | Some b0 -> run_object_has_instance s c b0 l0 v
    | None -> run_error s c Coq_native_error_type

(** val from_prop_descriptor :
    state -> execution_ctx -> full_descriptor -> result **)

and from_prop_descriptor s c _foo_ = match _foo_ with
  | Coq_full_descriptor_undef ->
    res_out s (res_val Coq_value_undef)
  | Coq_full_descriptor_some a ->
    let%object (s1, l) = run_construct_prealloc s c Coq_prealloc_object [] in
    let follow = (fun s0 x ->
      let a1 = (attributes_data_intro_all_true (Coq_value_bool (attributes_enumerable a))) in
      let%bool (s0_2, x0) = (object_define_own_prop s0 c l ("enumerable")
        (descriptor_of_attributes (Coq_attributes_data_of a1)) throw_false) in
      let a2 = (attributes_data_intro_all_true (Coq_value_bool (attributes_configurable a))) in
      let%bool (s_2, x1) = (object_define_own_prop s0_2 c l ("configurable") (descriptor_of_attributes (Coq_attributes_data_of a2)) throw_false) in
      res_ter s_2 (res_val (Coq_value_object l))) in
    match a with
    | Coq_attributes_data_of ad ->
      let a1 = (attributes_data_intro_all_true ad.attributes_data_value) in
      let%bool (s2, x) = (object_define_own_prop s1 c l ("value")
        (descriptor_of_attributes (Coq_attributes_data_of a1)) throw_false) in
      let a2 = (attributes_data_intro_all_true (Coq_value_bool ad.attributes_data_writable)) in
      let%bool (s3, v) = (object_define_own_prop s2 c l ("writable")
        (descriptor_of_attributes (Coq_attributes_data_of a2)) throw_false) in
      follow s3 v
    | Coq_attributes_accessor_of aa ->
      let a1 = (attributes_data_intro_all_true aa.attributes_accessor_get) in
      let%bool (s2, x) = (object_define_own_prop s1 c l ("get")
        (descriptor_of_attributes (Coq_attributes_data_of a1)) throw_false) in
      let a2 = (attributes_data_intro_all_true aa.attributes_accessor_set) in
      let%bool (s3, v) = (object_define_own_prop s2 c l ("set")
        (descriptor_of_attributes (Coq_attributes_data_of a2)) throw_false) in
      follow s3 v

(** val run_equal :
    state -> execution_ctx -> value -> value -> result **)

and run_equal s c v1 v2 =
  let conv_number = fun s0 v -> to_number s0 c v in
  let conv_primitive = fun s0 v -> to_primitive s0 c v None in
  let checkTypesThen = (fun s0 v3 v4 k ->
    let ty1 = type_of v3 in
    let ty2 = type_of v4 in
    if type_compare ty1 ty2
    then res_out s0 (res_val (Coq_value_bool (equality_test_for_same_type ty1 v3 v4)))
    else k ty1 ty2) in
  checkTypesThen s v1 v2 (fun ty1 ty2 ->
    let dc_conv = (fun v3 f v4 -> let%value (s0, v2_2) = (f s v4) in run_equal s0 c v3 v2_2) in
    let so = fun b -> res_out s (res_val (Coq_value_bool b)) in
    if (type_compare ty1 Coq_type_null) && (type_compare ty2 Coq_type_undef)
    then so true
    else if (type_compare ty1 Coq_type_undef) && (type_compare ty2 Coq_type_null)
    then so true
    else if (type_compare ty1 Coq_type_number) && (type_compare ty2 Coq_type_string)
    then dc_conv v1 conv_number v2
    else if (type_compare ty1 Coq_type_string) && (type_compare ty2 Coq_type_number)
    then dc_conv v2 conv_number v1
    else if type_compare ty1 Coq_type_bool
    then dc_conv v2 conv_number v1
    else if type_compare ty2 Coq_type_bool
    then dc_conv v1 conv_number v2
    else if ((type_compare ty1 Coq_type_string) || (type_compare ty1 Coq_type_number)) && (type_compare ty2 Coq_type_object)
    then dc_conv v1 conv_primitive v2
    else if (type_compare ty1 Coq_type_object) && ((type_compare ty2 Coq_type_string) || (type_compare ty2 Coq_type_number))
    then dc_conv v2 conv_primitive v1
    else so false)

(** val convert_twice :
    ('a2 resultof -> (state -> 'a1 -> ('a1 * 'a1) specres) -> ('a1 * 'a1)
    specres) -> (state -> value -> 'a2 resultof) -> state -> value -> value
    -> ('a1 * 'a1) specres **)

and convert_twice :
  'a1 'a2 . ('a2 resultof -> (state -> 'a1 -> ('a1 * 'a1) specres) -> ('a1 * 'a1) specres) ->
  (state -> value -> 'a2 resultof) -> state -> value -> value -> ('a1 * 'a1) specres
 = fun ifv kC s v1 v2 ->
    ifv (kC s v1) (fun s1 vc1 ->
        ifv (kC s1 v2) (fun s2 vc2 -> res_spec s2 (vc1, vc2)))

(** val convert_twice_primitive :
    state -> execution_ctx -> value -> value -> (prim * prim)
    specres **)

and convert_twice_primitive s c v1 v2 =
  convert_twice ifx_prim (fun s0 v -> to_primitive s0 c v None) s v1 v2

(** val convert_twice_number :
    state -> execution_ctx -> value -> value ->
    (number * number) specres **)

and convert_twice_number s c v1 v2 =
  convert_twice ifx_number (fun s0 v -> to_number s0 c v) s v1 v2

(** val convert_twice_string :
    state -> execution_ctx -> value -> value ->
    (string * string) specres **)

and convert_twice_string s c v1 v2 =
  convert_twice ifx_string (fun s0 v -> to_string s0 c v) s v1 v2

(** val issome : 'a1 option -> bool **)

and issome : 'a1 . 'a1 option -> bool = fun _foo_ ->
  match _foo_ with
  | Some t -> true
  | None -> false

and run_binary_op_add s c v1 v2 =
  let%spec (s1, (w1, w2)) = (convert_twice_primitive s c v1 v2) in
  if  (type_compare (type_of w1) Coq_type_string)
   || (type_compare (type_of w2) Coq_type_string)
  then let%spec (s2, (str1, str2)) = (convert_twice_string s1 c w1 w2) in
    res_out s2 (res_val (Coq_value_string (strappend str1 str2)))
  else let%spec (s2, (n1, n2)) = (convert_twice_number s1 c w1 w2) in
    res_out s2 (res_val (Coq_value_number (n1 +. n2)))

and run_binary_op_arith mathop s c v1 v2 =
  let%spec (s1, nn) = (convert_twice_number s c v1 v2) in
  let (n1, n2) = nn in
  res_out s1 (res_val (Coq_value_number (mathop n1 n2)))

and run_binary_op_shift b_unsigned mathop s c v1 v2 =
  let conv = (if b_unsigned then to_uint32 else to_int32) in
  let%spec (s1, k1) = (conv s c v1) in
  let%spec (s2, k2) = (to_uint32 s1 c v2) in
  let k2_2 = JsNumber.modulo_32 k2 in
  res_ter s2 (res_val (Coq_value_number (mathop k1 k2_2)))

and run_binary_op_bitwise mathop s c v1 v2 =
  let%spec (s1, k1) = (to_int32 s c v1) in
  let%spec (s2, k2) = (to_int32 s1 c v2) in
  res_ter s2 (res_val (Coq_value_number (mathop k1 k2)))

and run_binary_op_compare b_swap b_neg s c v1 v2 =
  let%spec (s1, ww) = convert_twice_primitive s c v1 v2 in
  let (w1, w2) = ww in
  let p = if b_swap then (w2, w1) else (w1, w2) in
  let (wa, wb) = p in
  let wr = inequality_test_primitive wa wb in
  if value_compare wr Coq_value_undef then res_out s1 (res_val (Coq_value_bool false))
  else if (b_neg) && (value_compare wr (Coq_value_bool true))
  then res_out s1 (res_val (Coq_value_bool false))
  else if (b_neg) && (value_compare wr (Coq_value_bool false))
  then res_out s1 (res_val (Coq_value_bool true))
  else res_out s1 (res_val wr)

and run_binary_op_instanceof s c v1 v2 =
  match v2 with
  | Coq_value_object l ->
    let%some b = (run_object_method object_has_instance_ s l) in
    (match b with
    | None -> run_error s c Coq_native_error_type
    | Some has_instance_id -> run_object_has_instance s c has_instance_id l v1)
  | _ -> run_error s c Coq_native_error_type

and run_binary_op_in s c v1 v2 =
  match v2 with
  | Coq_value_object l ->
    let%string (s2, x) = (to_string s c v1) in
    object_has_prop s2 c l x
  | _ -> run_error s c Coq_native_error_type

(** val run_binary_op :
    state -> execution_ctx -> binary_op -> value -> value ->
    result **)

and run_binary_op s c op v1 v2 =
  match op with
  | Coq_binary_op_mult -> run_binary_op_arith (fun x y -> x *. y) s c v1 v2
  | Coq_binary_op_div -> run_binary_op_arith (fun x y -> x /. y) s c v1 v2
  | Coq_binary_op_mod -> run_binary_op_arith (fun x y -> JsNumber.fmod x y) s c v1 v2
  | Coq_binary_op_sub -> run_binary_op_arith (fun x y -> x -. y) s c v1 v2
  | Coq_binary_op_lt -> run_binary_op_compare false false s c v1 v2
  | Coq_binary_op_gt -> run_binary_op_compare true false s c v1 v2
  | Coq_binary_op_le -> run_binary_op_compare true true s c v1 v2
  | Coq_binary_op_ge -> run_binary_op_compare false true s c v1 v2
  | Coq_binary_op_left_shift -> run_binary_op_shift false JsNumber.int32_left_shift s c v1 v2
  | Coq_binary_op_right_shift -> run_binary_op_shift false JsNumber.int32_right_shift s c v1 v2
  | Coq_binary_op_unsigned_right_shift -> run_binary_op_shift true JsNumber.uint32_right_shift s c v1 v2
  | Coq_binary_op_bitwise_and -> run_binary_op_bitwise JsNumber.int32_bitwise_and s c v1 v2
  | Coq_binary_op_bitwise_or  -> run_binary_op_bitwise JsNumber.int32_bitwise_or s c v1 v2
  | Coq_binary_op_bitwise_xor -> run_binary_op_bitwise JsNumber.int32_bitwise_xor s c v1 v2
  | Coq_binary_op_add -> run_binary_op_add s c v1 v2
  | Coq_binary_op_instanceof -> run_binary_op_instanceof s c v1 v2
  | Coq_binary_op_in -> run_binary_op_in s c v1 v2
  | Coq_binary_op_equal -> run_equal s c v1 v2
  | Coq_binary_op_disequal ->
    let%bool (s0, b0) = (run_equal s c v1 v2) in
    res_ter s0 (res_val (Coq_value_bool (not b0)))
  | Coq_binary_op_strict_equal ->
    res_out s (res_val (Coq_value_bool (strict_equality_test v1 v2)))
  | Coq_binary_op_strict_disequal ->
    res_out s (res_val (Coq_value_bool (not (strict_equality_test v1 v2))))
  | Coq_binary_op_coma -> res_out s (res_val v2)
  | Coq_binary_op_and -> Coq_result_impossible
  | Coq_binary_op_or  -> Coq_result_impossible

(** val run_prepost_op : unary_op -> ((number -> number) * bool) option **)

and run_prepost_op _foo_ = match _foo_ with
  | Coq_unary_op_delete -> None
  | Coq_unary_op_void -> None
  | Coq_unary_op_typeof -> None
  | Coq_unary_op_post_incr -> Some (add_one, false)
  | Coq_unary_op_post_decr -> Some (sub_one, false)
  | Coq_unary_op_pre_incr -> Some (add_one, true)
  | Coq_unary_op_pre_decr -> Some (sub_one, true)
  | Coq_unary_op_add -> None
  | Coq_unary_op_neg -> None
  | Coq_unary_op_bitwise_not -> None
  | Coq_unary_op_not -> None

(** val run_typeof_value : state -> value -> string **)

and run_typeof_value s _foo_ =
  match _foo_ with
  | Coq_value_object l ->
    if is_callable_dec s (Coq_value_object l)
    then "function"
    else "object"
  | _ -> typeof_prim _foo_

(** val run_unary_op :
    state -> execution_ctx -> unary_op -> expr -> result **)

and run_unary_op s c op e =
  if prepost_unary_op_dec op
  then
    let%success (s1, rv1)= run_expr s c e in
    let%spec (s2, v2) = ref_get_value s1 c rv1 in
    let%number (s3, n1) = to_number s2 c v2 in
    let%some po = run_prepost_op op in
    let (number_op, is_pre) = po in
    let n2 = number_op n1 in
    let v = Coq_value_number (if is_pre then n2 else n1) in
    let%void s4 = ref_put_value s3 c rv1 (Coq_value_number n2) in
    res_out s4 (res_val v)
  else (match op with
      | Coq_unary_op_delete ->
        let%success (s0, rv)= (run_expr s c e) in begin
            match rv with
            | Coq_resvalue_empty ->
              res_ter s0 (res_val (Coq_value_bool true))
            | Coq_resvalue_value v ->
              res_ter s0 (res_val (Coq_value_bool true))
            | Coq_resvalue_ref r ->
              if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_undef
              then if r.ref_strict
                then run_error s0 c Coq_native_error_syntax
                else res_ter s0
                    (res_val (Coq_value_bool true))
              else (match r.ref_base with
                  | Coq_ref_base_type_value v ->
                    let%object (s1, l) = (to_object s0 v) in
                        object_delete s1 c l r.ref_name
                          r.ref_strict
                  | Coq_ref_base_type_env_loc l ->
                    if r.ref_strict
                    then run_error s0 c Coq_native_error_syntax
                    else env_record_delete_binding s0 c l r.ref_name)
        end
      | Coq_unary_op_typeof ->
        let%success (s1, rv)= (run_expr s c e) in begin
            match rv with
            | Coq_resvalue_empty ->
              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s1
                ("Empty result for a `typeof\' in [run_unary_op].")
            | Coq_resvalue_value v ->
              res_ter s1
                (res_val (Coq_value_string (run_typeof_value s1 v)))
            | Coq_resvalue_ref r ->
              if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_undef
              then res_ter s1
                  (res_val (Coq_value_string ("undefined")))
              else let%spec
                  (s2, v) = (ref_get_value s1 c (Coq_resvalue_ref r)) in
                     res_ter s2
                       (res_val (Coq_value_string (run_typeof_value s2 v)))
        end
      | _ ->
        let%spec (s1, v) = (run_expr_get_value s c e) in
            match op with
            | Coq_unary_op_void ->
              res_ter s1 (res_val Coq_value_undef)
            | Coq_unary_op_add -> to_number s1 c v
            | Coq_unary_op_neg ->
              let%number (s2, n) = (to_number s1 c v) in
                  res_ter s2
                    (res_val (Coq_value_number (JsNumber.neg n)))
            | Coq_unary_op_bitwise_not ->
              let%spec (s2, k) = (to_int32 s1 c v) in
                  res_ter s2
                    (res_val (Coq_value_number (JsNumber.int32_bitwise_not k)))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_bool (not (to_boolean v))))
            | _ ->
              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op]."))

(** val create_new_function_in :
    state -> execution_ctx -> string list -> funcbody ->
    result **)

and create_new_function_in s c args bd =
  creating_function_object s c args bd c.execution_ctx_lexical_env
    c.execution_ctx_strict

(** val init_object :
    state -> execution_ctx -> object_loc -> propdefs -> result **)

and init_object s c l _foo_ = match _foo_ with
  | [] -> res_out s (res_val (Coq_value_object l))
  | p :: pds_2 ->
    let (pn, pb) = p in
    let  x = (string_of_propname pn) in
        let  follows = (fun s1 desc ->
            let%success
              (s2, rv) = (object_define_own_prop s1 c l x desc false) in  init_object s2 c l pds_2) in
            match pb with
            | Coq_propbody_val e0 ->
              let%spec (s1, v0) = (run_expr_get_value s c e0) in
                  let desc = { descriptor_value = (Some v0); descriptor_writable =
                                                               (Some true); descriptor_get = None; descriptor_set = None;
                               descriptor_enumerable = (Some true); descriptor_configurable =
                                                                      (Some true) }
                  in
                  follows s1 desc
            | Coq_propbody_get bd ->
              let%value (s1, v0) = (create_new_function_in s c [] bd) in
                  let desc = { descriptor_value = None; descriptor_writable = None;
                               descriptor_get = (Some v0); descriptor_set = None;
                               descriptor_enumerable = (Some true); descriptor_configurable =
                                                                      (Some true) }
                  in
                  follows s1 desc
            | Coq_propbody_set (args, bd) ->
              let%value (s1, v0) = (create_new_function_in s c args bd) in
                  let desc = { descriptor_value = None; descriptor_writable = None;
                               descriptor_get = None; descriptor_set = (Some v0);
                               descriptor_enumerable = (Some true); descriptor_configurable =
                                                                      (Some true) }
                  in
                  follows s1 desc

(** val run_array_element_list :
    state -> execution_ctx -> object_loc -> expr option list ->
    float -> result **)

and run_array_element_list s c l oes n =
  match oes with
  | [] -> res_out s (res_val (Coq_value_object l))
  | o :: oes_2 ->
    (match o with
     | Some e ->
       let
         loop_result = (fun s0 ->
           run_array_element_list s0 c l oes_2 0.) in
            let%spec (s0, v) = (run_expr_get_value s c e) in
                let%value
                   (s1, vlen) = (run_object_get s0 c l
                     ("length")) in
                      let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
                          let%string
                             (s3, slen) = (to_string s2 c (Coq_value_number (ilen +. n))) in
                                let  desc = ({ attributes_data_value = v;
                                              attributes_data_writable = true;
                                              attributes_data_enumerable = true;
                                              attributes_data_configurable = true }) in
                                    let%bool
                                       (s4, x) = (object_define_own_prop s3 c l slen
                                         (descriptor_of_attributes (Coq_attributes_data_of
                                                                      desc)) false) in
                                          let%object (s5, l0) = (loop_result s4) in
                                              res_ter s5 (res_val (Coq_value_object l0))
     | None ->
       let  firstIndex = (elision_head_count (None :: oes_2)) in
           run_array_element_list s c l
             (elision_head_remove (None :: oes_2)) (number_of_int firstIndex))

(** val init_array :
    state -> execution_ctx -> object_loc -> expr option list ->
    result **)

and init_array s c l oes =
  let elementList = (elision_tail_remove oes) in
  let elisionLength = (elision_tail_count oes) in
  let%object (s0, l0) = (run_array_element_list s c l elementList 0.) in
  let%value (s1, vlen) = (run_object_get s0 c l0 ("length")) in
  let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
  let%spec (s3, len) = (to_uint32 s2 c (Coq_value_number (ilen +. number_of_int elisionLength))) in
  let%not_throw (s4, x) = (object_put s3 c l0 ("length") (Coq_value_number len) throw_false) in
  res_out s4 (res_val (Coq_value_object l0))

(** val run_var_decl_item :
    state -> execution_ctx -> prop_name -> expr option -> result **)

and run_var_decl_item s c x _foo_ = match _foo_ with
  | Some e ->
    let%spec (s1, ir) = (identifier_resolution s c x) in
    let%spec (s2, v) = (run_expr_get_value s1 c e) in
    let%void s3 = (ref_put_value s2 c (Coq_resvalue_ref ir) v) in
    res_out s3 (res_val (Coq_value_string x))
  | None ->
    res_out s (res_val (Coq_value_string x))

(** val run_var_decl :
    state -> execution_ctx -> (prop_name * expr option) list ->
    result **)

and run_var_decl s c _foo_ = match _foo_ with
  | [] -> res_out s res_empty
  | y :: xeos_2 ->
    let (x, eo) = y in
    let%value (s1, vname) = (run_var_decl_item s c x eo) in
        run_var_decl s1 c xeos_2

(** val run_list_expr :
    state -> execution_ctx -> value list -> expr list -> value
    list specres **)

and run_list_expr s1 c vs _foo_ = match _foo_ with
  | [] -> res_spec s1 (rev vs)
  | e :: es_2 ->
    let%spec (s2, v) = (run_expr_get_value s1 c e) in
        run_list_expr s2 c (v :: vs) es_2

(** val run_block :
    state -> execution_ctx -> stat list -> result **)

and run_block s c _foo_ = match _foo_ with
  | [] -> res_ter s (res_normal Coq_resvalue_empty)
  | t :: ts_rev_2 ->
    let%success (s0, rv0)= (run_block s c ts_rev_2) in
      ifx_success_state rv0 (run_stat s0 c t) (fun x x0 -> res_out x (res_normal x0))

and run_binary_op_and s c e1 e2 =
  let%spec (s1, v1) = (run_expr_get_value s c e1) in
  let b1 = (to_boolean v1) in
  if not b1
    then res_ter s1 (res_val v1)
    else let%spec (s2, v) = (run_expr_get_value s1 c e2) in
    res_ter s2 (res_val v)

and run_binary_op_or s c e1 e2 =
  let%spec (s1, v1) = (run_expr_get_value s c e1) in
  let b1 = (to_boolean v1) in
  if b1
    then res_ter s1 (res_val v1)
    else let%spec (s2, v) = (run_expr_get_value s1 c e2) in
    res_ter s2 (res_val v)

(** val run_expr_binary_op :
    state -> execution_ctx -> binary_op -> expr -> expr ->
    result **)

and run_expr_binary_op s c op e1 e2 =
  match op with
  | Coq_binary_op_and -> run_binary_op_and s c e1 e2
  | Coq_binary_op_or -> run_binary_op_or s c e1 e2
  | _ ->
    let%spec (s1,v1) = run_expr_get_value s c e1 in
    let%spec (s2,v2) = run_expr_get_value s1 c e2 in
    run_binary_op s2 c op v1 v2

(** val run_expr_access :
    state -> execution_ctx -> expr -> expr -> result **)

(* TODO DEPRECATEd
   and run_expr_access s c e1 e2 =
   let%spec (s1, v1) = (run_expr_get_value s c e1) in
    let%spec (s2, v2) = (run_expr_get_value s1 c e2) in
      if or_decidable (value_compare v1 Coq_value_undef)
           (value_compare v1 Coq_value_null)
      then run_error s2 Coq_native_error_type
      else let%string (s3, x) = (to_string s2 c v2) in
             res_ter s3
               (res_ref (ref_create_value v1 x c.execution_ctx_strict))
*)

and run_expr_access s c e1 e2 =
  let%spec (s1,v1) = run_expr_get_value s c e1 in
  let%spec (s2,v2) = run_expr_get_value s1 c e2 in
  if    (value_compare v1 Coq_value_undef)
     || (value_compare v1 Coq_value_null)
  then run_error s2 c Coq_native_error_type
  else let%string (s3,x) = to_string s2 c v2 in
    res_ter s3 (res_ref (ref_create_value v1 x c.execution_ctx_strict))

(** val run_expr_assign :
    state -> execution_ctx -> binary_op option -> expr -> expr
    -> result **)

and run_expr_assign s c opo e1 e2 =
  let%success (s1, rv1)= (run_expr s c e1) in
  let follow = (fun s0 rv_2 ->
    match rv_2 with
    | Coq_resvalue_value v ->
      let%void s_2= (ref_put_value s0 c rv1 v) in
      res_out s_2 (res_val v)
    | Coq_resvalue_empty ->
      Debug.impossible_with_heap_because __LOC__ s0 "Non-value result in [run_expr_assign]."; Coq_result_impossible
    | Coq_resvalue_ref r ->
      Debug.impossible_with_heap_because __LOC__ s "Non-value result in [run_expr_assign]."; Coq_result_impossible
    ) in
  match opo with
  | Some op ->
    let%spec (s2, v1) = (ref_get_value s1 c rv1) in
    let%spec (s3, v2) = (run_expr_get_value s2 c e2) in
    let%success (s4, v) = (run_binary_op s3 c op v1 v2) in
    follow s4 v
  | None ->
    let%spec (x, x0 )= (run_expr_get_value s1 c e2) in
    follow x (Coq_resvalue_value x0)

(** val run_expr_function :
    state -> execution_ctx -> prop_name option -> string list
    -> funcbody -> result **)

and run_expr_function s c fo args bd =
  match fo with
  | Some fn ->
    let p = (lexical_env_alloc_decl s c.execution_ctx_lexical_env) in
    let (lex_2, s_2) = p in
    let follow = fun l ->
      let%some e = (env_record_binds_option s_2 l) in
      let%void s1 = (env_record_create_immutable_binding s_2 l fn) in
      let%object (s2, l0) = (creating_function_object s1 c args bd lex_2 (funcbody_is_strict bd)) in
      let%void s3 = (env_record_initialize_immutable_binding s2 l fn (Coq_value_object l0)) in
      res_out s3 (res_val (Coq_value_object l0))
    in destr_list lex_2
      (fun x -> Debug.impossible_with_heap_because __LOC__ s "Empty lexical environnment allocated in [run_expr_function]."; Coq_result_impossible)
      (fun l x -> follow l) ()
  | None ->
    let lex = c.execution_ctx_lexical_env in
    creating_function_object s c args bd lex (funcbody_is_strict bd)

(** val entering_eval_code :
    state -> execution_ctx -> bool -> funcbody -> (state ->
    execution_ctx -> result) -> result **)

and entering_eval_code s c direct bd k =
  let str = ((funcbody_is_strict bd) || (direct && c.execution_ctx_strict)) in
       let c_2 = (if direct then c else execution_ctx_initial str) in
           let p = (if str
              then lexical_env_alloc_decl s c_2.execution_ctx_lexical_env
              else (c_2.execution_ctx_lexical_env, s)) in
                 let (lex, s_2) = p in
                 let c1 = (if str then execution_ctx_with_lex_same c_2 lex else c_2) in
                      let p0 = (funcbody_prog bd) in
                          let%void
                             s1 = (execution_ctx_binding_inst s_2 c1 Coq_codetype_eval None
                               p0 []) in  k s1 c1

(** val run_eval :
    state -> execution_ctx -> bool -> value list -> result **)

and run_eval s c is_direct_call vs =
  match get_arg 0 vs with
  | Coq_value_undef ->
    res_out s (res_val Coq_value_undef)
  | Coq_value_null ->
    res_out s (res_val Coq_value_null)
  | Coq_value_bool b ->
    res_out s (res_val (Coq_value_bool b))
  | Coq_value_number n ->
    res_out s (res_val (Coq_value_number n))
  | Coq_value_string s0 ->
    let str = (is_direct_call && c.execution_ctx_strict) in
    (match parse_pickable s0 str with
    | Some p0 ->
       entering_eval_code s c is_direct_call (Coq_funcbody_intro (p0, s0))
         (fun s1 c_2 ->
           let%ter (s2, r) = (run_prog s1 c_2 p0) in
           match r.res_type with
           | Coq_restype_normal ->
             ifx_empty_label s2 r (fun x ->
               match r.res_value with
               | Coq_resvalue_empty -> res_ter s2 (res_val Coq_value_undef)
               | Coq_resvalue_value v -> res_ter s2 (res_val v)
               | Coq_resvalue_ref r0 -> (Debug.impossible_with_heap_because __LOC__ s2
                   "Reference found in the result of an `eval\' in [run_eval]."; Coq_result_impossible))
           | Coq_restype_throw -> res_ter s2 (res_throw r.res_value)
           | _ -> (Debug.impossible_with_heap_because __LOC__ s2
               "Forbidden result type returned by an `eval\' in [run_eval]."; Coq_result_impossible))
    | None -> run_error s c Coq_native_error_syntax)
  | Coq_value_object o ->
    res_out s (res_val (Coq_value_object o))

(** val run_expr_call :
    state -> execution_ctx -> expr -> expr list -> result **)

and run_expr_call s c e1 e2s =
  let is_eval_direct = (is_syntactic_eval e1) in
  let%success (s1, rv)= (run_expr s c e1) in
  let%spec (s2, f) = (ref_get_value s1 c rv) in
  let%spec (s3, vs) = (run_list_expr s2 c [] e2s) in
  match f with
  | Coq_value_object l ->
    if is_callable_dec s3 (Coq_value_object l)
    then let  follow = (fun vthis ->
        if object_loc_compare l (Coq_object_loc_prealloc
                                      Coq_prealloc_global_eval)
        then run_eval s3 c is_eval_direct vs
        else run_call s3 c l vthis vs) in
        match rv with
        | Coq_resvalue_empty ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s3
            ("[run_expr_call] unable to call an  empty result.")
        | Coq_resvalue_value v ->
          follow Coq_value_undef
        | Coq_resvalue_ref r ->
          (match r.ref_base with
           | Coq_ref_base_type_value v ->
             if   (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_primitive_base)
               || (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_null)
               || (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_object)
             then follow v
             else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                 s3
                 ("[run_expr_call] unable to call a non-property function.")
           | Coq_ref_base_type_env_loc l0 ->
             let%some v = (env_record_implicit_this_value s3 l0) in  follow v)
    else run_error s3 c Coq_native_error_type
  | _ -> run_error s3 c Coq_native_error_type

(** val run_expr_conditionnal :
    state -> execution_ctx -> expr -> expr -> expr -> result **)

and run_expr_conditionnal s c e1 e2 e3 =
  let%spec (s1, v1) = (run_expr_get_value s c e1) in
      let  b = (to_boolean v1) in
          let  e = (if b then e2 else e3) in
              let%spec (s0, r) = (run_expr_get_value s1 c e) in
                  res_ter s0 (res_val r)

(** val run_expr_new :
    state -> execution_ctx -> expr -> expr list -> result **)

and run_expr_new s c e1 e2s =
  let%spec (s1, v) = (run_expr_get_value s c e1) in
  let%spec (s2, args) = (run_list_expr s1 c [] e2s) in
  match v with
  | Coq_value_object l ->
    let%some coo = (run_object_method object_construct_ s2 l) in
    (match coo with
    | Some co -> run_construct s2 c co l args
    | None -> run_error s2 c Coq_native_error_type)
  | _ -> run_error s2 c Coq_native_error_type

(** val run_stat_label :
    state -> execution_ctx -> label -> stat -> result **)

and run_stat_label s c lab t =
  let%break (s1, r1) = run_stat s c t in
  res_out s1 (if label_compare r1.res_label lab then res_normal r1.res_value else r1)

(** val run_stat_with :
    state -> execution_ctx -> expr -> stat -> result **)

and run_stat_with s c e1 t2 =
  let%spec (s1, v1) = (run_expr_get_value s c e1) in
      let%object (s2, l) = (to_object s1 v1) in
          let  lex = (c.execution_ctx_lexical_env) in
              let
                p = (lexical_env_alloc_object s2 lex l provide_this_true) in
                   let (lex_2, s3) = p in
                   let  c_2 = (execution_ctx_with_lex c lex_2) in
                       run_stat s3 c_2 t2

(** val run_stat_if :
    state -> execution_ctx -> expr -> stat -> stat option ->
    result **)

and run_stat_if s c e1 t2 to0 =
  let%spec (s1, v1) = (run_expr_get_value s c e1) in
  let b = (to_boolean v1) in
  if b
  then run_stat s1 c t2
  else (match to0 with
    | Some t3 -> run_stat s1 c t3
    | None -> res_out s1 (res_normal Coq_resvalue_empty))

(** val run_stat_while :
    state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

and run_stat_while s c rv labs e1 t2 =
  let%spec (s1, v1) = (run_expr_get_value s c e1) in
      let  b = (to_boolean v1) in
          if b
          then let%ter (s2, r) = (run_stat s1 c t2) in
              let
                 rv_2 = (if not
                    (resvalue_compare r.res_value Coq_resvalue_empty)
                 then r.res_value
                 else rv) in
                    let  loop = (fun x ->
                        run_stat_while s2 c rv_2 labs e1 t2) in
                        if  (not (restype_compare r.res_type Coq_restype_continue))
                         || (not (res_label_in r labs))
                        then if
                             (restype_compare r.res_type Coq_restype_break)
                           && (res_label_in r labs)
                          then res_ter s2 (res_normal rv_2)
                          else if not
                              (restype_compare r.res_type
                                 Coq_restype_normal)
                          then res_ter s2 r
                          else loop ()
                        else loop ()
          else res_ter s1 (res_normal rv)

(** val run_stat_switch_end :
    state -> execution_ctx -> resvalue -> switchclause list ->
    result **)

and run_stat_switch_end s c rv _foo_ = match _foo_ with
  | [] -> res_out s (res_normal rv)
  | y :: scs_2 ->
    match y with Coq_switchclause_intro (e, ts) ->
      ifx_success_state rv (run_block s c (rev ts)) (fun s1 rv1 ->
          run_stat_switch_end s1 c rv1 scs_2)

(** val run_stat_switch_no_default :
    state -> execution_ctx -> value -> resvalue -> switchclause
    list -> result **)

and run_stat_switch_no_default s c vi rv _foo_ = match _foo_ with
  | [] -> res_out s (res_normal rv)
  | y :: scs_2 ->
    match y with Coq_switchclause_intro (e, ts) ->
      let%spec (s1, v1) = (run_expr_get_value s c e) in
          let  b = (strict_equality_test v1 vi) in
              if b
              then let%success (s2, rv2)= (run_block s1 c (rev ts)) in
                  run_stat_switch_end s2 c rv2 scs_2
              else run_stat_switch_no_default s1 c vi rv scs_2

(** val run_stat_switch_with_default_default :
    state -> execution_ctx -> stat list -> switchclause list ->
    result **)

and run_stat_switch_with_default_default s c ts scs =
  let%success (s1, rv)= (run_block s c (rev ts)) in
      run_stat_switch_end s1 c rv scs

(** val run_stat_switch_with_default_B :
    state -> execution_ctx -> value -> resvalue -> stat list ->
    switchclause list -> result **)

and run_stat_switch_with_default_B s c vi rv ts0 scs = match scs with
  | [] -> run_stat_switch_with_default_default s c ts0 scs
  | y :: scs_2 ->
    match y with Coq_switchclause_intro (e, ts) ->
      let%spec (s1, v1) = (run_expr_get_value s c e) in
          let  b = (strict_equality_test v1 vi) in
              if b
              then let%success (s2, rv2)= (run_block s1 c (rev ts)) in
                  run_stat_switch_end s2 c rv2 scs_2
              else run_stat_switch_with_default_B s1 c vi rv ts0 scs_2

(** val run_stat_switch_with_default_A :
    state -> execution_ctx -> bool -> value -> resvalue ->
    switchclause list -> stat list -> switchclause list -> result **)

and run_stat_switch_with_default_A s c found vi rv scs1 ts0 scs2 =
  match scs1 with
  | [] ->
    if found
    then run_stat_switch_with_default_default s c ts0 scs2
    else run_stat_switch_with_default_B s c vi rv ts0 scs2
  | y :: scs_2 ->
    match y with Coq_switchclause_intro (e, ts) ->
      let
        follow = (fun s0 ->
          ifx_success_state rv (run_block s0 c (rev ts)) (fun s1 rv0 ->
              run_stat_switch_with_default_A s1 c true vi rv0 scs_2 ts0 scs2)) in
           if found
           then follow s
           else let%spec (s1, v1) = (run_expr_get_value s c e) in
               let  b = (strict_equality_test v1 vi) in
                   if b
                   then follow s1
                   else run_stat_switch_with_default_A s1 c false vi rv
                       scs_2 ts0 scs2

(** val run_stat_switch :
    state -> execution_ctx -> label_set -> expr -> switchbody ->
    result **)

(* ALTERNATIVE VERSION, WITH LESS FACTORIZATION
and run_stat_switch s c labs e sb =
  let%spec (s1, vi) = run_expr_get_value s c e in
  match sb with
  | Coq_switchbody_nodefault scs ->
    let%success (s0, r) = begin
      let%break (s2, r) =
        run_stat_switch_no_default s1 c vi
        Coq_resvalue_empty scs in
      if res_label_in r labs
      then res_out s2 (res_normal r.res_value)
      else res_out s2 r
      end in
    res_ter s0 (res_normal r)
  | Coq_switchbody_withdefault (scs1, ts, scs2) ->
    let%success (s0, r) = begin
      let%break (s2, r) =
        run_stat_switch_with_default_A s1 c false vi
         Coq_resvalue_empty scs1 ts scs2 in
      if res_label_in r labs
      then res_out s2 (res_normal r.res_value)
      else res_out s2 r end in
    res_ter s0 (res_normal r)
*)

and run_stat_switch s c labs e sb =
  let%spec (s1, vi) = run_expr_get_value s c e in
  let  follow = (fun w ->
    let%success (s0, r) =
      let%break (s2, r) = w in
      if res_label_in r labs
      then res_out s2 (res_normal r.res_value)
      else res_out s2 r in
    res_ter s0 (res_normal r)) in
  match sb with
  | Coq_switchbody_nodefault scs ->
    follow (run_stat_switch_no_default s1 c vi
              Coq_resvalue_empty scs)
  | Coq_switchbody_withdefault (scs1, ts, scs2) ->
    follow (run_stat_switch_with_default_A s1 c false vi
              Coq_resvalue_empty scs1 ts scs2)

(** val run_stat_do_while :
    state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

and run_stat_do_while s c rv labs e1 t2 =
  let%ter (s1, r) = (run_stat s c t2) in
      let
         rv_2 = (if resvalue_compare r.res_value Coq_resvalue_empty
         then rv
         else r.res_value) in
            let  loop = (fun x ->
                let%spec (s2, v1) = (run_expr_get_value s1 c e1) in
                    let  b = (to_boolean v1) in
                        if b
                        then run_stat_do_while s2 c rv_2 labs e1 t2
                        else res_ter s2 (res_normal rv_2)) in
                if  (restype_compare r.res_type Coq_restype_continue)
                   && (res_label_in r labs)
                then loop ()
                else if
                    (restype_compare r.res_type Coq_restype_break)
                 && (res_label_in r labs)
                then res_ter s1 (res_normal rv_2)
                else if not
                    (restype_compare r.res_type Coq_restype_normal)
                then res_ter s1 r
                else loop ()

(** val run_stat_try :
    state -> execution_ctx -> stat -> (prop_name * stat) option
    -> stat option -> result **)

and run_stat_try s c t1 t2o t3o =
  let  finallycont = (fun s1 r ->
      match t3o with
      | Some t3 ->
        let%success (s2, rv_2) = (run_stat s1 c t3) in  res_ter s2 r
      | None -> res_ter s1 r) in
      ifx_any_or_throw (run_stat s c t1) finallycont (fun s1 v ->
          match t2o with
          | Some y ->
            let (x, t2) = y in
            let  lex = (c.execution_ctx_lexical_env) in
                let  p = (lexical_env_alloc_decl s1 lex) in
                    let (lex_2, s_2) = p in
                    (match lex_2 with
                     | [] ->
                       (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                         s_2
                         ("Empty lexical environnment in [run_stat_try].")
                     | l :: oldlex ->
                       let%void
                          s2= (env_record_create_set_mutable_binding s_2 c l x None v
                            throw_irrelevant) in
                             let c_2 = execution_ctx_with_lex c lex_2 in
                             let%ter (s3, r) = (run_stat s2 c_2 t2) in finallycont s3 r)
          | None -> finallycont s1 (res_throw (Coq_resvalue_value v)))

(** val run_stat_throw :
    state -> execution_ctx -> expr -> result **)

and run_stat_throw s c e =
  let%spec (s1, v1) = (run_expr_get_value s c e) in
      res_ter s1 (res_throw (Coq_resvalue_value v1))

(** val run_stat_return :
    state -> execution_ctx -> expr option -> result **)

and run_stat_return s c _foo_ = match _foo_ with
  | Some e ->
      let%spec (s1, v1) = (run_expr_get_value s c e) in
      res_ter s1 (res_return (Coq_resvalue_value v1))
  | None ->
      res_out s (res_return (Coq_resvalue_value Coq_value_undef))

(** val run_stat_for_loop :
    state -> execution_ctx -> label_set -> resvalue -> expr
    option -> expr option -> stat -> result **)

and run_stat_for_loop s c labs rv eo2 eo3 t =
  let  follows = (fun s0 ->
      let%ter (s1, r) = (run_stat s0 c t) in
          let
             rv_2 = (if not
                (resvalue_compare r.res_value Coq_resvalue_empty)
             then r.res_value
             else rv) in
                let  loop = (fun s2 ->
                    run_stat_for_loop s2 c labs rv_2 eo2 eo3 t) in
                    if   (restype_compare r.res_type Coq_restype_break)
                      && (res_label_in r labs)
                    then res_ter s1 (res_normal rv_2)
                    else if
                         (restype_compare r.res_type Coq_restype_normal)
                      || (    (restype_compare r.res_type Coq_restype_continue)
                           && (res_label_in r labs))
                    then (match eo3 with
                        | Some e3 ->
                          let%spec
                            (s2, v3) = (run_expr_get_value s1 c e3) in loop s2
                        | None -> loop s1)
                    else res_ter s1 r) in
      match eo2 with
      | Some e2 ->
        let%spec (s0, v2) = (run_expr_get_value s c e2) in
            let  b = (to_boolean v2) in
                if b then follows s0 else res_ter s0 (res_normal rv)
      | None -> follows s

(** val run_stat_for :
    state -> execution_ctx -> label_set -> expr option -> expr
    option -> expr option -> stat -> result **)

and run_stat_for s c labs eo1 eo2 eo3 t =
  let follows = fun s0 ->
    run_stat_for_loop s0 c labs Coq_resvalue_empty eo2 eo3 t
  in
  (match eo1 with
   | Some e1 ->
     let%spec (s0, v1) = (run_expr_get_value s c e1) in follows s0
   | None -> follows s)

(** val run_stat_for_var :
    state -> execution_ctx -> label_set -> (string * expr
    option) list -> expr option -> expr option -> stat -> result **)

and run_stat_for_var s c labs ds eo2 eo3 t =
  let%ter (s0, r) = (run_stat s c (Coq_stat_var_decl ds)) in
      run_stat_for_loop s0 c labs Coq_resvalue_empty eo2 eo3 t

(** val run_expr : state -> execution_ctx -> expr -> result **)

and run_expr s c _term_ = match _term_ with
  | Coq_expr_this ->
    res_out s (res_val c.execution_ctx_this_binding)
  | Coq_expr_identifier x ->
    let%spec (s0, r) = (identifier_resolution s c x) in
    res_ter s0 (res_ref r)
  | Coq_expr_literal i ->
    res_out s (res_val (convert_literal_to_prim i))
  | Coq_expr_object pds ->
    let%object (s1, l) = run_construct_prealloc s c Coq_prealloc_object [] in
    init_object s1 c l pds
  | Coq_expr_array oes ->
    let%object (s1, l) = run_construct_prealloc s c Coq_prealloc_array [] in
    init_array s1 c l oes
  | Coq_expr_function (fo, args, bd) -> run_expr_function s c fo args bd
  | Coq_expr_access (e1, e2) -> run_expr_access s c e1 e2
  | Coq_expr_member (e1, f) ->
    run_expr s c (Coq_expr_access (e1, (Coq_expr_literal
                                          (Coq_literal_string f))))
  | Coq_expr_new (e1, e2s) -> run_expr_new s c e1 e2s
  | Coq_expr_call (e1, e2s) -> run_expr_call s c e1 e2s
  | Coq_expr_unary_op (op, e0) -> run_unary_op s c op e0
  | Coq_expr_binary_op (e1, op, e2) -> run_expr_binary_op s c op e1 e2
  | Coq_expr_conditional (e1, e2, e3) ->
    run_expr_conditionnal s c e1 e2 e3
  | Coq_expr_assign (e1, opo, e2) -> run_expr_assign s c opo e1 e2

(** val run_stat : state -> execution_ctx -> stat -> result **)

and run_stat s c _term_ = match _term_ with
  | Coq_stat_expr e ->
    let%spec (s0, r) = (run_expr_get_value s c e) in
        res_ter s0 (res_val r)
  | Coq_stat_label (lab, t0) ->
    run_stat_label s c (Coq_label_string lab) t0
  | Coq_stat_block ts -> run_block s c (rev ts)
  | Coq_stat_var_decl xeos -> run_var_decl s c xeos
  | Coq_stat_if (e1, t2, to0) -> run_stat_if s c e1 t2 to0
  | Coq_stat_do_while (ls, t1, e2) ->
    run_stat_do_while s c Coq_resvalue_empty ls e2 t1
  | Coq_stat_while (ls, e1, t2) ->
    run_stat_while s c Coq_resvalue_empty ls e1 t2
  | Coq_stat_with (e1, t2) -> run_stat_with s c e1 t2
  | Coq_stat_throw e -> run_stat_throw s c e
  | Coq_stat_return eo -> run_stat_return s c eo
  | Coq_stat_break so -> res_out s (res_break so)
  | Coq_stat_continue so -> res_out s (res_continue so)
  | Coq_stat_try (t1, t2o, t3o) -> run_stat_try s c t1 t2o t3o
  | Coq_stat_for (ls, eo1, eo2, eo3, s0) ->
    run_stat_for s c ls eo1 eo2 eo3 s0
  | Coq_stat_for_var (ls, ds, eo2, eo3, s0) ->
    run_stat_for_var s c ls ds eo2 eo3 s0
  | Coq_stat_for_in (ls, e1, e2, s0) ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_not_yet_implemented)
      ("stat_for_in")
  | Coq_stat_for_in_var (ls, x, e1o, e2, s0) ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_not_yet_implemented)
      ("stat_for_in_var")
  | Coq_stat_debugger -> res_out s res_empty
  | Coq_stat_switch (labs, e, sb) -> run_stat_switch s c labs e sb

(** val run_elements :
    state -> execution_ctx -> elements -> result **)

and run_elements s c _foo_ = match _foo_ with
  | [] -> res_out s (res_normal Coq_resvalue_empty)
  | el :: els_rev_2 ->
    let%success (s0, rv0)= (run_elements s c els_rev_2) in
        match el with
        | Coq_element_stat t ->
          let%ter (s1, r1) = (run_stat s0 c t) in
              let r2 = res_overwrite_value_if_empty rv0 r1 in
              res_out s1 r2
        | Coq_element_func_decl (name, args, bd) -> res_ter s0 (res_normal rv0)

(** val run_prog : state -> execution_ctx -> prog -> result **)

and run_prog s c _term_ = match _term_ with
  | Coq_prog_intro (str, els) -> run_elements s c (rev els)

(** val push :
    state -> execution_ctx -> object_loc -> value list -> float
    -> result **)

and push s c l args ilen =
  let vlen = ilen in
      match args with
      | [] ->
        let%not_throw
           (s0, x) = (object_put s c l ("length")
             (Coq_value_number vlen) throw_true) in
              res_out s0 (res_val (Coq_value_number vlen))
      | v :: vs ->
        let%string
          (s0, slen) = (to_string s c (Coq_value_number vlen)) in
             let%not_throw  (s1, x) = (object_put s0 c l slen v throw_true) in
                 push s1 c l vs (ilen +. 1.)

(** val run_object_is_sealed :
    state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

and run_object_is_sealed s c l _foo_ = match _foo_ with
  | [] ->
    let%some ext = (run_object_method object_extensible_ s l) in
        res_ter s (res_val (Coq_value_bool (not ext)))
  | x :: xs_2 ->
    let%spec (s0, d) = (run_object_get_own_prop s c l x) in
        match d with
        | Coq_full_descriptor_undef ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s0
            ("[run_object_is_sealed]:  Undefined descriptor found in a place where it shouldn\'t.")
        | Coq_full_descriptor_some a ->
          if attributes_configurable a
          then res_ter s0 (res_val (Coq_value_bool false))
          else run_object_is_sealed s0 c l xs_2

(** val run_object_seal :
    state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

and run_object_seal s c l _foo_ = match _foo_ with
  | [] ->
    let%some s0= (run_object_heap_set_extensible false s l) in
        res_ter s0 (res_val (Coq_value_object l))
  | x :: xs_2 ->
    let%spec (s0, d) = (run_object_get_own_prop s c l x) in
        match d with
        | Coq_full_descriptor_undef ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s0
            ("[run_object_seal]:  Undefined descriptor found in a place where it shouldn\'t.")
        | Coq_full_descriptor_some a ->
          let a_2 =
            if attributes_configurable a
            then let desc = { descriptor_value = None; descriptor_writable =
                                                         None; descriptor_get = None; descriptor_set = None;
                              descriptor_enumerable = None; descriptor_configurable = (Some
                                                                                         false) }
              in
              attributes_update a desc
            else a
          in
          let%bool
             (s1, x0) = (object_define_own_prop s0 c l x (descriptor_of_attributes a_2)
               true) in run_object_seal s1 c l xs_2

(** val run_object_freeze :
    state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

and run_object_freeze s c l _foo_ = match _foo_ with
  | [] ->
    let%some s0= (run_object_heap_set_extensible false s l) in
        res_ter s0 (res_val (Coq_value_object l))
  | x :: xs_2 ->
    let%spec (s0, d) = (run_object_get_own_prop s c l x) in
        match d with
        | Coq_full_descriptor_undef ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s0
            ("[run_object_freeze]:  Undefined descriptor found in a place where it shouldn\'t.")
        | Coq_full_descriptor_some a ->
          let a_2 =
            if (attributes_is_data_dec a) && (attributes_writable a)
            then let desc = { descriptor_value = None; descriptor_writable =
                                                         (Some false); descriptor_get = None; descriptor_set = None;
                              descriptor_enumerable = None; descriptor_configurable = None }
              in
              attributes_update a desc
            else a
          in
          let a_3 =
            if attributes_configurable a_2
            then let desc = { descriptor_value = None; descriptor_writable =
                                                         None; descriptor_get = None; descriptor_set = None;
                              descriptor_enumerable = None; descriptor_configurable = (Some
                                                                                         false) }
              in
              attributes_update a_2 desc
            else a_2
          in
          let%bool
             (s1, x0) = (object_define_own_prop s0 c l x (descriptor_of_attributes a_3)
               true) in run_object_freeze s1 c l xs_2

(** val run_object_is_frozen :
    state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

and run_object_is_frozen s c l _foo_ = match _foo_ with
  | [] ->
    let%some ext = (run_object_method object_extensible_ s l) in
        res_ter s (res_val (Coq_value_bool (not ext)))
  | x :: xs_2 ->
    let%spec (s0, d) = (run_object_get_own_prop s c l x) in
        let  check_configurable = (fun a ->
            if attributes_configurable a
            then res_ter s0 (res_val (Coq_value_bool false))
            else run_object_is_frozen s0 c l xs_2) in
            match d with
            | Coq_full_descriptor_undef ->
              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s0
                ("[run_object_is_frozen]:  Undefined descriptor found in a place where it shouldn\'t.")
            | Coq_full_descriptor_some a ->
              (match a with
               | Coq_attributes_data_of ad ->
                 if attributes_writable (Coq_attributes_data_of ad)
                 then res_ter s0 (res_val (Coq_value_bool false))
                 else check_configurable (Coq_attributes_data_of ad)
               | Coq_attributes_accessor_of aa ->
                 check_configurable (Coq_attributes_accessor_of aa))

(** val run_get_args_for_apply :
    state -> execution_ctx -> object_loc -> float -> float ->
    value list specres **)

and run_get_args_for_apply s c l index n =
  if  index < n
  then let%string
       (s0, sindex) = (to_string s c (Coq_value_number index)) in
          let%value (s1, v) = (run_object_get s0 c l sindex) in
              let

                tail_args = (run_get_args_for_apply s1 c l (index +. 1.) n) in
                   let%spec (s2, tail) = (tail_args) in res_spec s2 (v :: tail)
  else res_spec s []

(** val valueToStringForJoin :
    state -> execution_ctx -> object_loc -> float -> string
    specres **)

and valueToStringForJoin s c l k =
  let%string (s0, prop) = (to_string s c (Coq_value_number k)) in
  let%value (s1, v) = (run_object_get s0 c l prop) in
  match v with
  | Coq_value_undef -> res_spec s1 ""
  | Coq_value_null -> res_spec s1 ""
  | Coq_value_bool b ->
    let%string (s2, s3) = (to_string s1 c v) in res_spec s2 s3
  | Coq_value_number n ->
    let%string (s2, s3) = (to_string s1 c v) in res_spec s2 s3
  | Coq_value_string s2 ->
    let%string (s3, s4) = (to_string s1 c v) in res_spec s3 s4
  | Coq_value_object o ->
    let%string (s2, s3) = (to_string s1 c v) in res_spec s2 s3

(** val run_array_join_elements :
    state -> execution_ctx -> object_loc -> float -> float ->
    string -> string -> result **)

and run_array_join_elements s c l k length0 sep sR =
  if  k < length0
  then let  ss = (strappend sR sep) in
      let  sE = (valueToStringForJoin s c l k) in
          let%spec (s0, element) = (sE) in
              let  sR0 = (strappend ss element) in
                  run_array_join_elements s0 c l (k +. 1.)
                    length0 sep sR0
  else res_ter s (res_val (Coq_value_string sR))

(** Definitions of prealloc (built-in) functions *)
(** val run_call_prealloc :
    state -> execution_ctx -> prealloc -> value -> value list ->
    result **)

and run_call_prealloc s c b vthis args =
  match b with
  | Coq_prealloc_global_is_finite ->
    let  v = (get_arg 0 args) in
    let%number (s0, n) = (to_number s c v) in
    res_ter s0 (res_val (Coq_value_bool (not ((JsNumber.isnan n) || (n === JsNumber.infinity) || (n === JsNumber.neg_infinity)))))
  | Coq_prealloc_global_is_nan ->
    let  v = (get_arg 0 args) in
        let%number (s0, n) = (to_number s c v) in
            res_ter s0
              (res_val (Coq_value_bool (JsNumber.isnan n)))
  | Coq_prealloc_object ->
    let  value0 = (get_arg 0 args) in begin
        match value0 with
        | Coq_value_undef -> run_construct_prealloc s c b args
        | Coq_value_null -> run_construct_prealloc s c b args
        | Coq_value_bool b0 -> to_object s value0
        | Coq_value_number n -> to_object s value0
        | Coq_value_string s0 -> to_object s value0
        | Coq_value_object o -> to_object s value0
    end
  | Coq_prealloc_object_get_proto_of ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_object l ->
          let%some proto = (run_object_method object_proto_ s l) in
          res_ter s (res_val proto)
        | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_get_own_prop_descriptor ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%string (s1, x) = (to_string s c (get_arg 1 args)) in
        let%spec (s2, d) = (run_object_get_own_prop s1 c l x) in
        from_prop_descriptor s2 c d
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_define_prop ->
    let o = (get_arg 0 args) in
    let p = (get_arg 1 args) in
    let attr = (get_arg 2 args) in begin
      match o with
      | Coq_value_object l ->
        let%string (s1, name) = (to_string s c p) in
        let%spec (s2, desc) = (to_property_descriptor s1 attr) in
        let%bool (s3, x) = (object_define_own_prop s2 c l name desc true) in
        res_ter s3 (res_val (Coq_value_object l))
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_seal ->
    let v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%some _x_ = (object_properties_keys_as_list_option s l) in
        run_object_seal s c l _x_
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_freeze ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%some _x_ = (object_properties_keys_as_list_option s l) in
        run_object_freeze s c l _x_
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_prevent_extensions ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%some o = (object_binds_option s l) in
        let o1 = object_with_extension o false in
        let s_2 = object_write s l o1 in
        res_ter s_2 (res_val (Coq_value_object l))
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_is_sealed ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%some _x_ = (object_properties_keys_as_list_option s l) in  run_object_is_sealed s c l _x_
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_is_frozen ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%some _x_ = (object_properties_keys_as_list_option s l) in  run_object_is_frozen s c l _x_
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_is_extensible ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%some r = (run_object_method object_extensible_ s l) in
        res_ter s (res_val (Coq_value_bool r))
      | _ -> run_error s c Coq_native_error_type
    end
  | Coq_prealloc_object_proto_to_string ->
    (match vthis with
     | Coq_value_undef ->
       res_out s (res_val (Coq_value_string ("[object Undefined]")))
     | Coq_value_null ->
       res_out s (res_val (Coq_value_string ("[object Null]")))
     | Coq_value_bool b0 ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s0= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Coq_value_string (strappend "[object " (strappend s0 "]"))))
     | Coq_value_number n ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s0= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Coq_value_string (strappend "[object " (strappend s0 "]"))))
     | Coq_value_string s0 ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s2= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Coq_value_string (strappend "[object " (strappend s2 "]"))))
     | Coq_value_object o ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s0= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Coq_value_string (strappend "[object " (strappend s0 "]")))))
  | Coq_prealloc_object_proto_value_of -> to_object s vthis
  | Coq_prealloc_object_proto_has_own_prop ->
    let  v = (get_arg 0 args) in
        let%string (s1, x) = (to_string s c v) in
            let%object (s2, l) = (to_object s1 vthis) in
                let%spec (s3, d) = (run_object_get_own_prop s2 c l x) in begin
                    match d with
                    | Coq_full_descriptor_undef ->
                      res_ter s3 (res_val (Coq_value_bool false))
                    | Coq_full_descriptor_some a ->
                      res_ter s3 (res_val (Coq_value_bool true))
    end
  | Coq_prealloc_object_proto_is_prototype_of ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Coq_value_object l ->
        let%object (s1, lo) = (to_object s vthis) in
        object_proto_is_prototype_of s1 lo l
      | _ ->
        res_out s (res_val (Coq_value_bool false))
    end
  | Coq_prealloc_object_proto_prop_is_enumerable ->
    let  v = (get_arg 0 args) in
    let%string (s1, x) = (to_string s c v) in
    let%object (s2, l) = (to_object s1 vthis) in
    let%spec (s3, d) = (run_object_get_own_prop s2 c l x) in begin
      match d with
      | Coq_full_descriptor_undef ->
        res_ter s3 (res_val (Coq_value_bool false))
      | Coq_full_descriptor_some a ->
        res_ter s3 (res_val (Coq_value_bool (attributes_enumerable a)))
    end
  | Coq_prealloc_function_proto ->
    res_out s (res_val Coq_value_undef)
  | Coq_prealloc_function_proto_to_string ->
    if is_callable_dec s vthis
    then (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_not_yet_implemented)
        ("Function.prototype.toString() is implementation dependent.")
    else run_error s c Coq_native_error_type
  | Coq_prealloc_function_proto_apply ->
    let  thisArg = (get_arg 0 args) in
    let  argArray = (get_arg 1 args) in
    if is_callable_dec s vthis
    then
      (match vthis with
      | Coq_value_object thisobj ->
        (match argArray with
        | Coq_value_undef -> run_call s c thisobj thisArg []
        | Coq_value_null -> run_call s c thisobj thisArg []
        | Coq_value_bool b0 -> run_error s c Coq_native_error_type
        | Coq_value_number n -> run_error s c Coq_native_error_type
        | Coq_value_string s0 -> run_error s c Coq_native_error_type
        | Coq_value_object array ->
          let%value (s0, v) = (run_object_get s c array ("length")) in
          let%spec (s1, ilen) = (to_uint32 s0 c v) in
          let%spec (s2, arguments_) = (run_get_args_for_apply s1 c array 0. ilen) in
          run_call s2 c thisobj thisArg arguments_)
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("Value is callable, but isn\'t an object."))
            else run_error s c Coq_native_error_type
  | Coq_prealloc_function_proto_call ->
    if is_callable_dec s vthis
    then
      (match vthis with
      | Coq_value_object thisobj ->
        let (thisArg, a) = get_arg_first_and_rest args in
        run_call s c thisobj thisArg a
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("Value is callable, but isn\'t an object.")
      )
    else run_error s c Coq_native_error_type
  | Coq_prealloc_function_proto_bind ->
    if is_callable_dec s vthis
    then
      (match vthis with
      | Coq_value_object thisobj ->
        let (vthisArg, a) = get_arg_first_and_rest args in
        let o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_object_proto)) ("Object")) in
        let o2 = (object_with_get o1 Coq_builtin_get_function) in
        let o3 = (object_with_details o2 None None None (Some thisobj) (Some vthisArg) (Some a) None) in
        let o4 = (object_set_class o3 ("Function")) in
        let o5 = (object_set_proto o4 (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_function_proto))) in
        let o6 = (object_with_invokation o5 (Some Coq_construct_after_bind) (Some Coq_call_after_bind) (Some Coq_builtin_has_instance_after_bind)) in
        let o7 = (object_set_extensible o6 true) in
        let (l, s_2) = object_alloc s o7 in
        let vlength = (
          let%some class0 = (run_object_method object_class_ s_2 thisobj) in
          if string_eq class0 ("Function")
          then
            let%number (s10, n) = (run_object_get s_2 c thisobj ("length")) in
            let%spec (s11, ilen) = (to_int32 s10 c (Coq_value_number n)) in
            if  ilen < (number_of_int (LibList.length a))
            then res_spec s11 0.
            else res_spec s11 (ilen -.  (number_of_int (LibList.length a)))
          else res_spec s_2 0.) in
        let%spec (s10, length0) = (vlength) in
        let a0 = ({ attributes_data_value = (Coq_value_number  length0);
                    attributes_data_writable = false;
                    attributes_data_enumerable = false;
                    attributes_data_configurable = false }) in
        let%some s11 = (run_object_heap_map_properties s10 l
          (fun p -> HeapStr.write p ("length") (Coq_attributes_data_of a0))) in
        let vthrower = (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_throw_type_error)) in
        let a1 = ({ attributes_accessor_get = vthrower;
                    attributes_accessor_set = vthrower;
                    attributes_accessor_enumerable = false;
                    attributes_accessor_configurable = false }) in
        let%bool (s12, x) = (object_define_own_prop s11 c l ("caller") (descriptor_of_attributes (Coq_attributes_accessor_of a1)) false) in
        let%bool (s13, x0) = (object_define_own_prop s12 c l ("arguments") (descriptor_of_attributes (Coq_attributes_accessor_of a1)) false) in
        res_ter s13 (res_val (Coq_value_object l))
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("Value is callable, but isn\'t an object.")
      )
    else run_error s c Coq_native_error_type
  | Coq_prealloc_bool ->
      let v = get_arg 0 args in
      res_out s (res_val (Coq_value_bool (to_boolean v)))
  | Coq_prealloc_bool_proto_to_string ->
    (match vthis with
     | Coq_value_undef -> run_error s c Coq_native_error_type
     | Coq_value_null -> run_error s c Coq_native_error_type
     | Coq_value_bool b0 -> res_ter s (res_val (Coq_value_string (convert_bool_to_string b0)))
     | Coq_value_number n -> run_error s c Coq_native_error_type
     | Coq_value_string s0 -> run_error s c Coq_native_error_type
     | Coq_value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s c Coq_native_error_type) (fun s0 ->
           if string_eq s0 ("Boolean")
           then ifx_some_or_default (run_object_method object_prim_value_ s l)
             (run_error s c Coq_native_error_type) (fun wo ->
               match wo with
               | Some v ->
                 (match v with
                 | Coq_value_undef -> run_error s c Coq_native_error_type
                 | Coq_value_null -> run_error s c Coq_native_error_type
                 | Coq_value_bool b0 ->
                   res_ter s
                     (res_val (Coq_value_string (convert_bool_to_string b0)))
                 | Coq_value_number n ->
                   run_error s c Coq_native_error_type
                 | Coq_value_string s1 ->
                   run_error s c Coq_native_error_type
                 | Coq_value_object o -> run_error s c Coq_native_error_type)
               | None -> run_error s c Coq_native_error_type)
             else run_error s c Coq_native_error_type))
  | Coq_prealloc_bool_proto_value_of ->
    (match vthis with
     | Coq_value_undef -> run_error s c Coq_native_error_type
     | Coq_value_null -> run_error s c Coq_native_error_type
     | Coq_value_bool b0 ->
       res_ter s (res_val (Coq_value_bool b0))
     | Coq_value_number n -> run_error s c Coq_native_error_type
     | Coq_value_string s0 -> run_error s c Coq_native_error_type
     | Coq_value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s c Coq_native_error_type) (fun s0 ->
             if string_eq s0 ("Boolean")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
               (run_error s c Coq_native_error_type) (fun wo ->
                 match wo with
                 | Some v ->
                   (match v with
                    | Coq_value_undef -> run_error s c Coq_native_error_type
                    | Coq_value_null -> run_error s c Coq_native_error_type
                    | Coq_value_bool b0 -> res_ter s (res_val (Coq_value_bool b0))
                    | Coq_value_number n -> run_error s c Coq_native_error_type
                    | Coq_value_string s1 -> run_error s c Coq_native_error_type
                    | Coq_value_object o -> run_error s c Coq_native_error_type)
                 | None -> run_error s c Coq_native_error_type)
             else run_error s c Coq_native_error_type))
  | Coq_prealloc_number ->
    if list_eq_nil_decidable args
    then res_out s (res_val (Coq_value_number JsNumber.zero))
    else let v = get_arg 0 args in to_number s c v
  | Coq_prealloc_number_proto_value_of ->
    (match vthis with
     | Coq_value_undef -> run_error s c Coq_native_error_type
     | Coq_value_null -> run_error s c Coq_native_error_type
     | Coq_value_bool b0 -> run_error s c Coq_native_error_type
     | Coq_value_number n ->
       res_ter s (res_val (Coq_value_number n))
     | Coq_value_string s0 -> run_error s c Coq_native_error_type
     | Coq_value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s c Coq_native_error_type) (fun s0 ->
             if string_eq s0 ("Number")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
                 (run_error s c Coq_native_error_type) (fun wo ->
                     match wo with
                     | Some v ->
                       (match v with
                        | Coq_value_undef -> run_error s c Coq_native_error_type
                        | Coq_value_null -> run_error s c Coq_native_error_type
                        | Coq_value_bool b0 -> run_error s c Coq_native_error_type
                        | Coq_value_number n -> res_ter s (res_val (Coq_value_number n))
                        | Coq_value_string s1 -> run_error s c Coq_native_error_type
                        | Coq_value_object o -> run_error s c Coq_native_error_type)
                     | None -> run_error s c Coq_native_error_type)
             else run_error s c Coq_native_error_type))
  | Coq_prealloc_array ->
    run_construct_prealloc s c Coq_prealloc_array args
  | Coq_prealloc_array_is_array ->
    let  arg = (get_arg 0 args) in begin
        match arg with
        | Coq_value_object arg0 ->
          let%some class0= (run_object_method object_class_ s arg0) in
          if string_eq class0 ("Array")
          then res_ter s (res_val (Coq_value_bool true))
          else res_ter s (res_val (Coq_value_bool false))
        | _ -> res_ter s (res_val (Coq_value_bool false))
    end
  | Coq_prealloc_array_proto_to_string ->
    let%object (s0, array) = (to_object s vthis) in
    let%value (s1, vfunc) = (run_object_get s0 c array ("join")) in
    if is_callable_dec s1 vfunc
    then (match vfunc with
        | Coq_value_object func -> run_call s1 c func (Coq_value_object array) []
        | _ ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s1
            ("Value is callable, but isn\'t an object."))
    else run_call_prealloc s1 c Coq_prealloc_object_proto_to_string (Coq_value_object array) []
  | Coq_prealloc_array_proto_join ->
    let  vsep = (get_arg 0 args) in
    let%object (s0, l) = (to_object s vthis) in
    let%value (s1, vlen) = (run_object_get s0 c l ("length")) in
    let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
    let rsep = (if not (value_compare vsep Coq_value_undef) then vsep else Coq_value_string (",")) in
    let%string (s3, sep) = (to_string s2 c rsep) in
    if ilen = 0.0
    then res_ter s3 (res_val (Coq_value_string ""))
    else
      let sR = (valueToStringForJoin s3 c l 0.) in
      let%spec (s4, sR0) = (sR) in
      run_array_join_elements s4 c l 1. ilen sep sR0
  | Coq_prealloc_array_proto_pop ->
    let%object (s0, l) = (to_object s vthis) in
    let%value (s1, vlen) = (run_object_get s0 c l ("length")) in
    let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
    if ilen = 0.0
    then
      let%not_throw (s3, x) = (object_put s2 c l ("length") (Coq_value_number JsNumber.zero) throw_true) in
      res_out s3 (res_val Coq_value_undef)
    else
      let%string (s3, sindx) = (to_string s2 c (Coq_value_number (ilen -. 1.))) in
      let%value (s4, velem) = (run_object_get s3 c l sindx) in
      let%not_throw (s5, x) = (object_delete s4 c l sindx throw_true) in
      let%not_throw (s6, x0) = (object_put s5 c l ("length") (Coq_value_string sindx) throw_true) in
      res_out s6 (res_val velem)
  | Coq_prealloc_array_proto_push ->
    let%object (s0, l) = (to_object s vthis) in
    let%value (s1, vlen) = (run_object_get s0 c l ("length")) in
    let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
    push s2 c l args ilen
  | Coq_prealloc_string ->
    if list_eq_nil_decidable args
    then res_ter s (res_val (Coq_value_string ""))
    else
      let value0 = (get_arg 0 args) in
      let%string (s0, s1) = (to_string s c value0) in
      res_ter s0 (res_val (Coq_value_string s1))
  | Coq_prealloc_string_proto_to_string ->
    (match vthis with
     | Coq_value_object l ->
       let%some s0= (run_object_method object_class_ s l) in
           if string_eq s0 ("String")
           then run_object_prim_value s l
           else run_error s c Coq_native_error_type
     | _ ->
       if type_compare (type_of vthis) Coq_type_string
       then res_ter s (res_val vthis)
       else run_error s c Coq_native_error_type
    )
  | Coq_prealloc_string_proto_value_of ->
    (match vthis with
     | Coq_value_object l ->
       let%some s0= (run_object_method object_class_ s l) in
           if string_eq s0 ("String")
           then run_object_prim_value s l
           else run_error s c Coq_native_error_type
     | _ ->
       if type_compare (type_of vthis) Coq_type_string
       then res_ter s (res_val vthis)
       else run_error s c Coq_native_error_type
    )
  | Coq_prealloc_error ->
    let  v = (get_arg 0 args) in
        build_error s c (Coq_value_object (Coq_object_loc_prealloc
                                           Coq_prealloc_error_proto)) v
  | Coq_prealloc_native_error ne ->
    let  v = (get_arg 0 args) in
    build_error s c (Coq_value_object (Coq_object_loc_prealloc
                                       (Coq_prealloc_native_error_proto ne))) v
  | Coq_prealloc_throw_type_error -> run_error s c Coq_native_error_type
  | _ ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_not_yet_implemented)
      (strappend ("Call prealloc_") (strappend (string_of_prealloc b) (" not yet implemented")))

(** val run_call :
    state -> execution_ctx -> object_loc -> value -> value list
    -> result **)

and run_call s c l vthis args =
  let%some co = run_object_method object_call_ s l in
  let%some c0 = co in
  match c0 with
  | Coq_call_default -> entering_func_code s c l vthis args
  | Coq_call_after_bind ->
    let%some oarg = run_object_method object_bound_args_ s l in
    let%some boundArgs = oarg in
    let%some obnd = run_object_method object_bound_this_ s l in
    let%some boundThis = obnd in
    let%some otrg = run_object_method object_target_function_ s l in
    let%some target = otrg in
    let arguments_ = (LibList.append boundArgs args) in run_call s c target boundThis arguments_
  | Coq_call_prealloc b -> run_call_prealloc s c b vthis args
  | Coq_call_proxy -> Coq_result_not_yet_implemented (* FIXME: Proxy *)

(** val run_javascript_from_state : state -> prog -> result **)

and run_javascript_from_state s p =
  let c = execution_ctx_initial (prog_intro_strictness p) in
  let%void s_2 = execution_ctx_binding_inst s c Coq_codetype_global None p [] in
  run_prog s_2 c p

(** val run_javascript_from_result : result -> prog -> result **)

and run_javascript_from_result w p =
  if_success w (fun s _ -> run_javascript_from_state s p)

(** val run_javascript : prog -> result **)

and run_javascript p =
  run_javascript_from_state state_initial p
