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


(** val equality_test_for_same_type : coq_type -> value -> value -> bool **)

let equality_test_for_same_type ty v1 v2 =
  match ty with
  | Type_undef -> true
  | Type_null -> true
  | Type_bool -> value_compare v1 v2
  | Type_number ->
    (match v1 with
     | Value_undef -> false
     | Value_null -> false
     | Value_bool b -> false
     | Value_string s -> false
     | Value_number n1 ->
       (match v2 with
        | Value_undef -> false
        | Value_null -> false
        | Value_bool b -> false
        | Value_string s -> false
        | Value_number n2 ->
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
        | Value_object o -> false)
     | Value_object o -> false)
  | Type_string -> value_compare v1 v2
  | Type_object -> value_compare v1 v2

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
  then Value_undef else if n1 === n2
       then Value_bool false
       else if   (JsNumber.isposzero n1)
              && (JsNumber.isnegzero n2)
            then Value_bool false
            else if (JsNumber.isnegzero n1)
                 && (JsNumber.isposzero n2)
                 then Value_bool false
                 else if n1 === JsNumber.infinity
                      then Value_bool false
                      else if n2 === JsNumber.infinity
                           then Value_bool true
                           else if n2 === JsNumber.neg_infinity
                                then Value_bool false
                                else if n1 === JsNumber.neg_infinity
                                     then Value_bool true
                                     else Value_bool (n1 < n2)

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


(** val typeof_prim : prim -> string **)

let typeof_prim _foo_ = match _foo_ with
| Value_undef -> "undefined"
| Value_null -> "object"
| Value_bool b -> "boolean"
| Value_number n -> "number"
| Value_string s -> "string"
| _ -> failwith "typeof_prim: recieved an object"

(** val string_of_propname : propname -> prop_name **)

let string_of_propname _term_ = match _term_ with
| Propname_identifier s -> s
| Propname_string s -> s
| Propname_number n -> JsNumber.to_string n

(*---------------------------------*)


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

let rec build_error s c vproto vmsg =
  let o = object_new vproto ("Error") in
  let (l, s_2) = object_alloc s o in
  if value_compare vmsg Value_undef
  then res_out s_2 (res_val (Value_object l))
  else (
    let%value (s_3, vstr) = to_string s_2 vmsg in
    let a = { attributes_data_value = vstr; attributes_data_writable = true;
      attributes_data_enumerable = false; attributes_data_configurable = true } in
    let%success (s_4, rv) = object_define_own_prop s_3 c l "message" (descriptor_of_attributes (Attributes_data_of a)) throw_true in
    res_out s_4 (res_val (Value_object l))
  )

and run_error s c ne =
  let%object (s_2, l) = (build_error s c (Value_object (Object_loc_prealloc
                                                            (Prealloc_native_error_proto ne))) Value_undef) in
  res_out s_2 (res_throw (Resvalue_value (Value_object l)))

and run_error_no_c : 'a. state -> native_error -> 'a specret resultof =
  fun s ne ->
  let vproto = Value_object (Object_loc_prealloc (Prealloc_native_error_proto ne)) in
  let o = object_new vproto ("error") in
  let (l, s_2) = object_alloc s o in
  res_out s_2 (res_throw (Resvalue_value (Value_object l)))

(* TODO: This is a ES5 HACK for where a context is required, remove when possible *)
and some_context = execution_ctx_initial true

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
and object_has_internal_slot : 'a. state -> object_loc -> (coq_object -> 'a option) -> bool =
  fun s l prj ->
  match run_object_method prj s l with
  | None -> failwith "object_has_internal_slot failed"
  | Some a -> is_some a

(** Function to implement the specification text of "O has a [[X]] internal method" *)
and object_has_internal_method : 'a. state -> object_loc -> (coq_object -> 'a option) -> bool =
  fun s l prj ->
  object_has_internal_slot s l prj

(** {5 Object Internal Method Dispatch Functions }
    Functions in this section are used to dispatch Internal Methods to the
    correct implementation based upon the value of the internal slot

    All these functions accept a parameter o, which is the {i location} of the object.
 *)

(** Function to dispatch calls to O.[[GetPrototypeOf]]() *)
and object_internal_get_prototype_of s o =
  let%some internal_method = run_object_method object_get_prototype_of_ s o in
  match internal_method with
  | Builtin_get_prototype_of_default -> ordinary_object_internal_get_prototype_of s o
  | Builtin_get_prototype_of_proxy   -> proxy_object_internal_get_prototype_of s o

(** Function to dispatch calls to O.[[GetPrototypeOf]](V) *)
and object_internal_set_prototype_of s o v =
  let%some internal_method = run_object_method object_set_prototype_of_ s o in
  match internal_method with
  | Builtin_set_prototype_of_default -> ordinary_object_internal_set_prototype_of s o v
  | Builtin_set_prototype_of_proxy   -> proxy_object_internal_set_prototype_of s o v

(** Function to dispatch calls to O.[[IsExtensible]]() *)
and object_internal_is_extensible s o =
  let%some internal_method = run_object_method object_is_extensible_ s o in
  match internal_method with
  | Builtin_is_extensible_default -> ordinary_object_internal_is_extensible s o
  | Builtin_is_extensible_proxy   -> proxy_object_internal_is_extensible s o

(** Function to dispatch calls to O.[[PreventExtensions]]() *)
and object_internal_prevent_extensions s o =
  let%some internal_method = run_object_method object_prevent_extensions_ s o in
  match internal_method with
  | Builtin_prevent_extensions_default -> ordinary_object_internal_prevent_extensions s o
  | Builtin_prevent_extensions_proxy   -> proxy_object_internal_prevent_extensions s o

(** Function to dispatch calls to O.[[GetOwnProperty]](P) *)
and object_internal_get_own_property s o p =
  let%some internal_method = run_object_method object_get_own_prop_ s o in
  let old_get_own_prop s o p =
    let%spec (s, desc) = run_object_get_own_prop s some_context o (string_of_value p) (* TODO: ES6 version *)
    in res_spec s (undef_descriptor_of_full_descriptor desc)
  in
  match internal_method with
  | Builtin_get_own_prop_default  -> ordinary_object_internal_get_own_property s o p
  | Builtin_get_own_prop_args_obj -> old_get_own_prop s o p (* TODO: ES6 version *)
  | Builtin_get_own_prop_string   -> old_get_own_prop s o p (* TODO: ES6 version *)
  | Builtin_get_own_prop_proxy    -> proxy_object_internal_get_own_property s o p

(** Function to dispatch calls to O.[[DefineOwnProperty]](P, desc) *)
and object_internal_define_own_property s o p desc =
  let%some internal_method = run_object_method object_define_own_prop_ s o in
  match internal_method with
  | Builtin_define_own_prop_default  -> ordinary_object_internal_define_own_property s o p desc
  | Builtin_define_own_prop_array    -> object_define_own_prop s some_context o (string_of_value p) desc false (* TODO: ES6 version *)
  | Builtin_define_own_prop_args_obj -> object_define_own_prop s some_context o (string_of_value p) desc false (* TODO: ES6 version *)
  | Builtin_define_own_prop_proxy    -> proxy_object_internal_define_own_property s o p desc

(** Function to dispatch calls to O.[[HasProperty]](P) *)
and object_internal_has_property s o p =
  let%some b = run_object_method object_has_prop_ s o in
  match b with
  | Builtin_has_prop_default -> ordinary_object_internal_has_property s o p
  | Builtin_has_prop_proxy   -> proxy_object_internal_has_property s o p

(** Function to dispatch calls to O.[[Get]](P, receiver) *)
and object_internal_get s o p receiver =
  let%some internal_method = (run_object_method object_get_ s o) in
  match internal_method with
  | Builtin_get_default  -> ordinary_object_internal_get s o p receiver
  | Builtin_get_args_obj -> object_get_builtin_args s internal_method receiver o p (* TODO: ES6-ify *)
  | Builtin_get_proxy    -> proxy_object_internal_get s o p receiver

(** Function to dispatch calls to O.[[Set]](P, V, Receiver) *)
and object_internal_set s o p v receiver =
  let%some internal_method = (run_object_method object_set_ s o) in
  match internal_method with
  | Builtin_set_default -> ordinary_object_internal_set s o p v receiver
  | Builtin_set_proxy   -> proxy_object_internal_set s o p v receiver

(** @deprecated TODO: Replace in favour of potentially the same [object_internal_set] ES6 method *)
and object_put s c l p v str = object_internal_set s l (Value_string p) v (Value_object l)

(** Function to dispatch calls to O.[[Delete]](P) *)
and object_internal_delete s o p =
  let%some internal_method = run_object_method object_delete_ s o in
  match internal_method with
  | Builtin_delete_default  -> ordinary_object_internal_delete s o p
  | Builtin_delete_args_obj -> object_delete s some_context o (string_of_value p) false (* TODO: Replace with ES6 version *)
  | Builtin_delete_proxy    -> proxy_object_internal_delete s o p

(** Function to dispatch calls to O.[[OwnPropertyKeys]]() *)
and object_internal_own_property_keys s o =
  let%some internal_method = run_object_method object_own_property_keys_ s o in
  match internal_method with
  | Builtin_own_property_keys_default -> ordinary_object_internal_own_property_keys s o
  | Builtin_own_property_keys_proxy   -> proxy_object_internal_own_property_keys s o

(** Function to dispatch calls to O.[[Call]](thisArgument, argumentsList) *)
and object_internal_call s o thisArgument argumentsList =
  let%some internal_method = run_object_method object_call_ s o in
  let%some internal_method = internal_method in
  match internal_method with
  | Call_proxy -> proxy_object_internal_call s o thisArgument argumentsList
  | _              -> run_call s some_context o thisArgument argumentsList (* TODO: ES5 *)

(** Function to dispatch calls to O.[[Construct]](argumentsList, newTarget) *)
and object_internal_construct s o argumentsList newTarget =
  let%some internal_method = run_object_method object_construct_ s o in
  let%some internal_method = internal_method in
  match internal_method with
  | Construct_proxy -> proxy_object_internal_construct s o argumentsList newTarget
  | _                   -> (* TODO: update to ES6 *)
    if not (newTarget === Value_object o) then Result_not_yet_implemented else
    run_construct s some_context internal_method o argumentsList


(** {3 The Reference Specification Type}
    @essec 6.2.4
    @esid sec-reference-specification-type *)

and get_base = ref_base

(* Note: may prefer this to return a raw string? *)
and get_referenced_name r =
  Value_string (ref_name r)

and is_strict_reference = ref_strict

(* TODO: Copied ES5 code, tidy up *)
and has_primitive_base r =
  ref_kind_comparable (ref_kind_of r) Ref_kind_primitive_base

and is_property_reference r =
  (ref_kind_of r === Ref_kind_object) || has_primitive_base r

and is_unresolvable_reference r =
  ref_kind_of r === Ref_kind_undef

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
      run_error_no_c s Native_error_ref
    else if is_property_reference v then
      let base = value_of_ref_base_type base in
      let%ret (s, base) =
        if has_primitive_base v then
          let%assert_ret _ = not (base === Value_undef || base === Value_null) in
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
    run_error_no_c s Native_error_ref
  else
  let v = ref_of_resvalue v in
  let base = get_base v in
  if is_unresolvable_reference v then
    if is_strict_reference v then
      run_error_no_c s Native_error_ref
    else
      let globalObj = get_global_object s c in
      set s globalObj (get_referenced_name v) w (Value_bool false)
  else if is_property_reference v then
    let base = value_of_ref_base_type base in
    let%ret s, base =
      if has_primitive_base v then
        let%assert_ret _ = not (base === Value_null || base === Value_undef) in
        let%OBJECT_ret s, base = to_object s base in
        Continue (s, base)
      else
        Continue (s, loc_of_value base)
    in
    let%bool s, succeeded = object_internal_set s base (get_referenced_name v) w (get_this_value v) in
    if (not succeeded) && (is_strict_reference v) then
      run_error_no_c s Native_error_type
    else
      res_void s
  else
    let base = env_loc_of_ref_base_type base in
    set_mutable_binding s base (get_referenced_name v) w (Value_bool (is_strict_reference v))

(** @esid sec-getthisvalue
    @essec 6.2.4.3 *)
and get_this_value v =
  let _ = spec_assert (is_property_reference v) in
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

(** @essec 6.2.5.4
    @esid sec-frompropertydescriptor *)
and from_property_descriptor s desc =
  if desc === Descriptor_undef then res_ter s (res_val Value_undef)
  else
  let desc = descriptor_get_defined desc in
  let%value s, obj = object_create s (Value_object (Object_loc_prealloc Prealloc_object_proto)) None in
  let%assert _ = type_of obj === Type_object in (* TODO: is extensible with no own properties *)
  let%ret s = if is_some desc.descriptor_value then
    let%bool_ret s, result = create_data_property s obj (Value_string "value") (unsome_error desc.descriptor_value) in
    let%assert_ret _ = result in
    Continue s
    else Continue s

  in let%ret s = if is_some desc.descriptor_writable then
    let%bool_ret s, result = create_data_property s obj (Value_string "writable") (Value_bool (unsome_error desc.descriptor_writable)) in
    let%assert_ret _ = result in
    Continue s
    else Continue s

  in let%ret s = if is_some desc.descriptor_get then
    let%bool_ret s, result = create_data_property s obj (Value_string "get") (unsome_error desc.descriptor_get) in
    let%assert_ret _ = result in
    Continue s
    else Continue s

  in let%ret s = if is_some desc.descriptor_set then
    let%bool_ret s, result = create_data_property s obj (Value_string "set") (unsome_error desc.descriptor_set) in
    let%assert_ret _ = result in
    Continue s
    else Continue s

  in let%ret s = if is_some desc.descriptor_enumerable then
    let%bool_ret s, result = create_data_property s obj (Value_string "enumerable") (Value_bool (unsome_error desc.descriptor_enumerable)) in
    let%assert_ret _ = result in
    Continue s
    else Continue s

  in let%ret s = if is_some desc.descriptor_configurable then
    let%bool_ret s, result = create_data_property s obj (Value_string "configurable") (Value_bool (unsome_error desc.descriptor_configurable)) in
    let%assert_ret _ = result in
    Continue s
    else Continue s
  (* The bulk CreateDataProperty returns true assertion of step 10 has been placed into each individual operation *)
  in res_ter s (res_val obj)

(** @esid sec-topropertydescriptor
    @essec 6.2.5.5 *)
and to_property_descriptor s _foo_ =
  match _foo_ with
  | Value_object l ->
    let desc = descriptor_intro_empty in
    let%bool (s, hasEnumerable) = has_property s _foo_ (Value_string "enumerable") in
    let%spec (s, desc) =
      if hasEnumerable
      then
        let%value (s, v) = get s _foo_ (Value_string "enumerable") in
        let enum = to_boolean v in
        res_spec s (descriptor_with_enumerable desc (Some enum))
      else res_spec s desc
    in
    let%bool (s, hasConfigurable) = has_property s _foo_ (Value_string "configurable") in
    let%spec (s, desc) =
      if hasConfigurable
      then
        let%value (s, v) = get s _foo_ (Value_string "configurable") in
        let conf = to_boolean v in
        res_spec s (descriptor_with_configurable desc (Some conf))
      else res_spec s desc
    in
    let%bool (s, hasValue) = has_property s _foo_ (Value_string "value") in
    let%spec (s, desc) =
      if hasValue
      then
        let%value (s, value) = get s _foo_ (Value_string "value") in
        res_spec s (descriptor_with_value desc (Some value))
      else res_spec s desc
    in
    let%bool (s, hasWritable) = has_property s _foo_ (Value_string "writable") in
    let%spec (s, desc) =
      if hasValue
      then
        let%value (s, v) = get s _foo_ (Value_string "writable") in
        let writable = to_boolean v in
        res_spec s (descriptor_with_writable desc (Some writable))
      else res_spec s desc
    in
    let%bool (s, hasGet) = has_property s _foo_ (Value_string "get") in
    let%spec (s, desc) =
      if hasGet
      then
        let%value (s, getter) = get s _foo_ (Value_string "get") in
        if (not (is_callable s getter)) && (not (type_of getter === Type_undef))
        then run_error_no_c s Native_error_type
        else res_spec s (descriptor_with_get desc (Some getter))
      else res_spec s desc
    in
    let%bool (s, hasSet) = has_property s _foo_ (Value_string "set") in
    let%spec (s, desc) =
      if hasSet
      then
        let%value (s, setter) = get s _foo_ (Value_string "set") in
        if (not (is_callable s setter)) && (not (type_of setter === Type_undef))
        then run_error_no_c s Native_error_type
        else res_spec s (descriptor_with_set desc (Some setter))
      else res_spec s desc
    in
    if (is_some desc.descriptor_get || is_some desc.descriptor_set)
      && (is_some desc.descriptor_value || is_some desc.descriptor_writable)
    then run_error_no_c s Native_error_type
    else res_spec s desc
  | _ -> throw_result (run_error_no_c s Native_error_type)

(** Completes a property descriptor by setting default fields.

  WARNING: ⚠️ This implementation treats Property Descriptors as value types
  instead of as reference types as the specification intends. This method
  is used in the spec as "Call CompletePropertyDescriptor(Desc)" expecting
  that Desc will be mutated. ⚠️

  @essec 6.2.5.6
  @esid sec-completepropertydescriptor *)
and complete_property_descriptor desc =
  let desc = descriptor_get_defined desc in
  let like = { descriptor_value        = Some Value_undef;
               descriptor_writable     = Some false;
               descriptor_get          = Some Value_undef;
               descriptor_set          = Some Value_undef;
               descriptor_enumerable   = Some false;
               descriptor_configurable = Some false }
  in let desc =
    if is_generic_descriptor (Descriptor desc) || is_data_descriptor (Descriptor desc)
    then
      let desc = if is_none desc.descriptor_value then descriptor_with_value desc like.descriptor_value else desc in
      let desc = if is_none desc.descriptor_writable then descriptor_with_writable desc like.descriptor_writable else desc in
      desc
    else
      let desc = if is_none desc.descriptor_get then descriptor_with_get desc like.descriptor_get else desc in
      let desc = if is_none desc.descriptor_set then descriptor_with_set desc like.descriptor_set else desc in
      desc
  in let desc = if is_none desc.descriptor_enumerable then descriptor_with_enumerable desc like.descriptor_enumerable else desc
  in let desc = if is_none desc.descriptor_configurable then descriptor_with_configurable desc like.descriptor_configurable else desc
  in desc

(** {1 Abstract Operations }
    @essec 7
    @esid sec-abstract-operations *)

(** {2 Type Conversion }
    @essec 7.1
    @esid sec-type-conversion *)

(** @essec 7.1.1
    @esid sec-toprimitive *)
and to_primitive s input preferredType =
  match input with
  (** @esid table-9 *)
  | Value_undef    -> res_out s (res_val input)
  | Value_null     -> res_out s (res_val input)
  | Value_bool _   -> res_out s (res_val input)
  | Value_number _ -> res_out s (res_val input)
  | Value_string _ -> res_out s (res_val input)
  (* | Value_symbol _ -> res_out s (res_val input) *)
  | Value_object l ->
    (* TODO: ES5 HACK! *)
    let%prim s0, r = object_default_value s some_context l preferredType in
    res_ter s0 (res_val r)


(** @essec 7.1.2
    @esid sec-toboolean *)
and to_boolean argument = match argument with
| Value_undef -> false
| Value_null -> false
| Value_bool b -> b
| Value_number n ->
  if (JsNumber.isposzero n) || (JsNumber.isnegzero n) || (JsNumber.isnan n) then false
  else true
| Value_string s -> if string_eq s "" then false else true
(* | Value_symbol s -> true *)
| Value_object o -> true

(** @essed 7.1.3
    @esid sec-tonumber *)
and to_number s argument = match argument with
(** @esid table-11 *)
| Value_undef    -> res_ter s (res_val (Value_number JsNumber.nan))
| Value_null     -> res_ter s (res_val (Value_number JsNumber.zero))
| Value_bool b   ->
    if b then res_ter s (res_val (Value_number JsNumber.one))
    else res_ter s (res_val (Value_number JsNumber.zero))
| Value_number n -> res_ter s (res_val (Value_number n))
| Value_string x -> res_ter s (res_val (Value_number (to_number_string x)))
(* TODO: | Value_symbol _ -> run_error_no_c s Native_error_type *)
| Value_object l ->
  let%value s, primValue = to_primitive s argument (Some Preftype_number) in
  to_number s primValue

(** TODO: ES6-ify
    @essec 7.1.3.1
    @esid sec-tonumber-applied-to-the-string-type *)
and to_number_string s = JsNumber.from_string s

(** @essec 7.1.4
    @esid sec-tointeger *)
and to_integer s argument =
  let%number s, number = to_number s argument in
  if JsNumber.isnan number then
    res_ter s (res_val (Value_number JsNumber.zero))
  else if (JsNumber.isposzero number) || (JsNumber.isnegzero number) ||
          (number === JsNumber.infinity) || (number === JsNumber.neg_infinity) then
    res_ter s (res_val (Value_number number))
  else
    res_ter s (res_val (Value_number ((JsNumber.sign number) *. (JsNumber.floor (JsNumber.absolute number)))))


(** @essec 7.1.12
    @esid sec-tostring *)
and to_string s argument =
  match argument with
  (** @esid table-12 *)
  | Value_undef    -> res_out s (res_val (Value_string "undefined"))
  | Value_null     -> res_out s (res_val (Value_string "null"))
  | Value_bool b   -> (match b with
    | true  -> res_out s (res_val (Value_string "true"))
    | false -> res_out s (res_val (Value_string "false")))
  | Value_number n -> res_out s (res_val (Value_string (to_string_number n)))
  | Value_string _ -> res_out s (res_val argument)
  (* | Value_symbol _ -> run_error_no_c s Native_error_type *)
  | Value_object o ->
      let%value s, primValue = to_primitive s argument (Some Preftype_string) in
      to_string s primValue

(** @essec 7.1.12.1
    @esid sec-tostring-applied-to-the-number-type *)
and to_string_number = JsNumber.to_string

(** @essec 7.1.14
    @esid sec-topropertykey *)
and to_property_key s argument =
  let%value s, key = to_primitive s argument (Some Preftype_string) in
  (* TODO: Symbol check *)
  let%VALUE s, str = to_string s key in
  res_out s (res_val str)

(** @essec 7.1.15
    @esid sec-tolength *)
and to_length s argument =
  let%number s, len = to_integer s argument in
  if len <= 0. then res_ter s (res_val (Value_number 0.))
  else res_ter s (res_val (Value_number (JsNumber.min len ((JsNumber.pow 2. 53.)-.1.))))

(** {2 Testing and Comparison Operations }
    @essec 7.2
    @esid sec-testing-and-comparison-operations *)

(** @essec 7.2.1
    @esid sec-requireobjectcoercible *)
and require_object_coercible s argument =
  match argument with
  (** @esid table-14 *)
  | Value_undef -> run_error_no_c s Native_error_type
  | Value_null  -> run_error_no_c s Native_error_type
  | Value_bool   _ -> res_ter s (res_val argument)
  | Value_number _ -> res_ter s (res_val argument)
  | Value_string _ -> res_ter s (res_val argument)
  (* | Value_symbol  _ -> res_ter s (res_val argument) *)
  | Value_object _ -> res_ter s (res_val argument)

(** @essec 7.2.3
    @esid sec-iscallable *)
and is_callable s argument =
  if not (type_of argument === Type_object) then false
  else object_has_internal_method s (loc_of_value argument) object_call_

(** @essec 7.2.4
    @esid sec-isconstructor *)
and is_constructor s argument =
  if not (type_of argument === Type_object) then false
  else object_has_internal_method s (loc_of_value argument) object_construct_

(** @essec 7.2.5
    @esid sec-isextensible-o *)
and is_extensible s o =
  let%assert _ = match type_of o with Type_object -> true | _ -> false in
  match o with
  | Value_object l -> object_internal_is_extensible s l
  | _ -> failwith "is_extensible received non-object"

(** @essec 7.2.7
    @esid sec-ispropertykey *)
and is_property_key argument =
  (type_of argument) === Type_string (* TODO: || (type_of argument) === Type_symbol *)

(** @essec 7.2.9
    @esid sec-samevalue *)
and same_value x y =
  if not (type_compare (type_of x) (type_of y))
  then false
  else match type_of x with
  | Type_number ->
    (match x with
    | Value_number n_x ->
      (match y with
      | Value_number n_y ->
        if (JsNumber.isnan n_x) && (JsNumber.isnan n_y) then true
        else if (JsNumber.isposzero n_x) && (JsNumber.isnegzero n_y) then true
        else if (JsNumber.isnegzero n_x) && (JsNumber.isposzero n_y) then true
        else n_x === n_y
      | _ -> failwith "impossible")
    | _ -> failwith "impossible")
  | _ -> same_value_non_number x y

(** @essec 7.2.11
    @esid sec-samevaluenonnumber *)
and same_value_non_number x y =
  let _ = spec_assert (not (type_compare (type_of x) Type_number)) in
  let _ = spec_assert (type_compare (type_of x) (type_of y)) in
  match x with
  | Value_undef      -> true
  | Value_null       -> true
  | Value_string s_x ->
    (match y with
    | Value_string s_y -> string_eq s_x s_y
    | _ -> spec_assert_fail ())
  | Value_bool b_x   ->
    (match y with
     | Value_bool b_y -> bool_eq b_x b_y
     | _ -> spec_assert_fail ())
  (* TODO: Symbol
  | Value_symbol s_x ->
     (match y with
     | Value_symbol s_y -> symbol_compare s_x s_y
     | _ -> spec_spec_assertion_failure ())
  *)
  | Value_object l_x ->
    (match y with
    | Value_object l_y -> object_loc_compare l_x l_y
    | _ -> spec_assert_fail ())
  | _ -> spec_assert_fail ()

(** {2 Operations on Objects }
    @essec 7.3
    @esid sec-operations-on-objects *)

(** @essec 7.3.1
    @esid sec-get-o-p *)
and get s o p =
  let%assert _ = (type_of o) === Type_object in
  let%assert _ = is_property_key p in
  let l = loc_of_value o in
  object_internal_get s l p o

(** @essec 7.3.2
    @esid sec-getv *)
and get_v s v p =
  let%assert _ = is_property_key p in
  let%object s, l = to_object s v in
  object_internal_get s l p v

(** @essec 7.3.3
    @esid sec-set-o-p-v-throw *)
and set s o p v throw =
  let%assert _ = (type_of o) === Type_object in
  let l = loc_of_value o in
  let%assert _ = is_property_key p in
  let%assert _ = (type_of throw) === Type_bool in
  let throw = bool_of_value throw in
  let%bool s, success = object_internal_set s l p v o in
  if (not success) && throw then
    run_error_no_c s Native_error_type
  else
    res_ter s (res_val (Value_bool success))

(** @essec 7.3.4
    @esid sec-createdataproperty *)
and create_data_property s o p v =
  let%assert _ = type_of o === Type_object in
  let o = loc_of_value o in
  let%assert _ = is_property_key p in
  let newDesc = descriptor_intro_data v true true true in
  object_internal_define_own_property s o p newDesc

(** @essec 7.3.7
    @esid sec-definepropertyorthrow *)
and define_property_or_throw s o p desc =
  let%assert _ = type_of o === Type_object in
  let o = loc_of_value o in
  let%assert _ = is_property_key p in
  let%bool s, success = object_internal_define_own_property s o p desc in
  if not success then
    run_error_no_c s Native_error_type
  else
    res_ter s (res_val (Value_bool success))

(** @essec 7.3.9
    @esid sec-getmethod *)
and get_method s v p =
  let%assert _ = is_property_key p in
  let%value s, func = get_v s v p in
  match type_of func with
  | Type_undef -> res_out s (res_val Value_undef)
  | Type_null  -> res_out s (res_val Value_undef)
  | _ ->
    let callable = is_callable s func in
    if not callable
    then run_error_no_c s Native_error_type
    else res_out s (res_val func)

(** @essec 7.3.10
    @esid sec-hasproperty *)
and has_property s o p =
  let%assert _ = (type_of o) === Type_object in
  let%assert _ = is_property_key p in
  let l = loc_of_value o in
  object_internal_has_property s l p

(** @essec 7.3.12
    @esid sec-call *)
and call s f v argumentList =
  let argumentList = unsome_default [] argumentList in
  let callable = is_callable s f in
  if not callable then run_error_no_c s Native_error_type
  else
  let l = loc_of_value f in
  object_internal_call s l v argumentList

(** @essec 7.3.13
    @esid sec-construct *)
and construct s f argumentsList newTarget =
  let newTarget = unsome_default f newTarget in
  let argumentsList = unsome_default [] argumentsList in
  let%assert _ = is_constructor s f in
  let%assert _ = is_constructor s newTarget in
  object_internal_construct s (loc_of_value f) argumentsList newTarget

(** @essec 7.3.14
    @esid sec-setintegritylevel *)
and set_integrity_level s o level =
  let%assert _ = type_of o === Type_object in
  let o = loc_of_value o in
  let%assert _ = level === "sealed" || level === "frozen" in
  let%bool s, status = object_internal_prevent_extensions s o in
  if not status then res_ter s (res_val (Value_bool false))
  else
  let%spec s, keys = object_internal_own_property_keys s o in
  let%ret s =
  if level === "sealed" then
    (* TODO: repeat for k in keys *)
    Return Result_not_yet_implemented
  else
    (* TODO: repeat for k in keys *)
    Return Result_not_yet_implemented
  in
  res_ter s (res_val (Value_bool true))

(** @essec 7.3.15
    @esid sec-testintegritylevel *)
and test_integrity_level s o level =
  let%assert _ = type_of o === Type_object in
  let%assert _ = level === "sealed" || level === "frozen" in
  let%bool s, status = is_extensible s o in
  if status then res_ter s (res_val (Value_bool false))
  else
  let%spec s, keys = object_internal_own_property_keys s (loc_of_value o) in
  (* TODO: repeat for k in keys *)
  Result_not_yet_implemented

(** @essec 7.3.16
    @esid sec-createarrayfromlist *)
and create_array_from_list s elements =
  (* first assertion is implicit through type signature *)
  let%value s, array = array_create s (Value_number 0.) None in
  let n = 0. in
  let%ret (s, n) = iterate elements (s, n) (fun e acc ->
      let s, n = acc in
      let%VALUE_ret s, tempVar = to_string s (Value_number n) in
      let%BOOL_ret s, status = create_data_property s array tempVar e in
      let%assert_ret _ = status in
      Continue (s, n +. 1.)) in
  res_out s (res_val array)

(** @essec 7.3.17
    @esid sec-createlistfromarraylike *)
and create_list_from_array_like s obj elementTypes =
  let elementTypes = unsome_default
      [Type_undef; Type_null; Type_bool; Type_string;
       (* TODO: Symbols Type_symbol; *) Type_number; Type_object]
      elementTypes in
  if not (type_of obj === Type_object) then run_error_no_c s Native_error_type else
  let%value s, tempVar = get s obj (Value_string "length") in
  let%number s, len = to_length s tempVar in
  let list = [] in
  let index = 0. in
  (* TODO: Allow fun expressions to deconstruct tuples, to avoid this verbosity! *)
  let%ret (s, index, list) = repeat (fun acc -> let (s, index, list) = acc in index < len) (s, index, list) (fun acc ->
      let (s, index, list) = acc in
      let%VALUE_ret s, indexName = to_string s (Value_number index) in
      let%value_ret s, next = get s obj indexName in
      if not (mem_decide (fun x y -> x === y) (type_of next) elementTypes) then
        Return (run_error_no_c s Native_error_type)
      else
      let list = append list [next] in
      let index = index +. 1. in
      Continue (s, index, list)) in
  res_spec s list

(** @essec 7.3.21
    @esid sec-enumerableownproperties *)
and enumerable_own_properties s o kind =
  Result_not_yet_implemented

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
  | Env_record_decl e           -> decl_env_record_has_binding s e n
  | Env_record_object (l, this) -> object_env_record_has_binding s l this n

and create_mutable_binding s e n d =
  match e with
  | Env_record_decl e           -> decl_env_record_create_mutable_binding s e n d
  | Env_record_object (l, this) -> object_env_record_create_mutable_binding s l this n d

and create_immutable_binding s e n str =
  match e with
  | Env_record_decl e           -> decl_env_record_create_immutable_binding s e n str
  | Env_record_object (l, this) -> object_env_record_create_immutable_binding s l this n str

and initialize_binding s e n v =
  match e with
  | Env_record_decl e           -> decl_env_record_initialize_binding s e n v
  | Env_record_object (l, this) -> object_env_record_initialize_binding s l this n v
*)

and set_mutable_binding s l n v str =
  let%some e = env_record_binds_option s l in
  match e with
  | Env_record_decl e           -> decl_env_record_set_mutable_binding s l e n v str
  | Env_record_object (l, this) -> object_env_record_set_mutable_binding s (Value_object l) this n v str

and get_binding_value s l n str =
  let%some e = env_record_binds_option s l in
  match e with
  | Env_record_decl e           -> decl_env_record_get_binding_value s e n str
  | Env_record_object (l, this) -> object_env_record_get_binding_value s (Value_object l) this n str

(*
and delete_binding s e n =
  match e with
  | Env_record_decl e           -> decl_env_record_delete_binding s e n
  | Env_record_object (l, this) -> object_env_record_delete_binding s l this n

and has_this_binding s e =
  match e with
  | Env_record_decl e           -> decl_env_record_has_this_binding s e
  | Env_record_object (l, this) -> object_env_record_has_this_binding s l this

and has_super_binding s e =
  match e with
  | Env_record_decl e           -> decl_env_record_has_super_binding s e
  | Env_record_object (l, this) -> object_env_record_has_super_binding s l this

and with_base_object s e =
  match e with
  | Env_record_decl e           -> decl_env_record_with_base_object s e
  | Env_record_object (l, this) -> object_env_record_with_base_object s l this
*)

(**
    {4 Declarative Environment Records}
    @essec 8.1.1.1
    @esid sec-declarative-environment-records
*)

(* TODO: Move to a bindings data structure section. Apply to mutable bindings also. *)
and binding_is_uninitialized binding =
  let (mutability, unused) = binding in
  mutability_compare mutability Mutability_uninitialized_immutable

and binding_is_mutable binding =
  let (mutability, unused) = binding in
  (mutability === Mutability_nondeletable) || (mutability === Mutability_deletable)

(** @essec 8.1.1.1.2
    @esid sec-declarative-environment-records-createmutablebinding-n-d *)
and decl_env_record_create_mutable_binding s l envRec n' d =
  let n = string_of_value n' in
  let d = bool_of_value d in
  let%assert _ = not (HeapStr.indom_dec envRec n) in
  let s = env_record_write_decl_env s l n (mutability_of_bool d) Value_undef in (* TODO: Uninitialized field *)
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
      run_error_no_c s Native_error_ref
    else
      let%success s, _ = decl_env_record_create_mutable_binding s l envRec n' (Value_bool true) in
      let%success s, _ = decl_env_record_initialize_binding s l envRec n' v in
      res_void s
  else
  let%some binding = HeapStr.read_option envRec n in
  let (mutability, unused) = binding in
  (* TODO: Implement strictness for bindings: let str = if binding.ref_strict then true else str in *)
  if binding_is_uninitialized binding then
    run_error_no_c s Native_error_ref
  else let%ret s =
    if binding_is_mutable binding then
      Continue (env_record_write_decl_env s l n mutability v)
    else
      if str then
        Return (run_error_no_c s Native_error_type)
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
  if mutability_compare mutability Mutability_uninitialized_immutable (* TODO: Need to handle mutable uninitialized also *)
  then run_error_no_c s Native_error_ref
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
    then res_ter s (res_val Value_undef)
    else run_error_no_c s Native_error_ref
  else
    get s bindings n

(** {2 Execution Contexts}
    @essec 8.3
    @esid sec-execution-contexts *)

(** @esid sec-getglobalobject
    @essec 8.3.6 *)
and get_global_object s ctx =
  (* TODO: ES5 hack (realms required) *)
  let e = unsome_error (env_record_binds_option s env_loc_global_env_record) in
  match e with
  | Env_record_object (l, this) -> Value_object l
  |  _ -> failwith "get_global_object e not a env_record_object"

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
  let%value s, v = ordinary_get_prototype_of s o in
  res_out s (res_val v)

(** @essec 9.1.1.1
    @esid sec-ordinarygetprototypeof *)
and ordinary_get_prototype_of s o =
  let%some v = run_object_method object_proto_ s o in
  res_out s (res_val v)

(** [[SetPrototypeOf]](V)
    @essec 9.1.2
    @esid sec-ordinary-object-internal-methods-and-internal-slots-setprototypeof-v *)
and ordinary_object_internal_set_prototype_of s o v =
  let%value s, v = ordinary_set_prototype_of s o v in
  res_out s (res_val v)

(** @essec 9.1.2.1
    @esid sec-ordinarysetprototypeof *)
and ordinary_set_prototype_of s o v =
  let%assert _ = (match type_of v with Type_object -> true | Type_null -> true | _ -> false) in
  let%some extensible = run_object_method object_extensible_ s o in
  let%some current = run_object_method object_prototype_ s o in
  let sv = same_value v current in
  if sv then res_out s (res_val (Value_bool true))
  else if not extensible then res_out s (res_val (Value_bool false))
  else
    let p = v in
    let done' = false in
    let%ret (s, _, _) = repeat (fun acc -> let (_, done', _) = acc in not done') (s, done', p) (fun acc ->
      let (s, done', p) = acc in
      if p === Value_null then Continue (s, true, p)
      else if same_value p (Value_object o) then Return (res_out s (res_val (Value_bool false)))
      else
        let%some_ret gpo = run_object_method object_get_prototype_of_ s (loc_of_value p) in
        if not (gpo === Builtin_get_prototype_of_default) then Continue (s, true, p)
        else
          let%some_ret p = run_object_method object_prototype_ s (loc_of_value p) in
          Continue (s, done', p)
    ) in
    let%some s = run_object_set_internal object_set_proto s o v in
    res_out s (res_val (Value_bool true))

(** @essec 9.1.3
    @esid sec-ordinary-object-internal-methods-and-internal-slots-isextensible *)
and ordinary_object_internal_is_extensible s o =
  let%value s, v = ordinary_is_extensible s o in
  res_out s (res_val v)

(** @essec 9.1.3.1
    @esid sec-ordinaryisextensible *)
and ordinary_is_extensible s o =
  let%some b = run_object_method object_extensible_ s o in
  res_out s (res_val (Value_bool b))

(** @essec 9.1.4
    @esid sec-ordinary-object-internal-methods-and-internal-slots-preventextensions *)
and ordinary_object_internal_prevent_extensions s o =
  let%value s, v = ordinary_prevent_extensions s o in
  res_out s (res_val v)

(** @essec 9.1.4.1
    @esid sec-ordinarypreventextensions *)
and ordinary_prevent_extensions s o =
  let%some s = run_object_set_internal object_set_extensible s o false in
  res_ter s (res_val (Value_bool true))

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
  | Attributes_data_of x ->
      { d with descriptor_value    = (Some x.attributes_data_value);
               descriptor_writable = (Some x.attributes_data_writable) }
  | Attributes_accessor_of x ->
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
  validate_and_apply_property_descriptor s (Value_object o) p extensible desc current

(** @essec 9.1.6.2
    @esid sec-iscompatiblepropertydescriptor *)
and is_compatible_property_descriptor s extensible desc current =
  validate_and_apply_property_descriptor s Value_undef Value_undef extensible desc current

(** @essec 9.1.6.3
    @esid sec-validateandapplypropertydescriptor *)
and validate_and_apply_property_descriptor s o p extensible desc current =
  (* NOTE: o, p type mismatch, specified as object, property key but undefined is passed by
     is_compatible_property_descriptor *)
  (* A -> B === !A || B *)
  let%assert _ = (value_compare o Value_undef) || (is_property_key p) in
  match current with
  (* Three types of descriptors: full, attributes: accessor and data...
     Spec assumes one with variable field definitions, move to this? (equiv. our full)
   *)
  | Descriptor_undef ->
    if not extensible then res_out s (res_val (Value_bool false))
    else
      let%assert _ = extensible in (* SPEC: This assert assumes extensible is any value, but is strictly typed for us *)
      let%some s =
        match o with
        | Value_object l ->
          let p = string_of_value p in
          if (is_generic_descriptor (Descriptor desc)) || (is_data_descriptor (Descriptor desc))
          then object_set_property s l p (Attributes_data_of (attributes_data_of_descriptor desc))
          else object_set_property s l p (Attributes_accessor_of (attributes_accessor_of_descriptor desc))
        | Value_undef -> Some s
        | _ -> None
      in res_ter s (res_val (Value_bool true))

  | Descriptor current ->
    (* The following two steps 3 & 4 of the spec are implied by the rest of the function, except in the
       case of NaN values for [[Value]] which may change internal representation.
       Otherwise, they are only an optimisation as far as I can tell. *)
    (* Step 3 (also implied by step 4) *)
    let%ret s = if descriptor_is_empty desc
      then Return (res_ter s (res_val (Value_bool true)))
      else Continue s

    (* Step 4 *)
    in let%ret s =
    if descriptor_contained_by desc current same_value
      then Return (res_ter s (res_val (Value_bool true)))
      else Continue s

    (* Steps 5 *)
    in let%ret s =
    if (option_compare bool_eq current.descriptor_configurable (Some false))
    then
      if (option_compare bool_eq desc.descriptor_configurable (Some true))
      then Return (res_ter s (res_val (Value_bool false)))
      else if ((is_some desc.descriptor_enumerable) &&
        not (some_compare bool_eq current.descriptor_enumerable desc.descriptor_enumerable))
      then Return (res_ter s (res_val (Value_bool false)))
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
        then Return (res_ter s (res_val (Value_bool false)))
      (* 7b *)
      else if is_data_descriptor (Descriptor current)
        then
          let s = unsome_default s (match o with
          | Value_object l -> object_map_property s l (string_of_value p) attributes_accessor_of_attributes_data
          | _ -> Some s)
          in Continue s
        else
          let s = unsome_default s (match o with
          | Value_object l -> object_map_property s l (string_of_value p) attributes_data_of_attributes_accessor
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
        then Return (res_ter s (res_val (Value_bool false)))

        (* Step 8aii *)
        else if option_compare bool_eq current.descriptor_writable (Some false)
        then
          (* Step 8aii1 *)
          if (is_some desc.descriptor_value)
             && not (option_compare same_value desc.descriptor_value current.descriptor_value)
          then Return (res_ter s (res_val (Value_bool false)))
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
        then Return (res_ter s (res_val (Value_bool false)))
        else if (is_some desc.descriptor_get) &&
                not (option_compare same_value desc.descriptor_get current.descriptor_get)
        then Return (res_ter s (res_val (Value_bool false)))
        else Continue s
      else Continue s

    (* Step 10 *)
    in let%some s = match o with
    | Value_object l ->
       object_map_property s l (string_of_value p) (fun a -> attributes_update a desc)
    | _ -> Some s

    (* Step 11 *)
    in res_ter s (res_val (Value_bool true))

(** @essesc 9.1.7
    @esid sec-ordinary-object-internal-methods-and-internal-slots-hasproperty-p *)
and ordinary_object_internal_has_property s o p =
  ordinary_has_property s o p

(** @essec 9.1.7.1
    @esid sec-ordinaryhasproperty *)
and ordinary_has_property s o p =
  let%assert _ = is_property_key p in
  let%spec s, hasOwn = object_internal_get_own_property s o p in
  if not (hasOwn === Descriptor_undef) then res_ter s (res_val (Value_bool true))
  else let%value s, parent = object_internal_get_prototype_of s o in
  if not (parent === Value_null) then object_internal_has_property s (loc_of_value parent) p
  else res_ter s (res_val (Value_bool false))

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
    if parent === Value_null then
      res_ter s (res_val Value_undef)
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
    if getter === Value_undef then
      res_ter s (res_val Value_undef)
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
      if not (parent === Value_null) then
        let parent = loc_of_value parent in
        Return (object_internal_set s parent p v receiver)
      else Continue (Descriptor {
        descriptor_value = Some Value_undef;
        descriptor_writable = Some true;
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
    if not writable then res_ter s (res_val (Value_bool false))
    else if not (type_of receiver === Type_object) then res_ter s (res_val (Value_bool false))
    else
      let receiver = loc_of_value receiver in
      let%spec s, existingDescriptor = object_internal_get_own_property s receiver p in
      if not (existingDescriptor === Descriptor_undef) then begin
        if is_accessor_descriptor existingDescriptor then res_ter s (res_val (Value_bool false))
        else
          let existingDescriptor = descriptor_get_defined existingDescriptor in
          let%some w = existingDescriptor.descriptor_writable in
          if not w then res_ter s (res_val (Value_bool false))
          else
            let valueDesc = (descriptor_with_value descriptor_intro_empty (Some v)) in
            object_internal_define_own_property s receiver p valueDesc
      end else
        create_data_property s (Value_object receiver) p v
  end else
    let%assert _ = is_accessor_descriptor ownDesc in
    let ownDesc = descriptor_get_defined ownDesc in
    let%some setter = ownDesc.descriptor_set in
    if setter === Value_undef then res_ter s (res_val (Value_bool false))
    else
      let%success s, _ = call s setter receiver (Some [v]) in
      res_ter s (res_val (Value_bool true))

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
    res_ter s (res_val (Value_bool true))
  else
  let desc = descriptor_get_defined desc in
  if some_compare bool_eq (descriptor_configurable desc) (Some true) then
    let p = string_of_value p in
    let%some s = run_object_heap_map_properties s o (fun props -> HeapStr.rem props p)  in
    res_ter s (res_val (Value_bool true))
  else
    res_ter s (res_val (Value_bool false))

and ordinary_object_internal_own_property_keys s o =
  let%spec s, k = ordinary_own_property_keys s o in
  res_spec s k

(** @essec 9.1.11.1
    @esid sec-ordinaryownpropertykeys *)
and ordinary_own_property_keys s o =
  let%some keys = object_properties_keys_as_list_option s o in
  (* FIXME: Precise key ordering is to be implemented here! *)
  let keys = LibList.map (fun key -> Value_string key) keys in
  res_spec s keys

(** @essec 9.1.12
    @esid sec-objectcreate *)
and object_create s proto internalSlotsList =
  let internalSlotsList = unsome_default [] internalSlotsList in
  (* TODO: Do something with internalSlotsList *)
  (* TODO: Draw this definition closer to the spec language *)
  let obj = object_new proto "" in
  let l, s = object_alloc s obj in
  res_ter s (res_val (Value_object l))

(** {2 ECMAScript Function Objects}
    @essec 9.2
    @esid sec-ecmascript-function-objects *)

(** @essec 9.2.7
    @esid sec-addrestrictedfunctionproperties *)
and add_restricted_function_properties s f realm =
  (*  TODO: Realm support *)
  (* Assert ... *)
  let thrower = Value_object (Object_loc_prealloc Prealloc_throw_type_error) in
  let%SUCCESS s, _ = define_property_or_throw s f (Value_string "caller") {
      descriptor_intro_empty with
      descriptor_get = Some thrower;
      descriptor_set = Some thrower;
      descriptor_enumerable = Some false;
      descriptor_configurable = Some true;
    } in
  let%SUCCESS s, rv = define_property_or_throw s f (Value_string "arguments") {
      descriptor_intro_empty with
      descriptor_get = Some thrower;
      descriptor_set = Some thrower;
      descriptor_enumerable = Some false;
      descriptor_configurable = Some true;
    } in
  res_ter s (res_normal rv)


(** @essec 9.2.7.1
    @esid sec-%throwtypeerror% *)
and builtin_throw_type_error s c thisArgument argumentsList newTarget =
  run_error s c Native_error_type

(** {2 Built-in Function Objects}
    @essec 9.3
    @esid sec-built-in-function-objects *)

(** @essec 9.3.3
    @esid sec-createbuiltinfunction

    Note: This function has an additional 'length' property that is implicit in the specification. Ditto for whether the
    function is a constructor.
    TODO: Suggest specification amendments to make the setting of this explicit in CreateBuiltinFunction. Possibly fold
    this into the 'steps' psuedo-value?
*)
and create_builtin_function s steps internalSlotsList realm prototype length isconstructor =
  (* 1. Assert: steps is either a set of algorithm steps or other definition of a function's behaviour provided in this specification. *)
  (* 2. If realm is not present, set realm to te current Realm Record. *)
  (* 3. Assert: realm is a Realm Record. *)
  (* TODO (realms): 4. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%FunctionPrototype%]]. *)
  let prototype = unsome_default (Value_object (Object_loc_prealloc Prealloc_function_proto)) prototype in

  (* 5. Let func be a new built-in function object that when called performs the action described by steps. The new function
     object has internal slots whose names are the elements of internalSlotsList. The initial value of each of those
     internal slots is undefined. *)
  (* TODO: 6. Set func.[[Realm]] to realm. *)
  (* 7. Set func.[[Prototype]] to prototype. *)
  (* 8. Set func.[[Extensible]] to true. *)
  (* 9. Set func.[[ScriptOrModule]] to null. *)
  (* 10. Return func. *)
  let s, func = builtin_function_new s prototype steps length isconstructor in
  res_ter s (res_val func)

(** {2 Array Exotic Objects}
    @essec 9.4.2
    @esid sec-array-exotic-objects *)
(** @essec 9.4.2.2
    @esid sec-arraycreate *)
and array_create s length proto =
  (* TODO: ES5 HACK *)
  run_construct_prealloc s some_context Prealloc_array [length]

(** {2 Proxy Object Internal Methods and Internal Slots}
    @essec 9.5
    @esid sec-proxy-object-internal-methods-and-internal-slots *)

(** {3 Specification text helper functions} *)
(** Implements the specification text "If [l] is a Proxy exotic object" *)
and is_proxy_exotic_object s l =
  object_has_internal_slot s l object_proxy_handler_

(** @essec 9.5.1
    @esid sec-proxy-object-internal-methods-and-internal-slots-getprototypeof *)
and proxy_object_internal_get_prototype_of s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "getPrototypeOf") in
  if trap === Value_undef then
    object_internal_get_prototype_of s (loc_of_value target)
  else
  let%value s, handlerProto = call s trap handler (Some [target]) in
  if not ((type_of handlerProto === Type_object) || (handlerProto === Value_null)) then
    run_error_no_c s Native_error_type
  else
  let%bool s, extensibleTarget = is_extensible s target in
  if extensibleTarget then res_ter s (res_val handlerProto)
  else
  let%value s, targetProto = object_internal_get_prototype_of s (loc_of_value target) in
  if not (same_value handlerProto targetProto) then
    run_error_no_c s Native_error_type
  else
    res_ter s (res_val handlerProto)

(** @essec 9.5.2
    @esid sec-proxy-object-internal-methods-and-internal-slots-setprototypeof-v *)
and proxy_object_internal_set_prototype_of s o v =
  let%assert _ = (type_of v) === Type_object || (type_of v) === Type_null in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "setPrototypeOf") in
  if trap === Value_undef then
    object_internal_set_prototype_of s (loc_of_value target) v
  else
  let%value s, tempVal = call s trap handler (Some [target; v]) in
  let booleanTrapResult = to_boolean tempVal in
  if not booleanTrapResult then res_ter s (res_val (Value_bool false))
  else
  let%bool s, extensibleTarget = is_extensible s target in
  if extensibleTarget then res_ter s (res_val (Value_bool true))
  else
  let%value s, targetProto = object_internal_get_prototype_of s (loc_of_value target) in
  if not (same_value v targetProto) then run_error_no_c s Native_error_type
  else res_ter s (res_val (Value_bool true))

(** @essec 9.5.3
    @esid sec-proxy-object-internal-methods-and-internal-slots-isextensible *)
and proxy_object_internal_is_extensible s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "isExtensible") in
  if trap === Value_undef then
    object_internal_is_extensible s (loc_of_value target)
  else
  let%value s, tempVal = call s trap handler (Some [target]) in
  let booleanTrapResult = to_boolean tempVal in
  let%value s, targetResult = object_internal_is_extensible s (loc_of_value target) in
  if not (same_value (Value_bool booleanTrapResult) targetResult) then run_error_no_c s Native_error_type
  else res_ter s (res_val (Value_bool booleanTrapResult))

(** @essec 9.5.4
    @esid sec-proxy-object-internal-methods-and-internal-slots-preventextensions *)
and proxy_object_internal_prevent_extensions s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "preventExtensions") in
  if trap === Value_undef then
    object_internal_prevent_extensions s (loc_of_value target)
  else
  let%value s, tempVal = call s trap handler (Some [target]) in
  let booleanTrapResult = to_boolean tempVal in
  let%ret s =
  if booleanTrapResult then
    let%bool_ret s, targetIsExtensible = object_internal_is_extensible s (loc_of_value target) in
    if targetIsExtensible then Return (run_error_no_c s Native_error_type)
    else Continue s
  else Continue s
  in
  res_ter s (res_val (Value_bool booleanTrapResult))

(** @essec 9.5.5
    @esid  sec-proxy-object-internal-methods-and-internal-slots-getownproperty-p *)
and proxy_object_internal_get_own_property s o p =
  let%assert _ = is_property_key p in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "getOwnPropertyDescriptor") in
  if trap === Value_undef then
    object_internal_get_own_property s (loc_of_value target) p
  else
  let%value s, trapResultObj = call s trap handler (Some [target; p]) in
  if not ((type_of trapResultObj === Type_object) || (trapResultObj === Value_undef)) then
    throw_result (run_error_no_c s Native_error_type)
  else
  let%spec s, targetDesc = object_internal_get_own_property s (loc_of_value target) p in
  (* targetDesc MUST be a full descriptor or undef, by 6.1.7.3 *)
  if trapResultObj === Value_undef then
    if targetDesc === Descriptor_undef then
      res_spec s Descriptor_undef
    else
      let targetDesc = descriptor_get_defined targetDesc in
      if unsome_error (descriptor_configurable targetDesc) === false then
        throw_result (run_error_no_c s Native_error_type)
      else
        let%bool s, extensibleTarget = is_extensible s target in
        if not extensibleTarget then
          throw_result (run_error_no_c s Native_error_type)
        else
          res_spec s Descriptor_undef
  else
  let%bool s, extensibleTarget = is_extensible s target in
  let%spec s, resultDesc = to_property_descriptor s trapResultObj in
  let resultDesc = complete_property_descriptor (Descriptor resultDesc) in (* TODO: See note in [complete_property_descriptor] *)
  let%bool s, valid = is_compatible_property_descriptor s extensibleTarget resultDesc targetDesc in
  if not valid then
    run_error_no_c s Native_error_type
  else if unsome_error (descriptor_configurable resultDesc) === false &&
    (targetDesc === Descriptor_undef || unsome_error (descriptor_configurable (descriptor_get_defined targetDesc)) === true) then
      run_error_no_c s Native_error_type
  else
    res_spec s (Descriptor resultDesc)

(** @essec 9.5.6
    @esid  sec-proxy-object-internal-methods-and-internal-slots-defineownproperty-p-desc *)
and proxy_object_internal_define_own_property s o p desc =
  let%assert _ = is_property_key p in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "defineProperty") in
  if trap === Value_undef then
    object_internal_define_own_property s (loc_of_value target) p desc
  else
  let%value s, descObj = from_property_descriptor s (Descriptor desc) in
  let%value s, tempVar = call s trap handler (Some [target; p; descObj]) in
  let booleanTrapResult = to_boolean tempVar in
  if not booleanTrapResult then
    res_ter s (res_val (Value_bool false))
  else
  let%spec s, targetDesc = object_internal_get_own_property s (loc_of_value target) p in
  let%bool s, extensibleTarget = is_extensible s target in
  let settingConfigFalse = is_some desc.descriptor_configurable && unsome_error desc.descriptor_configurable === false in
  let%ret s =
    if targetDesc === Descriptor_undef then
      if not extensibleTarget then Return (run_error_no_c s Native_error_type)
      else if settingConfigFalse then Return (run_error_no_c s Native_error_type)
      else Continue s
    else
      let%bool_ret s, tempVal = is_compatible_property_descriptor s extensibleTarget desc targetDesc in
      let targetDesc = descriptor_get_defined targetDesc in
      if not tempVal then
        Return (run_error_no_c s Native_error_type)
      else if settingConfigFalse && unsome_error targetDesc.descriptor_configurable === true then
        Return (run_error_no_c s Native_error_type)
      else Continue s
  in
  res_ter s (res_val (Value_bool true))


(** @essec 9.5.7
    @esid  sec-proxy-object-internal-methods-and-internal-slots-hasproperty-p *)
and proxy_object_internal_has_property s o p =
  let%assert _ = is_property_key p in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "has") in
  if trap === Value_undef then
    object_internal_has_property s (loc_of_value target) p
  else
  let%value s, tempVar = call s trap handler (Some [target; p]) in
  let booleanTrapResult = to_boolean tempVar in
  let%ret s =
  if not booleanTrapResult then
    let%spec_ret s, targetDesc = object_internal_get_own_property s (loc_of_value target) p in
    if not (targetDesc === Descriptor_undef) then
      let targetDesc = descriptor_get_defined targetDesc in
      if unsome_error targetDesc.descriptor_configurable === false then
        Return (run_error_no_c s Native_error_type)
      else
      let%bool_ret s, extensibleTarget = is_extensible s target in
      if not extensibleTarget then Return (run_error_no_c s Native_error_type)
      else Continue s
    else Continue s
  else Continue s
  in res_ter s (res_val (Value_bool booleanTrapResult))

(** @essec 9.5.8
    @esid  sec-proxy-object-internal-methods-and-internal-slots-get-p-receiver *)
and proxy_object_internal_get s o p receiver =
  let%assert _ = is_property_key p in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "get") in
  if trap === Value_undef then
    object_internal_get s (loc_of_value target) p receiver
  else
  let%value s, trapResult = call s trap handler (Some [target; p; receiver]) in
  let%spec s, targetDesc = object_internal_get_own_property s (loc_of_value target) p in

  let%ret s =
  if not (targetDesc === Descriptor_undef) then
    let targetDesc' = descriptor_get_defined targetDesc in

    let%ret_ret s =
    if is_data_descriptor targetDesc && unsome_error targetDesc'.descriptor_configurable === false && unsome_error targetDesc'.descriptor_writable === false then
      if not (same_value trapResult (unsome_error targetDesc'.descriptor_value)) then
        Return (run_error_no_c s Native_error_type)
      else Continue s
    else Continue s

    in let%ret_ret s =
    if is_accessor_descriptor targetDesc && unsome_error targetDesc'.descriptor_configurable === false && unsome_error targetDesc'.descriptor_get === Value_undef then
      if not (trapResult === Value_undef) then
        Return (run_error_no_c s Native_error_type)
      else Continue s
    else Continue s

    in Continue s
  else Continue s

  in res_ter s (res_val trapResult)

(** @essec 9.5.9
    @esid  sec-proxy-object-internal-methods-and-internal-slots-set-p-v-receiver *)
and proxy_object_internal_set s o p v receiver =
  let%assert _ = is_property_key p in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "set") in
  if trap === Value_undef then
    object_internal_set s (loc_of_value target) p v receiver
  else
  let%value s, tempVar = call s trap handler (Some [target; p; v; receiver]) in
  let booleanTrapResult = to_boolean tempVar in
  if not booleanTrapResult then res_ter s (res_val (Value_bool false))
  else
  let%spec s, targetDesc = object_internal_get_own_property s (loc_of_value target) p in
  let%ret s =
  if not (targetDesc === Descriptor_undef) then
    let targetDesc' = descriptor_get_defined targetDesc in
    let%ret_ret s =
    if is_data_descriptor targetDesc && unsome_error targetDesc'.descriptor_configurable === false && unsome_error targetDesc'.descriptor_writable === false then
      if not (same_value v (unsome_error targetDesc'.descriptor_value)) then
        Return (run_error_no_c s Native_error_type)
      else Continue s
    else Continue s

    in let%ret_ret s =
    if is_accessor_descriptor targetDesc && unsome_error targetDesc'.descriptor_configurable === false then
      if unsome_error targetDesc'.descriptor_set === Value_undef then
        Return (run_error_no_c s Native_error_type)
      else Continue s
    else Continue s

    in Continue s
  else Continue s
  in res_ter s (res_val (Value_bool true))

(** @essec 9.5.10
    @esid  sec-proxy-object-internal-methods-and-internal-slots-delete-p *)
and proxy_object_internal_delete s o p =
  let%assert _ = is_property_key p in
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "deleteProperty") in
  if trap === Value_undef then
    object_internal_delete s (loc_of_value target) p
  else
  let%value s, tempVar = call s trap handler (Some [target; p]) in
  let booleanTrapResult = to_boolean tempVar in
  if not booleanTrapResult then res_ter s (res_val (Value_bool false))
  else
  let%spec s, targetDesc = object_internal_get_own_property s (loc_of_value target) p in
  if targetDesc === Descriptor_undef then res_ter s (res_val (Value_bool true))
  else
  let targetDesc = descriptor_get_defined targetDesc in
  if unsome_error targetDesc.descriptor_configurable === false then run_error_no_c s Native_error_type
  else res_ter s (res_val (Value_bool true))

(** @essec 9.5.11
    @esid  sec-proxy-object-internal-methods-and-internal-slots-ownpropertykeys *)
and proxy_object_internal_own_property_keys s o =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "ownKeys") in
  if trap === Value_undef then
    object_internal_own_property_keys s (loc_of_value target)
  else
  let%value s, trapResultArray = call s trap handler (Some [target]) in
  let%spec s, trapResult = create_list_from_array_like s trapResultArray (Some [Type_string(*TODO: ; Type_symbol*)]) in
  let%bool s, extensibleTarget = is_extensible s target in
  let%spec s, targetKeys = object_internal_own_property_keys s (loc_of_value target) in
  (* Assert that [targetKeys : (Type_string | Type_symbol) list *)
  let targetConfigurableKeys = [] in
  let targetNonconfigurableKeys = [] in
  let%ret (s, targetConfigurableKeys, targetNonconfigurableKeys)  =
      iterate targetKeys (s, targetConfigurableKeys, targetNonconfigurableKeys)
      (fun key acc -> let (s, targetConfigurableKeys, targetNonconfigurableKeys) = acc in
        let%spec_ret s, desc = object_internal_get_own_property s (loc_of_value target) key in
        if (not (desc === Descriptor_undef)) && unsome_error (descriptor_get_defined desc).descriptor_configurable === false then
          Continue (s, targetConfigurableKeys, append targetNonconfigurableKeys [key])
        else
          Continue (s, append targetConfigurableKeys [key], targetNonconfigurableKeys)
      )
  in
  if extensibleTarget && is_empty targetNonconfigurableKeys then
    res_spec s trapResult
  else
  let uncheckedResultKeys = trapResult in
  let%ret s, uncheckedResultKeys = iterate targetNonconfigurableKeys (s, uncheckedResultKeys) (fun key acc ->
      let s, uncheckedResultKeys = acc in
      if not (mem_decide (fun x y -> x === y) key uncheckedResultKeys) then
        Return (run_error_no_c s Native_error_type)
      else
        Continue (s, filter (fun x -> not (x === key)) uncheckedResultKeys)
    )
  in
  if extensibleTarget then res_spec s trapResult
  else
  let%ret s, uncheckedResultKeys = iterate targetConfigurableKeys (s, uncheckedResultKeys) (fun key acc ->
      let s, uncheckedResultKeys = acc in
      if not (mem_decide (fun x y -> x === y) key uncheckedResultKeys) then
        Return (run_error_no_c s Native_error_type)
      else
        Continue (s, filter (fun x -> not (x === key)) uncheckedResultKeys)
    )
  in
  if not (is_empty uncheckedResultKeys) then run_error_no_c s Native_error_type
  else res_spec s trapResult

(** @essec 9.5.12
    @esid  sec-proxy-object-internal-methods-and-internal-slots-call-thisargument-argumentslist *)
and proxy_object_internal_call s o thisArgument argumentsList =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "apply") in
  if trap === Value_undef then
    call s target thisArgument (Some argumentsList)
  else
  let%value s, argArray = create_array_from_list s argumentsList in
  call s trap handler (Some [target; thisArgument; argArray])

(** @essec 9.5.13
    @esid  sec-proxy-object-internal-methods-and-internal-slots-construct-argumentslist-newtarget *)
and proxy_object_internal_construct s o argumentsList newTarget =
  let%some handler = run_object_method object_proxy_handler_ s o in
  let%some handler = handler in
  (** Note: this check also implies that [target] is not null *)
  if handler === Value_null then run_error_no_c s Native_error_type
  else
  let%assert _ = (type_of handler) === Type_object in
  let%some target = run_object_method object_proxy_target_ s o in
  let%some target = target in
  let%value s, trap = get_method s handler (Value_string "construct") in
  if trap === Value_undef then
    let%assert _ = object_has_internal_method s (loc_of_value target) object_construct_ in
    construct s target (Some argumentsList) (Some newTarget)
  else
  let%value s, argArray = create_array_from_list s argumentsList in
  let%value s, newObj = call s trap handler (Some [target; argArray; newTarget]) in
  if not (type_of newObj === Type_object) then run_error_no_c s Native_error_type
  else res_ter s (res_val newObj)

(** @essec 9.5.14
    @esid  sec-proxycreate *)
and proxy_create s target handler =
  if not (type_of target === Type_object) then
    run_error_no_c s Native_error_type
  else if is_proxy_exotic_object s (loc_of_value target)
       && unsome_error (unsome_error (run_object_method object_proxy_handler_ s (loc_of_value target))) === Value_null then
    run_error_no_c s Native_error_type
  else if not (type_of handler === Type_object) then
    run_error_no_c s Native_error_type
  else if is_proxy_exotic_object s (loc_of_value handler)
       && unsome_error (unsome_error (run_object_method object_proxy_handler_ s (loc_of_value handler))) === Value_null then
    run_error_no_c s Native_error_type
  else
  let s, p = proxy_object_new s in
  let%ret s =
  if is_callable s target then
    let%some_ret s = run_object_set_internal object_set_call s p Call_proxy in
    if object_has_internal_method s (loc_of_value target) object_construct_ then
      let%some_ret s = run_object_set_internal object_set_construct s p Construct_proxy in
      Continue s
    else Continue s
  else Continue s
  in
  let%some s = run_object_set_internal object_set_proxy_target s p target in
  let%some s = run_object_set_internal object_set_proxy_handler s p handler in
  res_ter s (res_val (Value_object p))

(** {1 Fundamental Objects}
    @essec 19
    @esid  sec-fundamental-objects *)
(** {2 Object Objects}
    @essec 19.1
    @esid  sec-object-objects *)
(** {3 Properties of the Object Contructor}
    @essec 19.1.2
    @esid  sec-properties-of-the-object-constructor *)

(** @esid sec-object.freeze
    @essec 19.1.2.6 *)
and builtin_object_freeze s c f this newTarget o =
  if not (type_of o === Type_object) then res_ter s (res_val o)
  else
  let%bool s, status = set_integrity_level s o "frozen" in
  if not status then run_error_no_c s Native_error_type
  else res_ter s (res_val o)

(** @esid sec-object.getprototypeof
    @essec 19.1.2.11 *)
and builtin_object_get_prototype_of s c f this newTarget o =
  let%object s, obj = to_object s o in
  object_internal_get_prototype_of s obj

(** @esid sec-object.isextensible
    @essec 19.1.2.13 *)
and builtin_object_is_extensible s c f this newTarget o =
  if not (type_of o === Type_object) then res_ter s (res_val (Value_bool false))
  else is_extensible s o

(** @esid sec-object.isfrozen
    @essec 19.1.2.14 *)
and builtin_object_is_frozen s c f this newTarget o =
  if not (type_of o === Type_object) then res_ter s (res_val (Value_bool true))
  else test_integrity_level s o "frozen"

(** @esid sec-object.issealed
    @essec 19.1.2.15 *)
and builtin_object_is_sealed s c f this newTarget o =
  if not (type_of o === Type_object) then res_ter s (res_val (Value_bool true))
  else test_integrity_level s o "sealed"

(** @esid sec-object.keys
    @essec 19.1.2.16 *)
and builtin_object_keys s c f this newTarget o =
  let%object (s, obj) = to_object s o in
  let%spec (s, nameList) = enumerable_own_properties s obj "key" in
  create_array_from_list s nameList

(** @esid sec-object.preventextensions
    @essec 19.1.2.17 *)
and builtin_object_prevent_extensions s c f this newTarget o =
  if not (type_of o === Type_object) then res_ter s (res_val o)
  else
  let%bool s, status = object_internal_prevent_extensions s (loc_of_value o) in
  if not status then run_error_no_c s Native_error_type
  else res_ter s (res_val o)

(** @esid sec-object.seal
    @essec 19.1.2.19 *)
and builtin_object_seal s c f this newTarget o =
  if not (type_of o === Type_object) then res_ter s (res_val o)
  else
  let%bool s, status = set_integrity_level s o "sealed" in
  if not status then run_error_no_c s Native_error_type
  else res_ter s (res_val o)

(** @esid sec-object.setprototypeof
    @essec 19.1.2.20 *)
and builtin_object_set_prototype_of s c f this newTarget o proto =
  let%value s, o = require_object_coercible s o in
  if not (type_of proto === Type_object || proto === Value_null) then run_error_no_c s Native_error_type
  else if not (type_of o === Type_object) then res_ter s (res_val o)
  else
  let%bool s, status = object_internal_set_prototype_of s (loc_of_value o) proto in
  if not status then run_error_no_c s Native_error_type
  else res_ter s (res_val o)

(** {3 Properties of the Object Prototype Object}
    @esid sec-properties-of-the-object-prototype-object
    @essec 19.1.3 *)

(** @esid sec-object.prototype.isprototypeof
    @essec 19.1.3.4 *)
and builtin_object_prototype_is_prototype_of s c f this newTarget v =
  if not (type_of v === Type_object) then res_out s (res_val (Value_bool false))
  else
  let v = loc_of_value v in
  let%value (s, o) = to_object s this in
  let%ret _ = repeat (fun _ -> true) s (fun s ->
    let%value_ret (s, v) = object_internal_get_prototype_of s v in
    if v === Value_null then Return (res_out s (res_val (Value_bool false)))
    else if same_value o v then Return (res_out s (res_val (Value_bool true)))
    else Continue s
  ) in
  Result_impossible (* The repeat function must terminate with a return. *)

(** {1 Reflection}
    @essec 26
    @esid sec-reflection *)
(** {2 The Reflect Object}
    @essec 26.1
    @esid sec-reflect-object *)
(** @essec 26.1.1
    @esid sec-reflect.apply *)
and builtin_reflect_apply s c f this newTarget target thisArgument argumentsList =
  if not (is_callable s target) then run_error s c Native_error_type else
  let%spec s, args = create_list_from_array_like s argumentsList None in
  (* TODO: prepare_for_tail_call goes here *)
  call s target thisArgument (Some args)

(** @essec 26.1.2
    @esid sec-reflect.construct

    Note: [_newTarget] is specified by 9.3.2 *)
and builtin_reflect_construct s c f this _newTarget target argumentsList newTarget =
  if not (is_constructor s target) then run_error s c Native_error_type else
  let%ret newTarget =
    if is_none newTarget then Continue target
    else let newTarget = unsome_error newTarget in
    if not (is_constructor s newTarget) then Return (run_error s c Native_error_type)
    else Continue newTarget in
  let%spec s, args = create_list_from_array_like s argumentsList None in
  construct s target (Some args) (Some newTarget)

(** @essec 26.1.3
    @esid sec-reflect.defineproperty *)
and builtin_reflect_define_property s c f this newTarget target propertyKey attributes =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%value s, key = to_property_key s propertyKey in
  let%spec s, desc = to_property_descriptor s attributes in
  object_internal_define_own_property s (loc_of_value target) key desc

(** @essec 26.1.4
    @esid sec-reflect.deleteproperty *)
and builtin_reflect_delete_property s c f this newTarget target propertyKey =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%value s, key = to_property_key s propertyKey in
  object_internal_delete s (loc_of_value target) key

(** @essec 26.1.5
    @esid sec-reflect.get *)
and builtin_reflect_get s c f this newTarget target propertyKey receiver =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%value s, key = to_property_key s propertyKey in
  let receiver = unsome_default target receiver in
  object_internal_get s (loc_of_value target) key receiver

(** @essec 26.1.6
    @esid sec-reflect.getownpropertydescriptor *)
and builtin_reflect_get_own_property_descriptor s c f this newTarget target propertyKey =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%value s, key = to_property_key s propertyKey in
  let%spec s, desc = object_internal_get_own_property s (loc_of_value target) key in
  from_property_descriptor s desc

(** @essec 26.1.7
    @esid sec-reflect.getprototypeof *)
and builtin_reflect_get_prototype_of s c f this newTarget target =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  object_internal_get_prototype_of s (loc_of_value target)

(** @essec 26.1.8
    @esid sec-reflect.has *)
and builtin_reflect_has s c f this newTarget target propertyKey =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%value s, key = to_property_key s propertyKey in
  object_internal_has_property s (loc_of_value target) key

(** @essec 26.1.9
    @esid sec-reflect.isextensible *)
and builtin_reflect_is_extensible s c f this newTarget target =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  object_internal_is_extensible s (loc_of_value target)

(** @essec 26.1.10
    @esid sec-reflect.ownkeys *)
and builtin_reflect_own_keys s c f this newTarget target =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%spec s, keys = object_internal_own_property_keys s (loc_of_value target) in
  create_array_from_list s keys

(** @essec 26.1.11
    @esid sec-reflect.preventextensions *)
and builtin_reflect_prevent_extensions s c f this newTarget target =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  object_internal_prevent_extensions s (loc_of_value target)

(** @essec 26.1.12
    @esid sec-reflect.set *)
and builtin_reflect_set s c f this newTarget target propertyKey v receiver =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  let%value s, key = to_property_key s propertyKey in
  let receiver = unsome_default target receiver in
  object_internal_set s (loc_of_value target) key v receiver

(** @essec 26.1.13
    @esid sec-reflect.setprototypeof *)
and builtin_reflect_set_prototype_of s c f this newTarget target proto =
  if not (type_of target === Type_object) then run_error s c Native_error_type else
  if (not (type_of proto === Type_object)) && (not (proto === Value_null)) then run_error s c Native_error_type else
  object_internal_set_prototype_of s (loc_of_value target) proto

(** {2 Proxy Objects}
    @essec 26.2
    @esid sec-proxy-objects *)
(** {3 The Proxy Constructor}

    The [%Proxy%] intrinsic object, initial value of the [Proxy] property of the global object.

    @essec 26.2.1
    @esid sec-proxy-constructor *)
(** @essec 26.2.1.1
    @esid sec-proxy-target-handler *)
and builtin_proxy_constructor s c f this newTarget target handler =
  if newTarget === Value_undef then
    run_error_no_c s Native_error_type
  else
    proxy_create s target handler

(** {3 Properties of the Proxy Constructor}
    @essec 26.2.2
    @esid sec-properties-of-the-proxy-constructor *)
(** [%Proxy%.[[Prototype]] = %FunctionPrototype%] *)

(** @essec 26.2.2.1
    @esid sec-proxy.revocable *)
and builtin_proxy_revocable s c f this newTarget target handler =
  let%value s, p = proxy_create s target handler in
  let steps = Builtin_proxy_revocation in
  let%object s, revoker = create_builtin_function s steps (* [ [[RevocableProxy]] ] *) [] None None 0. false in
  let%some s = run_object_set_internal object_set_revocable_proxy s revoker p in
  let%value s, result = object_create s (Value_object (Object_loc_prealloc Prealloc_object_proto)) None in
  let%bool s, _ = create_data_property s result (Value_string "proxy") p in
  let%bool s, _ = create_data_property s result (Value_string "revoke") (Value_object revoker) in
  res_ter s (res_val result)

(** @essec 26.2.2.1.1
    @esid sec-proxy-revocation-functions *)
and builtin_proxy_revocation_function s c f this newTarget =
  let%some p = run_object_method object_revocable_proxy_ s f in
  let%some p = p in
  if p === Value_null then res_ter s (res_val Value_undef)
  else
  let p = loc_of_value p in
  let%some s = run_object_set_internal object_set_revocable_proxy s f Value_null in
  let%assert _ = is_proxy_exotic_object s p in
  let%some s = run_object_set_internal object_set_proxy_target s p Value_null in
  let%some s = run_object_set_internal object_set_proxy_handler s p Value_null in
  res_ter s (res_val Value_undef)

(******** UNCHECKED ES5 IMPLEMENTATION CONTINUES BELOW ***********)

(** @deprecated Compatibility wrapper for ES5 code to ES6 implementation use [has_property] instead. TODO: Remove call
    sites. *)
and object_has_prop s c l x =
  object_internal_has_property s l (Value_string x)


(** val out_error_or_void :
    state -> strictness_flag -> native_error -> result **)

and out_error_or_void s c str ne =
  if str then run_error s c ne else res_void s

(** val out_error_or_cst :
    state -> strictness_flag -> native_error -> value -> result **)

and out_error_or_cst s c str ne v =
  if str then run_error s c ne else res_out s (res_val v)

(**
    @deprecated TODO: ES6 version of function. dispatched to in {!object_internal_get}.
*)
and object_get_builtin_args s b vthis l x =
  let x = string_of_value x in
  let c = some_context in
  let def s0 l0 =
    let%spec (s1, d) = (run_object_get_prop s0 c l0 x) in
    match d with
    | Full_descriptor_undef ->
      res_ter s1 (res_val Value_undef)
    | Full_descriptor_some a ->
      (match a with
       | Attributes_data_of ad ->
         res_ter s1 (res_val ad.attributes_data_value)
       | Attributes_accessor_of aa ->
         (match aa.attributes_accessor_get with
          | Value_undef ->
            res_ter s1 (res_val Value_undef)
          | Value_null -> Result_impossible
          | Value_bool b0 -> Result_impossible
          | Value_number n -> Result_impossible
          | Value_string s2 -> Result_impossible
          | Value_object lf -> run_call s1 c lf vthis [])) in
  let function0 s0 =
    let%value (s_2, v) = (def s0 l) in
    if spec_function_get_error_case_dec s_2 x v
    then run_error s_2 c Native_error_type
    else res_ter s_2 (res_val v) in
  match b with
  | Builtin_get_args_obj -> (
    let%some lmapo = (run_object_method object_parameter_map_ s l) in
    let%some lmap = (lmapo) in
    let%spec (s0, d) = (run_object_get_own_prop s c lmap x) in
    match d with
    | Full_descriptor_undef -> function0 s0
    | Full_descriptor_some a -> run_object_get s0 c lmap x)
  | _ -> Result_impossible (* Should have been caught by [object_internal_get] *)

(** val run_object_get :
    state -> execution_ctx -> object_loc -> prop_name -> result

    @deprecated This is the ES5 version, replaced by {!object_internal_get}
    TODO: Replace calls to this and delete.
    **)
and run_object_get s c l x =
  object_internal_get s l (Value_string x) (Value_object l)

(** val run_object_get_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

and run_object_get_prop s c l x =
  let%some b = (run_object_method object_get_prop_ s l) in
  match b with Builtin_get_prop_default ->
    let%spec (s1, d) = (run_object_get_own_prop s c l x) in
    if full_descriptor_compare d Full_descriptor_undef
    then let%some proto = (run_object_method object_proto_ s1 l) in
      match proto with
      | Value_null -> res_spec s1 Full_descriptor_undef
      | Value_object lproto ->
        run_object_get_prop s1 c lproto x
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s1
          ("Found a non-object or null value as a prototype in [run_object_get_prop].")
    else res_spec s1 d

(** val object_default_value :
    state -> execution_ctx -> object_loc -> preftype option ->
    result **)

and object_default_value s c l prefo =
  let%some b = (run_object_method object_default_value_ s l) in
  match b with Builtin_default_value_default ->
    let gpref = unsome_default Preftype_number prefo in
    let lpref = other_preftypes gpref in
    let sub0 s_2 x k =
      let%value (s1, vfo) = (run_object_get s_2 c l x) in
      let%some co = (run_callable s1 vfo) in
      match co with
      | Some b0 ->
        let%object (s2, lfunc) = (res_out s1 (res_val vfo)) in
        let%value (s3, v) = (run_call s2 c lfunc (Value_object l) []) in begin
          match v with
          | Value_object l0 -> k s3
          | _ -> res_out s3 (res_val v)
        end
      | None -> k s1 in
    let gmeth = (method_of_preftype gpref) in
    sub0 s gmeth (fun s_2 ->
        let lmeth = method_of_preftype lpref in
        sub0 s_2 lmeth (fun s_3 -> run_error s_3 c Native_error_type))

(** val to_int32 :
    state -> execution_ctx -> value -> float specres **)

and to_int32 s c v =
  let%number (s_2, n) = to_number s v in res_spec s_2 (JsNumber.to_int32 n)

(** val to_uint32 :
    state -> execution_ctx -> value -> float specres **)

and to_uint32 s c v =
  let%number (s_2, n) = to_number s v in res_spec s_2 (JsNumber.to_uint32 n)

(** val run_object_define_own_prop_array_loop :
    state -> execution_ctx -> object_loc -> float -> float ->
    descriptor -> bool -> bool -> (state -> prop_name -> descriptor ->
    strictness_flag -> __ specres) -> result **)

and run_object_define_own_prop_array_loop s c l newLen oldLen newLenDesc newWritable throwcont def =
  if newLen < oldLen
  then let oldLen_2 = (oldLen -. 1.) in
    let%string (s0, slen) = (to_string s (Value_number oldLen_2)) in
    let%bool (s1, deleteSucceeded) = (object_delete s0 c l slen false) in
    if not deleteSucceeded
    then let newLenDesc0 =
           (descriptor_with_value
              newLenDesc
              (Some (Value_number (oldLen_2 +. 1.)))) in
      let newLenDesc1 = (if not newWritable
                         then descriptor_with_writable newLenDesc0 (Some false)
                         else newLenDesc0) in
      let%bool (s2, x) = (def s1 ("length")
                            newLenDesc1 false) in
      out_error_or_cst s2 c throwcont Native_error_type
        (Value_bool false)
    else run_object_define_own_prop_array_loop s1 c l
        newLen oldLen_2 newLenDesc newWritable throwcont def
  else if not newWritable
  then def s ("length")
      { descriptor_value = None; descriptor_writable = (Some false);
        descriptor_get = None; descriptor_set = None;
        descriptor_enumerable = None; descriptor_configurable = None }
      false
  else res_ter s (res_val (Value_bool true))

(** ES5 [[DefineOwnProperty]]
    @deprecated Use ES6 object_internal_define_own_property instead. TODO: ES6 version. *)
and object_define_own_prop s c l x desc throwcont =
  let reject s0 throwcont0 =
    out_error_or_cst
      s0 c throwcont0 Native_error_type (Value_bool false) in
  let def s p d _ = ordinary_define_own_property s l (Value_string p) d in
  let%some b = (run_object_method object_define_own_prop_ s l) in
  match b with
  | Builtin_define_own_prop_array ->
    let%spec (s0, d) = (run_object_get_own_prop s c l ("length")) in
    begin
      match d with
      | Full_descriptor_undef ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s0
          ("Array length property descriptor cannot be undefined.")
      | Full_descriptor_some attr ->
        (match attr with
         | Attributes_data_of a ->
           let  oldLen = (a.attributes_data_value) in begin
             match oldLen with
             | Value_object l0 ->
               (Debug.impossible_with_heap_because __LOC__ s0
                  "Spec asserts length of array is number.";
                Result_impossible)
             | _ ->
               let%number s0, oldLen = to_number s0 oldLen in
               let oldLen0 = (JsNumber.to_uint32 oldLen) in
               let descValueOpt = (desc.descriptor_value) in
               if string_eq x ("length")
               then (match descValueOpt with
                   | Some descValue ->
                     let%spec (s1, newLen) = (to_uint32 s0 c descValue) in
                     let%number (s2, newLenN) = to_number s1 descValue in
                     if not (newLen === newLenN)
                     then run_error s2 c Native_error_range
                     else let newLenDesc =
                            (descriptor_with_value desc (Some (Value_number newLen))) in
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
                         then res_ter s3 (res_val (Value_bool false))
                         else run_object_define_own_prop_array_loop s3 c l newLen oldLen0 newLenDesc0 newWritable throwcont def
                   | None -> def s0 ("length") desc throwcont)
               else let%spec (s1, ilen) = (to_uint32 s0 c (Value_string x)) in
                 let%string (s2, slen) = (to_string s1 (Value_number ilen)) in
                 if (string_eq x slen) && (not ( ilen = 4294967295.))
                 then let%spec (s3, index) = (to_uint32 s2 c (Value_string x)) in
                   if  (le_int_decidable oldLen0 index) && (not a.attributes_data_writable)
                   then reject s3 throwcont
                   else let%bool (s4, b0) = (def s3 x desc false) in
                     if not b0
                     then reject s4 throwcont
                     else if le_int_decidable oldLen0 index
                     then let a0 =
                            descriptor_with_value (descriptor_of_attributes (Attributes_data_of a))
                              (Some (Value_number (index +. 1.))) in
                       def s4 ("length") a0 false
                     else res_ter s4 (res_val (Value_bool true))
                 else def s2 x desc throwcont
           end
         | Attributes_accessor_of a ->
           (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
             s0
             ("Array length property descriptor cannot be accessor."))
    end
  | Builtin_define_own_prop_args_obj ->
    let%some lmapo = (run_object_method object_parameter_map_ s l) in
    let%some lmap = (lmapo) in
    let%spec (s0, d) = (run_object_get_own_prop s c lmap x) in
    let%bool (s1, b0) = (def s0 x desc false) in
    if b0
    then let follow s2 = res_ter s2 (res_val (Value_bool true)) in
      match d with
      | Full_descriptor_undef -> follow s1
      | Full_descriptor_some a ->
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
  | _ ->
    let%bool (s, b) = object_internal_define_own_property s l (Value_string x) desc (* ES6 hack *)
    in if throwcont && not b then
      run_error_no_c s Native_error_type
    else
      res_ter s (res_val (Value_bool b))

(** val prim_new_object : state -> prim -> result **)

and prim_new_object s _foo_ = match _foo_ with
  | Value_bool b ->
    let o1 = (object_new (Value_object (Object_loc_prealloc Prealloc_bool_proto)) ("Boolean")) in
    let o = (object_with_primitive_value o1 (Value_bool b)) in
    let (l, s1) = object_alloc s o in
    res_out s1 (res_val (Value_object l))
  | Value_number n ->
    let o1 = (object_new (Value_object (Object_loc_prealloc Prealloc_number_proto)) ("Number")) in
     let o = (object_with_primitive_value o1 (Value_number n)) in
     let (l, s1) = object_alloc s o in
     res_out s1 (res_val (Value_object l))
  | Value_string s0 ->
    let o2 = (object_new (Value_object (Object_loc_prealloc Prealloc_string_proto)) ("String")) in
    let o1 = (object_with_get_own_property o2 Builtin_get_own_prop_string) in
    let o = (object_with_primitive_value o1 (Value_string s0)) in
    let (l, s1) = object_alloc s o in
    let%some s_2 = (run_object_heap_map_properties s1 l
      (fun p -> HeapStr.write p ("length")
         (Attributes_data_of (attributes_data_intro_constant
         (Value_number (number_of_int (strlength s0))))))) in
    res_ter s_2 (res_val (Value_object l))
  | _ ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
      s
      ("[prim_new_object] received an null or undef.")

(** val to_object : state -> value -> result **)

and to_object s _foo_ = match _foo_ with
  | Value_undef -> run_error_no_c s Native_error_type
  | Value_null -> run_error_no_c s Native_error_type
  | Value_bool b -> prim_new_object s _foo_
  | Value_number n -> prim_new_object s _foo_
  | Value_string s0 -> prim_new_object s _foo_
  | Value_object l ->
    res_out s (res_val (Value_object l))

(** val run_object_prim_value : state -> object_loc -> result **)

and run_object_prim_value s l =
  let%some ov = (run_object_method object_prim_value_ s l) in
      let%some v = (ov) in  res_ter s (res_val v)

(** val env_record_has_binding :
    state -> execution_ctx -> env_loc -> prop_name -> result **)

and env_record_has_binding s c l x =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Env_record_decl ed ->
        res_out s (res_val (Value_bool (HeapStr.indom_dec ed x)))
      | Env_record_object (l0, pt) -> object_has_prop s c l0 x

(** val lexical_env_get_identifier_ref :
    state -> execution_ctx -> lexical_env -> prop_name ->
    strictness_flag -> ref specres **)

and lexical_env_get_identifier_ref s c x x0 str =
  match x with
  | [] ->
    res_spec s (ref_create_value Value_undef x0 str)
  | l :: x_2 ->
    let%bool (s1, has) = (env_record_has_binding s c l x0) in
        if has
        then res_spec s1 (ref_create_env_loc l x0 str)
        else lexical_env_get_identifier_ref s1 c x_2 x0 str

(** @deprecated: ES5 implementation for arguments object. Others redirect to ES2016 implementation.
    TODO: Switch out to ES2016 version. *)
and object_delete s c l x str =
  let%some b = (run_object_method object_delete_ s l) in
      match b with
      | Builtin_delete_args_obj ->
        begin
          let%some mo = (run_object_method object_parameter_map_ s l) in
          let%some m = (mo) in
          let%spec (s1, d) = (run_object_get_own_prop s c m x) in
          let%bool (s2, b0) = (object_delete_default s1 l x str) in
          if b0 then (match d with
            | Full_descriptor_undef ->
              res_ter s2 (res_val (Value_bool b0))
            | Full_descriptor_some a ->
              let%bool (s3, b_2) = (object_delete s2 c m x false) in
              res_ter s3 (res_val (Value_bool b0)))
          else res_ter s2 (res_val (Value_bool b0))
        end
      | _ ->
        let%bool s, b = object_internal_delete s l (Value_string x) in
        if str && (not b) then
          run_error_no_c s Native_error_type
        else
          res_ter s (res_val (Value_bool b))

(** @deprecated TODO: Replace ES5 callsites with ES2016 version. *)
and object_delete_default s l x str =
  let%bool s, b = ordinary_object_internal_delete s l (Value_string x) in
  if str && (not b) then
    run_error_no_c s Native_error_type
  else
    res_ter s (res_val (Value_bool b))


(** val env_record_delete_binding :
    state -> execution_ctx -> env_loc -> prop_name -> result **)

and env_record_delete_binding s c l x =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Env_record_decl ed ->
        (match HeapStr.read_option ed x with
         | Some p ->
           let (mu, v) = p in
           (match mu with
            | Mutability_uninitialized_immutable ->
              res_out s (res_val (Value_bool false))
            | Mutability_immutable ->
              res_out s (res_val (Value_bool false))
            | Mutability_nondeletable ->
              res_out s (res_val (Value_bool false))
            | Mutability_deletable ->
              let s_2 =
                env_record_write s l (Env_record_decl (decl_env_record_rem ed x))
              in
              res_out s_2 (res_val (Value_bool true)))
         | None ->
           res_out s (res_val (Value_bool true)))
      | Env_record_object (l0, pt) ->
        object_delete s c l0 x throw_false

(** val env_record_implicit_this_value : state -> env_loc -> value option **)

and env_record_implicit_this_value s l =
  ifx_some_or_default (env_record_binds_option s l) None (fun e ->
      Some
        (match e with
         | Env_record_decl ed -> Value_undef
         | Env_record_object (l0, provide_this) ->
           if provide_this
           then Value_object l0
           else Value_undef))

(** val identifier_resolution :
    state -> execution_ctx -> prop_name -> ref specres **)

and identifier_resolution s c x =
  let x0 = c.execution_ctx_lexical_env in
  let str = c.execution_ctx_strict in
  lexical_env_get_identifier_ref s c x0 x str

(** @deprecated ES5 *)
and env_record_get_binding_value s c l x str =
  get_binding_value s l (Value_string x) str

(** @deprecated ES5 *)
and ref_get_value s c _foo_ =
  let%value s, v = get_value s (res_ter s (res_normal _foo_)) in
  res_spec s v

(** @deprecated ES5 *)
and run_expr_get_value s c _term_ =
  let result = run_expr s c _term_ in
  let%ter s, _ = result in
  let%value s, v = get_value s result in
  res_spec s v

(** val env_record_set_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> value ->
    strictness_flag -> result_void **)

and env_record_set_mutable_binding s c l x v str =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Env_record_decl ed ->
        let%some rm = (HeapStr.read_option ed x) in
            let (mu, v_old) = rm in
            if not (mutability_compare mu Mutability_immutable)
            then res_void (env_record_write_decl_env s l x mu v)
            else out_error_or_void s c str Native_error_type
      | Env_record_object (l0, pt) ->
        let%success s, _ = object_put s c l0 x v str in
        res_void s

(** @deprecated ES5 *)
and ref_put_value s c rv v =
  let%success s, _ = put_value s c (res_ter s (res_normal rv)) (res_ter s (res_val v))
  in res_void s

(** val env_record_create_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> bool
    option -> result_void **)

and env_record_create_mutable_binding s c l x deletable_opt =
  let  deletable = (unsome_default false deletable_opt) in
      let%some e = (env_record_binds_option s l) in
          match e with
          | Env_record_decl ed ->
            if HeapStr.indom_dec ed x
            then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                s
                ("Already declared environnment record in [env_record_create_mutable_binding].")
            else let s_2 = (env_record_write_decl_env s l x
                              (mutability_of_bool deletable) Value_undef) in
              res_void s_2
          | Env_record_object (l0, pt) ->
            let%bool (s1, has) = (object_has_prop s c l0 x) in
              if has
              then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                    s1
                    ("Already declared binding in [env_record_create_mutable_binding].")
              else let  a = ({ attributes_data_value = Value_undef; attributes_data_writable = true;
                                   attributes_data_enumerable = true;
                                   attributes_data_configurable = deletable }) in
                    let%success
                       (s2, rv) = (object_define_own_prop s1 c l0 x
                         (descriptor_of_attributes (Attributes_data_of a))
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
      | Env_record_decl ed ->
        if HeapStr.indom_dec ed x
        then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
            s
            ("Already declared environnment record in [env_record_create_immutable_binding].")
        else res_void
            (env_record_write_decl_env s l x
               Mutability_uninitialized_immutable Value_undef)
      | Env_record_object (o, p) ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s
          ("[env_record_create_immutable_binding] received an environnment record object.")

(** val env_record_initialize_immutable_binding :
    state -> env_loc -> prop_name -> value -> result_void **)

and env_record_initialize_immutable_binding s l x v =
  let%some e = (env_record_binds_option s l) in
      match e with
      | Env_record_decl ed ->
        let%some evs = (decl_env_record_option ed x) in
            if prod_compare mutability_compare value_compare evs
                (Mutability_uninitialized_immutable, Value_undef)
            then let s_2 = (env_record_write_decl_env s l x Mutability_immutable v) in res_void s_2
            else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                s
                ("Non suitable binding in [env_record_initialize_immutable_binding].")
      | Env_record_object (o, p) ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s
          ("[env_record_initialize_immutable_binding] received an environnment record object.")

(** val call_object_new : state -> value -> result **)

and call_object_new s c v =
  match type_of v with
  | Type_undef ->
    let o = (object_new (Value_object (Object_loc_prealloc Prealloc_object_proto))
      ("Object")) in
    let  p = object_alloc s o in
    let (l, s_2) = p in
    res_out s_2 (res_val (Value_object l))
  | Type_null ->
    let o = (object_new (Value_object (Object_loc_prealloc Prealloc_object_proto))
      ("Object")) in
    let  p = object_alloc s o in
    let (l, s_2) = p in
    res_out s_2 (res_val (Value_object l))
  | Type_bool -> to_object s v
  | Type_number -> to_object s v
  | Type_string -> to_object s v
  | Type_object -> res_out s (res_val v)

(** val array_args_map_loop :
    state -> execution_ctx -> object_loc -> value list -> float
    -> result_void **)

and array_args_map_loop s c l args ind =
  match args with
  | [] -> res_void s
  | h :: rest ->
    let%some s_2 = (run_object_heap_map_properties s l (fun p ->
           HeapStr.write p (JsNumber.to_string ind)
             (Attributes_data_of (attributes_data_intro_all_true h)))) in
             array_args_map_loop s_2 c l rest (ind +. 1.)

(** val run_construct_prealloc :
    state -> execution_ctx -> prealloc -> value list -> result **)

and run_construct_prealloc s c b args =
  match b with
  | Prealloc_object ->
    let v = (get_arg 0 args) in call_object_new s c v
  | Prealloc_bool ->
    let v = get_arg 0 args in
    let b0 = to_boolean v in
    let o1 = object_new (Value_object (Object_loc_prealloc Prealloc_bool_proto))
      ("Boolean") in
    let o = object_with_primitive_value o1 (Value_bool b0) in
    let  p = object_alloc s o in
    let (l, s_2) = p in
    res_out s_2 (res_val (Value_object l))
  | Prealloc_number ->
    let follow = (fun s_2 v ->
      let o1 = (object_new (Value_object (Object_loc_prealloc Prealloc_number_proto))
        ("Number")) in
      let o = object_with_primitive_value o1 v in
      let (l, s1) = object_alloc s_2 o in
      res_out s1 (res_val (Value_object l))) in
    if list_eq_nil_decidable args
    then follow s (Value_number JsNumber.zero)
    else
      let v = get_arg 0 args in
      let%number (x, x0) = (to_number s v) in
      follow x (Value_number x0)
  | Prealloc_array ->
    let o_2 = (object_new (Value_object (Object_loc_prealloc Prealloc_array_proto))
      ("Array")) in
    let o = (object_for_array o_2 Builtin_define_own_prop_array) in
    let p = (object_alloc s o) in
    let (l, s_2) = p in
    let follow = (fun s_3 length0 ->
      let%some s0 = (run_object_heap_map_properties s_3 l (fun p0 ->
        HeapStr.write p0 ("length") (Attributes_data_of
          { attributes_data_value = (Value_number length0);
            attributes_data_writable = true;
            attributes_data_enumerable = false;
            attributes_data_configurable = false }))) in
      res_ter s0 (res_val (Value_object l))) in
    let arg_len = (LibList.length args) in
    if nat_eq arg_len 1
    then let v = get_arg 0 args in
    match v with
    | Value_undef ->
      let%some s0 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s0 1.0
    | Value_null ->
      let%some s0 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s0 1.0
    | Value_bool b0 ->
      let%some s0 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s0 1.0
    | Value_number vlen ->
      let%spec (s0, ilen) = (to_uint32 s_2 c (Value_number vlen)) in
      if ilen === vlen
      then follow s0 ilen
      else run_error s0 c Native_error_range
    | Value_string s0 ->
      let%some s1 = (run_object_heap_map_properties s_2 l
        (fun p1 -> HeapStr.write p1 ("0") (Attributes_data_of (attributes_data_intro_all_true v)))) in
      follow s1 1.0
    | Value_object o0 ->
      let%some
         s0 = (run_object_heap_map_properties s_2 l
           (fun p0 ->
              HeapStr.write p0 ("0") (Attributes_data_of
                                     (attributes_data_intro_all_true v)))) in
            follow s0 1.0
                          else let%some
                               s0 = (run_object_heap_map_properties s_2 l
                                 (fun p0 ->
                                    HeapStr.write p0
                                      ("length")
                                      (Attributes_data_of { attributes_data_value =
                                                                  (Value_number (number_of_int arg_len));
                                                                attributes_data_writable = true;
                                                                attributes_data_enumerable = false;
                                                                attributes_data_configurable = false }))) in
                                  let%void
                                    s1 = (array_args_map_loop s0 c l args 0.) in  res_ter s1 (res_val (Value_object l))
  | Prealloc_string ->
    let
       o2 = (object_new (Value_object (Object_loc_prealloc
                                       Prealloc_string_proto))
         ("String")) in
          let

            o1 = (object_with_get_own_property o2 Builtin_get_own_prop_string) in
               let  follow = (fun s0 s1 ->
                   let
                      o = (object_with_primitive_value o1 (Value_string s1)) in
                         let (l, s2) = object_alloc s0 o in
                         let lenDesc = (attributes_data_intro_constant (Value_number (number_of_int (strlength s1)))) in
                              let%some
                                 s_2 = (run_object_heap_map_properties s2 l (fun p ->
                                     HeapStr.write p ("length")
                                       (Attributes_data_of lenDesc))) in
                                    res_ter s_2 (res_val (Value_object l))) in
                   let  arg_len = (LibList.length args) in
                       if nat_eq arg_len 0
                       then follow s ""
                       else let  arg = (get_arg 0 args) in
                           let%string (s0, s1) = (to_string s arg) in
                               follow s0 s1
  | Prealloc_error ->
    let  v = (get_arg 0 args) in
        build_error s c (Value_object (Object_loc_prealloc
                                           Prealloc_error_proto)) v
  | Prealloc_native_error ne ->
    let  v = (get_arg 0 args) in
        build_error s c (Value_object (Object_loc_prealloc
                                           (Prealloc_native_error_proto ne))) v
  | Prealloc_proxy -> builtin_proxy_constructor s c () () (Value_object (Object_loc_prealloc
                                                                                   Prealloc_proxy)) (get_arg 0 args) (get_arg 1 args)
  | _ ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Result_not_yet_implemented)
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
          vproto = (if type_compare (type_of v1) Type_object
          then v1
          else Value_object (Object_loc_prealloc
                                   Prealloc_object_proto)) in
             let

               o = (object_new vproto ("Object")) in
                  let  p = (object_alloc s1 o) in
                      let (l_2, s2) = p in
                      let%value
                        (s3, v2) = (run_call s2 c l (Value_object l_2) args) in
                           let
                              vr = (if type_compare (type_of v2) Type_object
                              then v2
                              else Value_object l_2) in res_ter s3 (res_val vr)

(** val run_construct :
    state -> execution_ctx -> construct -> object_loc -> value
    list -> result **)

and run_construct s c co l args =
  match co with
  | Construct_default -> run_construct_default s c l args
  | Construct_after_bind ->
    let%some otrg = run_object_method object_target_function_ s l in
    let%some target = (otrg) in
    let%some oco = run_object_method object_construct_ s target in begin
      match oco with
      | Some co0 ->
        let%some oarg = run_object_method object_bound_args_ s l in
        let%some boundArgs = oarg in
        let  arguments_ = (LibList.append boundArgs args) in
            run_construct s c co0 target arguments_
      | None -> run_error s c Native_error_type
    end
  | Construct_prealloc b -> run_construct_prealloc s c b args
  | _ -> construct s (Value_object l) (Some args) None

(** val run_call_default :
    state -> execution_ctx -> object_loc -> result **)

and run_call_default s c lf =
  let def = res_out s (res_val Value_undef) in
  let%some oC = (run_object_method object_code_ s lf) in
  match oC with
  | Some bd ->
    if list_eq_nil_decidable (prog_elements (funcbody_prog bd))
    then def
    else ifx_success_or_return (run_prog s c (funcbody_prog bd))
      (fun s_2 -> res_out s_2 (res_val Value_undef))
      (fun s_2 rv -> res_out s_2 (res_normal rv))
  | None -> def

(** val creating_function_object_proto :
    state -> execution_ctx -> object_loc -> result **)

and creating_function_object_proto s c l =
  let%object
    (s1, lproto) = (run_construct_prealloc s c Prealloc_object []) in
       let  a1 = ({ attributes_data_value = (Value_object l);
                     attributes_data_writable = true; attributes_data_enumerable = false;
                     attributes_data_configurable = true }) in
           let%bool

             (s2, b) = (object_define_own_prop s1 c lproto
                ("constructor")
                (descriptor_of_attributes (Attributes_data_of a1)) false) in
                let  a2 = ({ attributes_data_value = (Value_object lproto);
                              attributes_data_writable = true; attributes_data_enumerable =
                                                                 false; attributes_data_configurable = false }) in
                    object_define_own_prop s2 c l
                      ("prototype")
                      (descriptor_of_attributes (Attributes_data_of a2)) false

(** val creating_function_object :
    state -> execution_ctx -> string list -> funcbody ->
    lexical_env -> strictness_flag -> result **)

and creating_function_object s c names bd x str =
  let
     o = (object_new (Value_object (Object_loc_prealloc
                                     Prealloc_function_proto))
       ("Function")) in
            let o2 = (object_with_invokation o (Some Construct_default) (Some
                                                                         Call_default) (Some Builtin_has_instance_function)) in
             let o3 = (object_with_details o2 (Some x) (Some names) (Some bd) None None None None) in
             let p = (object_alloc s o3) in
             let (l, s1) = p in
             let a1 = ({
               attributes_data_value = (Value_number (number_of_int (LibList.length names)));
               attributes_data_writable = false; attributes_data_enumerable = false;
               attributes_data_configurable = false
             }) in
             let%bool (s2, b2) = (object_define_own_prop s1 c l ("length")
               (descriptor_of_attributes (Attributes_data_of a1)) false) in
             let%bool (s3, b3) = (creating_function_object_proto s2 c l) in
             if not str
             then res_ter s3 (res_val (Value_object l))
             else
               let vthrower = (Value_object (Object_loc_prealloc Prealloc_throw_type_error)) in
               let a2 = ({ attributes_accessor_get = vthrower;
                           attributes_accessor_set = vthrower;
                           attributes_accessor_enumerable = false;
                           attributes_accessor_configurable = false }) in
               let%bool (s4, b4) = (object_define_own_prop s3 c l ("caller")
                 (descriptor_of_attributes (Attributes_accessor_of a2)) false) in
               let%bool (s5, b5) = (object_define_own_prop s4 c l ("arguments")
                 (descriptor_of_attributes (Attributes_accessor_of a2)) false) in
               res_ter s5 (res_val (Value_object l))

(** val binding_inst_formal_params :
    state -> execution_ctx -> env_loc -> value list -> string
    list -> strictness_flag -> result_void **)

and binding_inst_formal_params s c l args names str =
  match names with
  | [] -> res_void s
  | argname :: names_2 ->
    let  v = (hd Value_undef args) in
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
                                   (Value_object fo) str) in
                                    binding_inst_function_decls s3 c l fds_2 str bconfig) in
                               let%bool
                                 (s2, has) = (env_record_has_binding s1 c l fname) in
                                    if has
                                    then if nat_eq l env_loc_global_env_record
                                      then let%spec
                                           (s3, d) = (run_object_get_prop s2 c
                                             (Object_loc_prealloc Prealloc_global)
                                             fname) in
                                              match d with
                                              | Full_descriptor_undef ->
                                                (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                                                  s3
                                                  ("Undefined full descriptor in [binding_inst_function_decls].")
                                              | Full_descriptor_some a ->
                                                if attributes_configurable a
                                                then let  a_2 = ({ attributes_data_value =
                                                                     Value_undef;
                                                                   attributes_data_writable = true;
                                                                   attributes_data_enumerable = true;
                                                                   attributes_data_configurable =
                                                                     bconfig }) in
                                                    let%bool
                                                       (s0, x) = (object_define_own_prop s3 c
                                                         (Object_loc_prealloc
                                                            Prealloc_global) fname
                                                         (descriptor_of_attributes
                                                            (Attributes_data_of a_2))
                                                         true) in follow s0
                                                else if
                                                       (is_accessor_descriptor (Descriptor (descriptor_of_attributes a)))
                                                    || (not (attributes_writable a))
                                                    || (not (attributes_enumerable a))
                                                then run_error s3 c Native_error_type
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
  let bd = Funcbody_intro ((Prog_intro (true, ((Element_stat
                                                          (Stat_return (Some (Expr_identifier x)))) :: []))), xbd)
  in
  creating_function_object s c [] bd x0 true

(** val make_arg_setter :
    state -> execution_ctx -> prop_name -> lexical_env -> result **)

and make_arg_setter s c x x0 =
  let xparam = strappend x ("_arg") in
  let xbd =
    strappend x (strappend (" = ") (strappend xparam ";"))
  in
  let bd = Funcbody_intro ((Prog_intro (true, ((Element_stat
                                                          (Stat_expr (Expr_assign ((Expr_identifier x), None,
                                                                                           (Expr_identifier xparam))))) :: []))), xbd)
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
              o_2 = (object_for_args_object o lmap Builtin_get_args_obj
                Builtin_get_own_prop_args_obj
                Builtin_define_own_prop_args_obj
                Builtin_delete_args_obj) in
                 res_void (object_write s l o_2))
    (fun len_2 ->
       let  tdl = (take_drop_last args) in
           let (rmlargs, largs) = tdl in
           let  arguments_object_map_loop_2 = (fun s0 xsmap0 ->
               arguments_object_map_loop s0 c l xs len_2 rmlargs x str lmap
                 xsmap0) in
               let  a = (attributes_data_intro_all_true largs) in
                  let%string s, tempVar = to_string s (Value_number (number_of_int len_2)) in
                   let%bool

                     (s1, b) = (object_define_own_prop s c l tempVar
                        (descriptor_of_attributes (Attributes_data_of a)) false) in
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
                                                          (Value_object lgetter);
                                                        attributes_accessor_set = (Value_object
                                                                                     lsetter); attributes_accessor_enumerable =
                                                                                                 false; attributes_accessor_configurable =
                                                                                                          true }) in
                                let%string s3, tempVar = to_string s3 (Value_number (number_of_int len_2)) in
                                              let%bool

                                                (s4, b_2) = (object_define_own_prop s3 c lmap
                                                   tempVar
                                                   (descriptor_of_attributes
                                                      (Attributes_accessor_of a_2)) false) in
                                                   arguments_object_map_loop_2 s4 (x0 :: xsmap))
    len

(** val arguments_object_map :
    state -> execution_ctx -> object_loc -> string list ->
    value list -> lexical_env -> strictness_flag -> result_void **)

and arguments_object_map s c l xs args x str =
  let%object
    (s_2, lmap) = (run_construct_prealloc s c Prealloc_object []) in
       arguments_object_map_loop s_2 c l xs (LibList.length args) args x
         str lmap []

(** val create_arguments_object :
    state -> execution_ctx -> object_loc -> string list ->
    value list -> lexical_env -> strictness_flag -> result **)

and create_arguments_object s c lf xs args x str =
  let
     o = (object_create_builtin (Value_object (Object_loc_prealloc
                                                Prealloc_object_proto))
       ("Arguments")
       Heap.empty) in
        let  p = (object_alloc s o) in
            let (l, s_2) = p in
            let  a = ({ attributes_data_value = (Value_number (number_of_int (LibList.length args)));
                          attributes_data_writable = true; attributes_data_enumerable = false;
                          attributes_data_configurable = true }) in
                let%bool

                  (s1, b) = (object_define_own_prop s_2 c l
                     ("length")
                     (descriptor_of_attributes (Attributes_data_of a)) false) in
                     let%void
                       s2= (arguments_object_map s1 c l xs args x str) in
                          if str
                          then let  vthrower = (Value_object (Object_loc_prealloc
                                                                Prealloc_throw_type_error)) in
                              let  a0 = ({ attributes_accessor_get = vthrower;
                                            attributes_accessor_set = vthrower;
                                            attributes_accessor_enumerable = false;
                                            attributes_accessor_configurable = false }) in
                                  let%bool

                                    (s3, b_2) = (object_define_own_prop s2 c l
                                       ("caller")
                                       (descriptor_of_attributes
                                          (Attributes_accessor_of a0)) false) in
                                       let%bool

                                         (s4, b_3) = (object_define_own_prop s3 c l
                                            ("callee")
                                            (descriptor_of_attributes
                                               (Attributes_accessor_of a0)) false) in
                                            res_ter s4 (res_val (Value_object l))
                          else let  a0 = ({ attributes_data_value = (Value_object lf);
                                             attributes_data_writable = true;
                                             attributes_data_enumerable = false;
                                             attributes_data_configurable = true }) in
                              let%bool
                                 (s3, b_2) = (object_define_own_prop s2 c l
                                   ("callee")
                                   (descriptor_of_attributes (Attributes_data_of a0))
                                   false) in
                                    res_ter s3 (res_val (Value_object l))

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
                     (Value_object largs)
            else env_record_create_set_mutable_binding s1 c l arguments_ None
                (Value_object largs) false

(** val binding_inst_var_decls :
    state -> execution_ctx -> env_loc -> string list -> bool
    -> strictness_flag -> result_void **)

and binding_inst_var_decls s c l vds bconfig str =
  match vds with
  | [] -> res_void s
  | vd :: vds_2 ->
    let  bivd = (fun s0 -> binding_inst_var_decls s0 c l vds_2 bconfig str) in
    let%bool (s1, has) = (env_record_has_binding s c l vd) in
    if has then
      bivd s1
    else
      let%void s2 = env_record_create_set_mutable_binding s1 c l vd (Some bconfig) Value_undef str in
      bivd s2

(** val execution_ctx_binding_inst :
    state -> execution_ctx -> codetype -> object_loc option ->
    prog -> value list -> result_void **)

and execution_ctx_binding_inst s c ct funco p args =
  match c.execution_ctx_variable_env with
  | [] ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
      s
      ("Empty [execution_ctx_variable_env] in [execution_ctx_binding_inst].")
  | l :: l0 ->
    let  str = (prog_intro_strictness p) in
    let  follow = (fun s_2 names ->
        let bconfig = (codetype_compare ct Codetype_eval) in
        let  fds = (prog_funcdecl p) in
        let%void s1= (binding_inst_function_decls s_2 c l fds str bconfig) in
        let%bool (s2, bdefined) = (env_record_has_binding s1 c l ("arguments")) in
        let follow2 = (fun s10 ->
            let vds = prog_vardecl p in
            binding_inst_var_decls s10 c l vds bconfig str) in
        match ct with
        | Codetype_func ->
          (match funco with
           | Some func ->
             if bdefined
             then follow2 s2
             else let%void s3 = (binding_inst_arg_obj s2 c func p names args l) in  follow2 s3
           | None ->
             if bdefined
             then follow2 s2
             else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                 s2
                 ("Weird `arguments\' object in [execution_ctx_binding_inst]."))
        | Codetype_global -> follow2 s2
        | Codetype_eval -> follow2 s2) in
    match ct with
    | Codetype_func ->
      (match funco with
       | Some func ->
         let%some nameso = (run_object_method object_formal_parameters_ s func) in
         let%some names = (nameso) in
         let%void s_2 = (binding_inst_formal_params s c l args names str) in
         follow s_2 names
       | None ->
         (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
           s
           ("Non coherent functionnal code type in [execution_ctx_binding_inst]."))
    | Codetype_global ->
      (match funco with
       | Some o ->
         (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
           s
           ("Non coherent non-functionnal code type in [execution_ctx_binding_inst].")
       | None -> follow s [])
    | Codetype_eval ->
      (match funco with
       | Some o ->
         (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
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
    let%void s2 = (execution_ctx_binding_inst s1 c_2 Codetype_func
      (Some lf) (funcbody_prog bd) args) in
    run_call_default s2 c_2 lf)
  in
  if str
  then follow s vthis
  else (match vthis with
      | Value_undef ->
        follow s (Value_object (Object_loc_prealloc Prealloc_global))
      | Value_null ->
        follow s (Value_object (Object_loc_prealloc Prealloc_global))
      | Value_bool b -> let%value (s2, v) = (to_object s vthis) in follow s2 v
      | Value_number n -> let%value (s2, v) = (to_object s vthis) in follow s2 v
      | Value_string s0 -> let%value (s2, v) = (to_object s vthis) in follow s2 v
      | Value_object lthis -> follow s vthis)

(** ES5 version of [[GetOwnProperty]], redirects to ES6 as appropriate.
    @deprecated TODO: Rewrite into ES6 *)
and run_object_get_own_prop s c l x =
  let%some b = (run_object_method object_get_own_prop_ s l) in
  let def = (fun s ->
    let%spec (s, prop) = ordinary_get_own_property s l (Value_string x) in
    res_spec s (full_descriptor_of_undef_descriptor prop)
  ) in
  match b with
  | Builtin_get_own_prop_args_obj ->
    let%spec (s1, d) = (def s) in
    begin
      match d with
      | Full_descriptor_undef -> res_spec s1 Full_descriptor_undef
      | Full_descriptor_some a ->
        let%some lmapo = (run_object_method object_parameter_map_ s1 l) in
        let%some lmap = (lmapo) in
        let%spec (s2, d0) = (run_object_get_own_prop s1 c lmap x) in
        let follow = (fun s_2 a0 -> res_spec s_2 (Full_descriptor_some a0)) in
        match d0 with
        | Full_descriptor_undef -> follow s2 a
        | Full_descriptor_some amap ->
          let%value (s3, v) = (run_object_get s2 c lmap x) in
          match a with
          | Attributes_data_of ad ->
            follow s3 (Attributes_data_of (attributes_data_with_value ad v))
          | Attributes_accessor_of aa ->
            (Debug.impossible_with_heap_because __LOC__ s3
              "[run_object_get_own_prop]:  received an accessor property descriptor in a point where the specification suppose it never happens.";
              Result_impossible)
    end
  | Builtin_get_own_prop_string ->
    let%spec (s0, d) = def s in
    (match d with
    | Full_descriptor_undef ->
      let%spec (s1, k) = (to_int32 s0 c (Value_string x)) in
      let%string (s2, s3) = (to_string s1 (Value_number (JsNumber.absolute k))) in
      if not (string_eq x s3)
      then res_spec s2 Full_descriptor_undef
      else
        let%string (s4, str) = (run_object_prim_value s2 l) in
        let%spec (s5, k0) = (to_int32 s4 c (Value_string x)) in
        let len = (number_of_int (strlength str)) in
        if le_int_decidable len k0
        then res_spec s5 Full_descriptor_undef
        else
          let resultStr = string_sub str (int_of_number k0) 1 (* TODO: check k0 is not negative *) in
          let a = { attributes_data_value = (Value_string resultStr);
                    attributes_data_writable = false; attributes_data_enumerable = true;
                    attributes_data_configurable = false } in
          res_spec s5 (Full_descriptor_some (Attributes_data_of a))
    | Full_descriptor_some a -> res_spec s0 d)
  | _ ->
    let%spec (s, prop) = object_internal_get_own_property s l (Value_string x) in
    res_spec s (full_descriptor_of_undef_descriptor prop)

(** val run_function_has_instance :
    state -> object_loc -> value -> result **)

and run_function_has_instance s c lv _foo_ =
  (match _foo_ with
  | Value_object lo ->
    let%value s, vproto = object_internal_get_prototype_of s lv in
      (match vproto with
        | Value_null -> res_ter s (res_val (Value_bool false))
        | Value_object proto ->
          if object_loc_compare proto lo
          then res_ter s (res_val (Value_bool true))
          else run_function_has_instance s c proto (Value_object lo)
        | _ -> (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
            s
            ("Primitive found in the prototype chain in [run_object_has_instance_loop]."))
  | _ -> run_error s c Native_error_type)

(** val run_object_has_instance :
    state -> execution_ctx -> builtin_has_instance -> object_loc
    -> value -> result **)

and run_object_has_instance s c b l v =
  match b with
  | Builtin_has_instance_function ->
    (match v with
     | Value_object lv ->
       let%value (s1, vproto) = (run_object_get s c l ("prototype")) in
       (match vproto with
       | Value_object lproto -> run_function_has_instance s1 c lv (Value_object lproto)
       | _ -> run_error s1 c Native_error_type)
     | _ -> res_out s (res_val (Value_bool false)))
  | Builtin_has_instance_after_bind ->
    let%some ol = run_object_method object_target_function_ s l in
    let%some l0 = ol in
    let%some ob = run_object_method object_has_instance_ s l0 in
    match ob with
    | Some b0 -> run_object_has_instance s c b0 l0 v
    | None -> run_error s c Native_error_type

(** val from_prop_descriptor :
    state -> execution_ctx -> full_descriptor -> result **)

and from_prop_descriptor s c _foo_ = match _foo_ with
  | Full_descriptor_undef ->
    res_out s (res_val Value_undef)
  | Full_descriptor_some a ->
    let%object (s1, l) = run_construct_prealloc s c Prealloc_object [] in
    let follow = (fun s0 x ->
      let a1 = (attributes_data_intro_all_true (Value_bool (attributes_enumerable a))) in
      let%bool (s0_2, x0) = (object_define_own_prop s0 c l ("enumerable")
        (descriptor_of_attributes (Attributes_data_of a1)) throw_false) in
      let a2 = (attributes_data_intro_all_true (Value_bool (attributes_configurable a))) in
      let%bool (s_2, x1) = (object_define_own_prop s0_2 c l ("configurable") (descriptor_of_attributes (Attributes_data_of a2)) throw_false) in
      res_ter s_2 (res_val (Value_object l))) in
    match a with
    | Attributes_data_of ad ->
      let a1 = (attributes_data_intro_all_true ad.attributes_data_value) in
      let%bool (s2, x) = (object_define_own_prop s1 c l ("value")
        (descriptor_of_attributes (Attributes_data_of a1)) throw_false) in
      let a2 = (attributes_data_intro_all_true (Value_bool ad.attributes_data_writable)) in
      let%bool (s3, v) = (object_define_own_prop s2 c l ("writable")
        (descriptor_of_attributes (Attributes_data_of a2)) throw_false) in
      follow s3 v
    | Attributes_accessor_of aa ->
      let a1 = (attributes_data_intro_all_true aa.attributes_accessor_get) in
      let%bool (s2, x) = (object_define_own_prop s1 c l ("get")
        (descriptor_of_attributes (Attributes_data_of a1)) throw_false) in
      let a2 = (attributes_data_intro_all_true aa.attributes_accessor_set) in
      let%bool (s3, v) = (object_define_own_prop s2 c l ("set")
        (descriptor_of_attributes (Attributes_data_of a2)) throw_false) in
      follow s3 v

(** val run_equal :
    state -> execution_ctx -> value -> value -> result **)

and run_equal s c v1 v2 =
  let conv_number = fun s0 v -> to_number s0 v in
  let conv_primitive = fun s0 v -> to_primitive s0 v None in
  let checkTypesThen = (fun s0 v3 v4 k ->
    let ty1 = type_of v3 in
    let ty2 = type_of v4 in
    if type_compare ty1 ty2
    then res_out s0 (res_val (Value_bool (equality_test_for_same_type ty1 v3 v4)))
    else k ty1 ty2) in
  checkTypesThen s v1 v2 (fun ty1 ty2 ->
    let dc_conv = (fun v3 f v4 -> let%value (s0, v2_2) = (f s v4) in run_equal s0 c v3 v2_2) in
    let so = fun b -> res_out s (res_val (Value_bool b)) in
    if (type_compare ty1 Type_null) && (type_compare ty2 Type_undef)
    then so true
    else if (type_compare ty1 Type_undef) && (type_compare ty2 Type_null)
    then so true
    else if (type_compare ty1 Type_number) && (type_compare ty2 Type_string)
    then dc_conv v1 conv_number v2
    else if (type_compare ty1 Type_string) && (type_compare ty2 Type_number)
    then dc_conv v2 conv_number v1
    else if type_compare ty1 Type_bool
    then dc_conv v2 conv_number v1
    else if type_compare ty2 Type_bool
    then dc_conv v1 conv_number v2
    else if ((type_compare ty1 Type_string) || (type_compare ty1 Type_number)) && (type_compare ty2 Type_object)
    then dc_conv v1 conv_primitive v2
    else if (type_compare ty1 Type_object) && ((type_compare ty2 Type_string) || (type_compare ty2 Type_number))
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
  convert_twice ifx_prim (fun s0 v -> to_primitive s0 v None) s v1 v2

(** val convert_twice_number :
    state -> execution_ctx -> value -> value ->
    (number * number) specres **)

and convert_twice_number s c v1 v2 =
  convert_twice ifx_number (fun s0 v -> to_number s0 v) s v1 v2

(** val convert_twice_string :
    state -> execution_ctx -> value -> value ->
    (string * string) specres **)

and convert_twice_string s c v1 v2 =
  convert_twice ifx_string (fun s0 v -> to_string s0 v) s v1 v2

(** val issome : 'a1 option -> bool **)

and issome : 'a1 . 'a1 option -> bool = fun _foo_ ->
  match _foo_ with
  | Some t -> true
  | None -> false

and run_binary_op_add s c v1 v2 =
  let%spec (s1, (w1, w2)) = (convert_twice_primitive s c v1 v2) in
  if  (type_compare (type_of w1) Type_string)
   || (type_compare (type_of w2) Type_string)
  then let%spec (s2, (str1, str2)) = (convert_twice_string s1 c w1 w2) in
    res_out s2 (res_val (Value_string (strappend str1 str2)))
  else let%spec (s2, (n1, n2)) = (convert_twice_number s1 c w1 w2) in
    res_out s2 (res_val (Value_number (n1 +. n2)))

and run_binary_op_arith mathop s c v1 v2 =
  let%spec (s1, nn) = (convert_twice_number s c v1 v2) in
  let (n1, n2) = nn in
  res_out s1 (res_val (Value_number (mathop n1 n2)))

and run_binary_op_shift b_unsigned mathop s c v1 v2 =
  let conv = (if b_unsigned then to_uint32 else to_int32) in
  let%spec (s1, k1) = (conv s c v1) in
  let%spec (s2, k2) = (to_uint32 s1 c v2) in
  let k2_2 = JsNumber.modulo_32 k2 in
  res_ter s2 (res_val (Value_number (mathop k1 k2_2)))

and run_binary_op_bitwise mathop s c v1 v2 =
  let%spec (s1, k1) = (to_int32 s c v1) in
  let%spec (s2, k2) = (to_int32 s1 c v2) in
  res_ter s2 (res_val (Value_number (mathop k1 k2)))

(** TODO: ES6ify
    @essec 7.2.12 *)
and run_binary_op_compare b_swap b_neg s c v1 v2 =
  let%spec (s1, ww) = convert_twice_primitive s c v1 v2 in
  let (w1, w2) = ww in
  let p = if b_swap then (w2, w1) else (w1, w2) in
  let (wa, wb) = p in
  let%ret s, wr =
    if (type_of wa === Type_string) && (type_of wb === Type_string) then
      Continue (s, Value_bool (inequality_test_string (string_of_value wb) (string_of_value wb)))
    else
      let%number_ret s, nx = to_number s1 wa in
      let%number_ret s, ny = to_number s wb in
      Continue (s, inequality_test_number nx ny)
  in
  if value_compare wr Value_undef then res_out s1 (res_val (Value_bool false))
  else if (b_neg) && (value_compare wr (Value_bool true))
  then res_out s1 (res_val (Value_bool false))
  else if (b_neg) && (value_compare wr (Value_bool false))
  then res_out s1 (res_val (Value_bool true))
  else res_out s1 (res_val wr)

and run_binary_op_instanceof s c v1 v2 =
  match v2 with
  | Value_object l ->
    let%some b = (run_object_method object_has_instance_ s l) in
    (match b with
    | None -> run_error s c Native_error_type
    | Some has_instance_id -> run_object_has_instance s c has_instance_id l v1)
  | _ -> run_error s c Native_error_type

and run_binary_op_in s c v1 v2 =
  match v2 with
  | Value_object l ->
    let%string (s2, x) = (to_string s v1) in
    object_has_prop s2 c l x
  | _ -> run_error s c Native_error_type

(** val run_binary_op :
    state -> execution_ctx -> binary_op -> value -> value ->
    result **)

and run_binary_op s c op v1 v2 =
  match op with
  | Binary_op_mult -> run_binary_op_arith (fun x y -> x *. y) s c v1 v2
  | Binary_op_div -> run_binary_op_arith (fun x y -> x /. y) s c v1 v2
  | Binary_op_mod -> run_binary_op_arith (fun x y -> JsNumber.fmod x y) s c v1 v2
  | Binary_op_sub -> run_binary_op_arith (fun x y -> x -. y) s c v1 v2
  | Binary_op_lt -> run_binary_op_compare false false s c v1 v2
  | Binary_op_gt -> run_binary_op_compare true false s c v1 v2
  | Binary_op_le -> run_binary_op_compare true true s c v1 v2
  | Binary_op_ge -> run_binary_op_compare false true s c v1 v2
  | Binary_op_left_shift -> run_binary_op_shift false JsNumber.int32_left_shift s c v1 v2
  | Binary_op_right_shift -> run_binary_op_shift false JsNumber.int32_right_shift s c v1 v2
  | Binary_op_unsigned_right_shift -> run_binary_op_shift true JsNumber.uint32_right_shift s c v1 v2
  | Binary_op_bitwise_and -> run_binary_op_bitwise JsNumber.int32_bitwise_and s c v1 v2
  | Binary_op_bitwise_or  -> run_binary_op_bitwise JsNumber.int32_bitwise_or s c v1 v2
  | Binary_op_bitwise_xor -> run_binary_op_bitwise JsNumber.int32_bitwise_xor s c v1 v2
  | Binary_op_add -> run_binary_op_add s c v1 v2
  | Binary_op_instanceof -> run_binary_op_instanceof s c v1 v2
  | Binary_op_in -> run_binary_op_in s c v1 v2
  | Binary_op_equal -> run_equal s c v1 v2
  | Binary_op_disequal ->
    let%bool (s0, b0) = (run_equal s c v1 v2) in
    res_ter s0 (res_val (Value_bool (not b0)))
  | Binary_op_strict_equal ->
    res_out s (res_val (Value_bool (strict_equality_test v1 v2)))
  | Binary_op_strict_disequal ->
    res_out s (res_val (Value_bool (not (strict_equality_test v1 v2))))
  | Binary_op_coma -> res_out s (res_val v2)
  | Binary_op_and -> Result_impossible
  | Binary_op_or  -> Result_impossible

(** val run_prepost_op : unary_op -> ((number -> number) * bool) option **)

and run_prepost_op _foo_ = match _foo_ with
  | Unary_op_delete -> None
  | Unary_op_void -> None
  | Unary_op_typeof -> None
  | Unary_op_post_incr -> Some (add_one, false)
  | Unary_op_post_decr -> Some (sub_one, false)
  | Unary_op_pre_incr -> Some (add_one, true)
  | Unary_op_pre_decr -> Some (sub_one, true)
  | Unary_op_add -> None
  | Unary_op_neg -> None
  | Unary_op_bitwise_not -> None
  | Unary_op_not -> None

(** val run_typeof_value : state -> value -> string **)

and run_typeof_value s _foo_ =
  match _foo_ with
  | Value_object l ->
    if is_callable_dec s (Value_object l)
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
    let%number (s3, n1) = to_number s2 v2 in
    let%some po = run_prepost_op op in
    let (number_op, is_pre) = po in
    let n2 = number_op n1 in
    let v = Value_number (if is_pre then n2 else n1) in
    let%void s4 = ref_put_value s3 c rv1 (Value_number n2) in
    res_out s4 (res_val v)
  else (match op with
      | Unary_op_delete ->
        let%success (s0, rv)= (run_expr s c e) in begin
            match rv with
            | Resvalue_empty ->
              res_ter s0 (res_val (Value_bool true))
            | Resvalue_value v ->
              res_ter s0 (res_val (Value_bool true))
            | Resvalue_ref r ->
              if ref_kind_comparable (ref_kind_of r) Ref_kind_undef
              then if r.ref_strict
                then run_error s0 c Native_error_syntax
                else res_ter s0
                    (res_val (Value_bool true))
              else (match r.ref_base with
                  | Ref_base_type_value v ->
                    let%object (s1, l) = (to_object s0 v) in
                        object_delete s1 c l r.ref_name
                          r.ref_strict
                  | Ref_base_type_env_loc l ->
                    if r.ref_strict
                    then run_error s0 c Native_error_syntax
                    else env_record_delete_binding s0 c l r.ref_name)
        end
      | Unary_op_typeof ->
        let%success (s1, rv)= (run_expr s c e) in begin
            match rv with
            | Resvalue_empty ->
              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                s1
                ("Empty result for a `typeof\' in [run_unary_op].")
            | Resvalue_value v ->
              res_ter s1
                (res_val (Value_string (run_typeof_value s1 v)))
            | Resvalue_ref r ->
              if ref_kind_comparable (ref_kind_of r) Ref_kind_undef
              then res_ter s1
                  (res_val (Value_string ("undefined")))
              else let%spec
                  (s2, v) = (ref_get_value s1 c (Resvalue_ref r)) in
                     res_ter s2
                       (res_val (Value_string (run_typeof_value s2 v)))
        end
      | _ ->
        let%spec (s1, v) = (run_expr_get_value s c e) in
            match op with
            | Unary_op_void ->
              res_ter s1 (res_val Value_undef)
            | Unary_op_add -> to_number s1 v
            | Unary_op_neg ->
              let%number (s2, n) = (to_number s1 v) in
                  res_ter s2
                    (res_val (Value_number (JsNumber.neg n)))
            | Unary_op_bitwise_not ->
              let%spec (s2, k) = (to_int32 s1 c v) in
                  res_ter s2
                    (res_val (Value_number (JsNumber.int32_bitwise_not k)))
            | Unary_op_not ->
              res_ter s1
                (res_val (Value_bool (not (to_boolean v))))
            | _ ->
              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op]."))

(** val create_new_function_in :
    state -> execution_ctx -> string list -> funcbody ->
    result **)

and create_new_function_in s c args _term_ =
  creating_function_object s c args _term_ c.execution_ctx_lexical_env
    c.execution_ctx_strict

(** val init_object :
    state -> execution_ctx -> object_loc -> propdefs -> result **)

and init_object s c l _foo_ = match _foo_ with
  | [] -> res_out s (res_val (Value_object l))
  | p :: pds_2 ->
    let (pn, _term_) = p in
    let  x = (string_of_propname pn) in
        let  follows = (fun s1 desc ->
            let%success
              (s2, rv) = (object_define_own_prop s1 c l x desc false) in  init_object s2 c l pds_2) in
            match _term_ with
            | Propbody_val e0 ->
              let%spec (s1, v0) = (run_expr_get_value s c e0) in
                  let desc = { descriptor_value = (Some v0); descriptor_writable =
                                                               (Some true); descriptor_get = None; descriptor_set = None;
                               descriptor_enumerable = (Some true); descriptor_configurable =
                                                                      (Some true) }
                  in
                  follows s1 desc
            | Propbody_get bd ->
              let%value (s1, v0) = (create_new_function_in s c [] bd) in
                  let desc = { descriptor_value = None; descriptor_writable = None;
                               descriptor_get = (Some v0); descriptor_set = None;
                               descriptor_enumerable = (Some true); descriptor_configurable =
                                                                      (Some true) }
                  in
                  follows s1 desc
            | Propbody_set (args, bd) ->
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
  | [] -> res_out s (res_val (Value_object l))
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
                             (s3, slen) = (to_string s2 (Value_number (ilen +. n))) in
                                let  desc = ({ attributes_data_value = v;
                                              attributes_data_writable = true;
                                              attributes_data_enumerable = true;
                                              attributes_data_configurable = true }) in
                                    let%bool
                                       (s4, x) = (object_define_own_prop s3 c l slen
                                         (descriptor_of_attributes (Attributes_data_of
                                                                      desc)) false) in
                                          let%object (s5, l0) = (loop_result s4) in
                                              res_ter s5 (res_val (Value_object l0))
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
  let%spec (s3, len) = (to_uint32 s2 c (Value_number (ilen +. number_of_int elisionLength))) in
  let%not_throw (s4, x) = (object_put s3 c l0 ("length") (Value_number len) throw_false) in
  res_out s4 (res_val (Value_object l0))

(** val run_var_decl_item :
    state -> execution_ctx -> prop_name -> expr option -> result **)

and run_var_decl_item s c x _foo_ = match _foo_ with
  | Some e ->
    let%spec (s1, ir) = (identifier_resolution s c x) in
    let%spec (s2, v) = (run_expr_get_value s1 c e) in
    let%void s3 = (ref_put_value s2 c (Resvalue_ref ir) v) in
    res_out s3 (res_val (Value_string x))
  | None ->
    res_out s (res_val (Value_string x))

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
  | [] -> res_ter s (res_normal Resvalue_empty)
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
  | Binary_op_and -> run_binary_op_and s c e1 e2
  | Binary_op_or -> run_binary_op_or s c e1 e2
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
      if or_decidable (value_compare v1 Value_undef)
           (value_compare v1 Value_null)
      then run_error s2 Native_error_type
      else let%string (s3, x) = (to_string s2 c v2) in
             res_ter s3
               (res_ref (ref_create_value v1 x c.execution_ctx_strict))
*)

and run_expr_access s c e1 e2 =
  let%spec (s1,v1) = run_expr_get_value s c e1 in
  let%spec (s2,v2) = run_expr_get_value s1 c e2 in
  if    (value_compare v1 Value_undef)
     || (value_compare v1 Value_null)
  then run_error s2 c Native_error_type
  else let%string (s3,x) = to_string s2 v2 in
    res_ter s3 (res_ref (ref_create_value v1 x c.execution_ctx_strict))

(** val run_expr_assign :
    state -> execution_ctx -> binary_op option -> expr -> expr
    -> result **)

and run_expr_assign s c opo e1 e2 =
  let%success (s1, rv1)= (run_expr s c e1) in
  let follow = (fun s0 rv_2 ->
    match rv_2 with
    | Resvalue_value v ->
      let%void s_2= (ref_put_value s0 c rv1 v) in
      res_out s_2 (res_val v)
    | Resvalue_empty ->
      Debug.impossible_with_heap_because __LOC__ s0 "Non-value result in [run_expr_assign]."; Result_impossible
    | Resvalue_ref r ->
      Debug.impossible_with_heap_because __LOC__ s "Non-value result in [run_expr_assign]."; Result_impossible
    ) in
  match opo with
  | Some op ->
    let%spec (s2, v1) = (ref_get_value s1 c rv1) in
    let%spec (s3, v2) = (run_expr_get_value s2 c e2) in
    let%success (s4, v) = (run_binary_op s3 c op v1 v2) in
    follow s4 v
  | None ->
    let%spec (x, x0 )= (run_expr_get_value s1 c e2) in
    follow x (Resvalue_value x0)

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
      let%void s3 = (env_record_initialize_immutable_binding s2 l fn (Value_object l0)) in
      res_out s3 (res_val (Value_object l0))
    in destr_list lex_2 (Result_impossible (*"Empty lexical environnment allocated in [run_expr_function]."*))
      (fun l -> follow l)
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
                             s1 = (execution_ctx_binding_inst s_2 c1 Codetype_eval None
                               p0 []) in  k s1 c1

(** val run_eval :
    state -> execution_ctx -> bool -> value list -> result **)

and run_eval s c is_direct_call vs =
  match get_arg 0 vs with
  | Value_undef ->
    res_out s (res_val Value_undef)
  | Value_null ->
    res_out s (res_val Value_null)
  | Value_bool b ->
    res_out s (res_val (Value_bool b))
  | Value_number n ->
    res_out s (res_val (Value_number n))
  | Value_string s0 ->
    let str = (is_direct_call && c.execution_ctx_strict) in
    (match parse_pickable s0 str with
    | Some p0 ->
       entering_eval_code s c is_direct_call (Funcbody_intro (p0, s0))
         (fun s1 c_2 ->
           let%ter (s2, r) = (run_prog s1 c_2 p0) in
           match r.res_type with
           | Restype_normal ->
             ifx_empty_label s2 r (fun x ->
               match r.res_value with
               | Resvalue_empty -> res_ter s2 (res_val Value_undef)
               | Resvalue_value v -> res_ter s2 (res_val v)
               | Resvalue_ref r0 -> (Debug.impossible_with_heap_because __LOC__ s2
                   "Reference found in the result of an `eval\' in [run_eval]."; Result_impossible))
           | Restype_throw -> res_ter s2 (res_throw r.res_value)
           | _ -> (Debug.impossible_with_heap_because __LOC__ s2
               "Forbidden result type returned by an `eval\' in [run_eval]."; Result_impossible))
    | None -> run_error s c Native_error_syntax)
  | Value_object o ->
    res_out s (res_val (Value_object o))

(** val run_expr_call :
    state -> execution_ctx -> expr -> expr list -> result **)

and run_expr_call s c e1 e2s =
  let is_eval_direct = (is_syntactic_eval e1) in
  let%success (s1, rv)= (run_expr s c e1) in
  let%spec (s2, f) = (ref_get_value s1 c rv) in
  let%spec (s3, vs) = (run_list_expr s2 c [] e2s) in
  match f with
  | Value_object l ->
    if is_callable_dec s3 (Value_object l)
    then let  follow = (fun vthis ->
        if object_loc_compare l (Object_loc_prealloc
                                      Prealloc_global_eval)
        then run_eval s3 c is_eval_direct vs
        else run_call s3 c l vthis vs) in
        match rv with
        | Resvalue_empty ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
            s3
            ("[run_expr_call] unable to call an  empty result.")
        | Resvalue_value v ->
          follow Value_undef
        | Resvalue_ref r ->
          (match r.ref_base with
           | Ref_base_type_value v ->
             if   (ref_kind_comparable (ref_kind_of r) Ref_kind_primitive_base)
               || (ref_kind_comparable (ref_kind_of r) Ref_kind_null)
               || (ref_kind_comparable (ref_kind_of r) Ref_kind_object)
             then follow v
             else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                 s3
                 ("[run_expr_call] unable to call a non-property function.")
           | Ref_base_type_env_loc l0 ->
             let%some v = (env_record_implicit_this_value s3 l0) in  follow v)
    else run_error s3 c Native_error_type
  | _ -> run_error s3 c Native_error_type

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
  | Value_object l ->
    let%some coo = (run_object_method object_construct_ s2 l) in
    (match coo with
    | Some co -> run_construct s2 c co l args
    | None -> run_error s2 c Native_error_type)
  | _ -> run_error s2 c Native_error_type

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
    | None -> res_out s1 (res_normal Resvalue_empty))

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
                    (resvalue_compare r.res_value Resvalue_empty)
                 then r.res_value
                 else rv) in
                    let  loop = (fun x ->
                        run_stat_while s2 c rv_2 labs e1 t2) in
                        if  (not (restype_compare r.res_type Restype_continue))
                         || (not (res_label_in r labs))
                        then if
                             (restype_compare r.res_type Restype_break)
                           && (res_label_in r labs)
                          then res_ter s2 (res_normal rv_2)
                          else if not
                              (restype_compare r.res_type
                                 Restype_normal)
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
    match y with Switchclause_intro (e, ts) ->
      ifx_success_state rv (run_block s c (rev ts)) (fun s1 rv1 ->
          run_stat_switch_end s1 c rv1 scs_2)

(** val run_stat_switch_no_default :
    state -> execution_ctx -> value -> resvalue -> switchclause
    list -> result **)

and run_stat_switch_no_default s c vi rv _foo_ = match _foo_ with
  | [] -> res_out s (res_normal rv)
  | y :: scs_2 ->
    match y with Switchclause_intro (e, ts) ->
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
    match y with Switchclause_intro (e, ts) ->
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
    match y with Switchclause_intro (e, ts) ->
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
  | Switchbody_nodefault scs ->
    let%success (s0, r) = begin
      let%break (s2, r) =
        run_stat_switch_no_default s1 c vi
        Resvalue_empty scs in
      if res_label_in r labs
      then res_out s2 (res_normal r.res_value)
      else res_out s2 r
      end in
    res_ter s0 (res_normal r)
  | Switchbody_withdefault (scs1, ts, scs2) ->
    let%success (s0, r) = begin
      let%break (s2, r) =
        run_stat_switch_with_default_A s1 c false vi
         Resvalue_empty scs1 ts scs2 in
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
  | Switchbody_nodefault scs ->
    follow (run_stat_switch_no_default s1 c vi
              Resvalue_empty scs)
  | Switchbody_withdefault (scs1, ts, scs2) ->
    follow (run_stat_switch_with_default_A s1 c false vi
              Resvalue_empty scs1 ts scs2)

(** val run_stat_do_while :
    state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

and run_stat_do_while s c rv labs e1 t2 =
  let%ter (s1, r) = (run_stat s c t2) in
      let
         rv_2 = (if resvalue_compare r.res_value Resvalue_empty
         then rv
         else r.res_value) in
            let  loop = (fun x ->
                let%spec (s2, v1) = (run_expr_get_value s1 c e1) in
                    let  b = (to_boolean v1) in
                        if b
                        then run_stat_do_while s2 c rv_2 labs e1 t2
                        else res_ter s2 (res_normal rv_2)) in
                if  (restype_compare r.res_type Restype_continue)
                   && (res_label_in r labs)
                then loop ()
                else if
                    (restype_compare r.res_type Restype_break)
                 && (res_label_in r labs)
                then res_ter s1 (res_normal rv_2)
                else if not
                    (restype_compare r.res_type Restype_normal)
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
                       (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
                         s_2
                         ("Empty lexical environnment in [run_stat_try].")
                     | l :: oldlex ->
                       let%void
                          s2= (env_record_create_set_mutable_binding s_2 c l x None v
                            throw_irrelevant) in
                             let c_2 = execution_ctx_with_lex c lex_2 in
                             let%ter (s3, r) = (run_stat s2 c_2 t2) in finallycont s3 r)
          | None -> finallycont s1 (res_throw (Resvalue_value v)))

(** val run_stat_throw :
    state -> execution_ctx -> expr -> result **)

and run_stat_throw s c e =
  let%spec (s1, v1) = (run_expr_get_value s c e) in
      res_ter s1 (res_throw (Resvalue_value v1))

(** val run_stat_return :
    state -> execution_ctx -> expr option -> result **)

and run_stat_return s c _foo_ = match _foo_ with
  | Some e ->
      let%spec (s1, v1) = (run_expr_get_value s c e) in
      res_ter s1 (res_return (Resvalue_value v1))
  | None ->
      res_out s (res_return (Resvalue_value Value_undef))

(** val run_stat_for_loop :
    state -> execution_ctx -> label_set -> resvalue -> expr
    option -> expr option -> stat -> result **)

and run_stat_for_loop s c labs rv eo2 eo3 t =
  let  follows = (fun s0 ->
      let%ter (s1, r) = (run_stat s0 c t) in
          let
             rv_2 = (if not
                (resvalue_compare r.res_value Resvalue_empty)
             then r.res_value
             else rv) in
                let  loop = (fun s2 ->
                    run_stat_for_loop s2 c labs rv_2 eo2 eo3 t) in
                    if   (restype_compare r.res_type Restype_break)
                      && (res_label_in r labs)
                    then res_ter s1 (res_normal rv_2)
                    else if
                         (restype_compare r.res_type Restype_normal)
                      || (    (restype_compare r.res_type Restype_continue)
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
    run_stat_for_loop s0 c labs Resvalue_empty eo2 eo3 t
  in
  (match eo1 with
   | Some e1 ->
     let%spec (s0, v1) = (run_expr_get_value s c e1) in follows s0
   | None -> follows s)

(** val run_stat_for_var :
    state -> execution_ctx -> label_set -> (string * expr
    option) list -> expr option -> expr option -> stat -> result **)

and run_stat_for_var s c labs ds eo2 eo3 t =
  let%ter (s0, r) = (run_stat s c (Stat_var_decl ds)) in
      run_stat_for_loop s0 c labs Resvalue_empty eo2 eo3 t

(** val run_expr : state -> execution_ctx -> expr -> result **)

and run_expr s c _term_ = match _term_ with
  | Expr_this ->
    res_out s (res_val c.execution_ctx_this_binding)
  | Expr_identifier x ->
    let%spec (s0, r) = (identifier_resolution s c x) in
    res_ter s0 (res_ref r)
  | Expr_literal i ->
    res_out s (res_val (convert_literal_to_prim i))
  | Expr_object pds ->
    let%object (s1, l) = run_construct_prealloc s c Prealloc_object [] in
    init_object s1 c l pds
  | Expr_array oes ->
    let%object (s1, l) = run_construct_prealloc s c Prealloc_array [] in
    init_array s1 c l oes
  | Expr_function (fo, args, bd) -> run_expr_function s c fo args bd
  | Expr_access (e1, e2) -> run_expr_access s c e1 e2
  | Expr_member (e1, f) ->
    run_expr s c (Expr_access (e1, (Expr_literal
                                          (Literal_string f))))
  | Expr_new (e1, e2s) -> run_expr_new s c e1 e2s
  | Expr_call (e1, e2s) -> run_expr_call s c e1 e2s
  | Expr_unary_op (op, e0) -> run_unary_op s c op e0
  | Expr_binary_op (e1, op, e2) -> run_expr_binary_op s c op e1 e2
  | Expr_conditional (e1, e2, e3) ->
    run_expr_conditionnal s c e1 e2 e3
  | Expr_assign (e1, opo, e2) -> run_expr_assign s c opo e1 e2

(** val run_stat : state -> execution_ctx -> stat -> result **)

and run_stat s c _term_ = match _term_ with
  | Stat_expr e ->
    let%spec (s0, r) = (run_expr_get_value s c e) in
        res_ter s0 (res_val r)
  | Stat_label (lab, t0) ->
    run_stat_label s c (Label_string lab) t0
  | Stat_block ts -> run_block s c (rev ts)
  | Stat_var_decl xeos -> run_var_decl s c xeos
  | Stat_if (e1, t2, to0) -> run_stat_if s c e1 t2 to0
  | Stat_do_while (ls, t1, e2) ->
    run_stat_do_while s c Resvalue_empty ls e2 t1
  | Stat_while (ls, e1, t2) ->
    run_stat_while s c Resvalue_empty ls e1 t2
  | Stat_with (e1, t2) -> run_stat_with s c e1 t2
  | Stat_throw e -> run_stat_throw s c e
  | Stat_return eo -> run_stat_return s c eo
  | Stat_break so -> res_out s (res_break so)
  | Stat_continue so -> res_out s (res_continue so)
  | Stat_try (t1, t2o, t3o) -> run_stat_try s c t1 t2o t3o
  | Stat_for (ls, eo1, eo2, eo3, s0) ->
    run_stat_for s c ls eo1 eo2 eo3 s0
  | Stat_for_var (ls, ds, eo2, eo3, s0) ->
    run_stat_for_var s c ls ds eo2 eo3 s0
  | Stat_for_in (ls, e1, e2, s0) ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Result_not_yet_implemented)
      ("stat_for_in")
  | Stat_for_in_var (ls, x, e1o, e2, s0) ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Result_not_yet_implemented)
      ("stat_for_in_var")
  | Stat_debugger -> res_out s res_empty
  | Stat_switch (labs, e, sb) -> run_stat_switch s c labs e sb

(** val run_elements :
    state -> execution_ctx -> elements -> result **)

and run_elements s c _foo_ = match _foo_ with
  | [] -> res_out s (res_normal Resvalue_empty)
  | el :: els_rev_2 ->
    let%success (s0, rv0)= (run_elements s c els_rev_2) in
        match el with
        | Element_stat t ->
          let%ter (s1, r1) = (run_stat s0 c t) in
              let r2 = res_overwrite_value_if_empty rv0 r1 in
              res_out s1 r2
        | Element_func_decl (name, args, bd) -> res_ter s0 (res_normal rv0)

(** val run_prog : state -> execution_ctx -> prog -> result **)

and run_prog s c _term_ = match _term_ with
  | Prog_intro (str, els) -> run_elements s c (rev els)

(** val push :
    state -> execution_ctx -> object_loc -> value list -> float
    -> result **)

and push s c l args ilen =
  let vlen = ilen in
      match args with
      | [] ->
        let%not_throw
           (s0, x) = (object_put s c l ("length")
             (Value_number vlen) throw_true) in
              res_out s0 (res_val (Value_number vlen))
      | v :: vs ->
        let%string
          (s0, slen) = (to_string s (Value_number vlen)) in
             let%not_throw  (s1, x) = (object_put s0 c l slen v throw_true) in
                 push s1 c l vs (ilen +. 1.)

(** val run_get_args_for_apply :
    state -> execution_ctx -> object_loc -> float -> float ->
    value list specres **)

and run_get_args_for_apply s c l index n =
  if  index < n
  then let%string
       (s0, sindex) = (to_string s (Value_number index)) in
          let%value (s1, v) = (run_object_get s0 c l sindex) in
              let

                tail_args = (run_get_args_for_apply s1 c l (index +. 1.) n) in
                   let%spec (s2, tail) = (tail_args) in res_spec s2 (v :: tail)
  else res_spec s []

(** val valueToStringForJoin :
    state -> execution_ctx -> object_loc -> float -> string
    specres **)

and valueToStringForJoin s c l k =
  let%string (s0, prop) = (to_string s (Value_number k)) in
  let%value (s1, v) = (run_object_get s0 c l prop) in
  match v with
  | Value_undef -> res_spec s1 ""
  | Value_null -> res_spec s1 ""
  | Value_bool b ->
    let%string (s2, s3) = (to_string s1 v) in res_spec s2 s3
  | Value_number n ->
    let%string (s2, s3) = (to_string s1 v) in res_spec s2 s3
  | Value_string s2 ->
    let%string (s3, s4) = (to_string s1 v) in res_spec s3 s4
  | Value_object o ->
    let%string (s2, s3) = (to_string s1 v) in res_spec s2 s3

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
  else res_ter s (res_val (Value_string sR))

(** Definitions of prealloc (built-in) functions *)
(** val run_call_prealloc :
    state -> execution_ctx -> prealloc -> value -> value list ->
    result **)

and run_call_prealloc s c b l vthis args =
  match b with
  | Prealloc_global_is_finite ->
    let  v = (get_arg 0 args) in
    let%number (s0, n) = (to_number s v) in
    res_ter s0 (res_val (Value_bool (not ((JsNumber.isnan n) || (n === JsNumber.infinity) || (n === JsNumber.neg_infinity)))))
  | Prealloc_global_is_nan ->
    let  v = (get_arg 0 args) in
        let%number (s0, n) = (to_number s v) in
            res_ter s0
              (res_val (Value_bool (JsNumber.isnan n)))
  | Prealloc_object ->
    let  value0 = (get_arg 0 args) in begin
        match value0 with
        | Value_undef -> run_construct_prealloc s c b args
        | Value_null -> run_construct_prealloc s c b args
        | Value_bool b0 -> to_object s value0
        | Value_number n -> to_object s value0
        | Value_string s0 -> to_object s value0
        | Value_object o -> to_object s value0
    end
  | Prealloc_object_get_proto_of ->
    let  v = (get_arg 0 args) in
    builtin_object_get_prototype_of s c () vthis () v
  | Prealloc_object_set_proto_of ->
    let o = get_arg 0 args in
    let proto = get_arg 1 args in
    builtin_object_set_prototype_of s c () vthis () o proto
  | Prealloc_object_get_own_prop_descriptor ->
    let  v = (get_arg 0 args) in begin
      match v with
      | Value_object l ->
        let%string (s1, x) = (to_string s (get_arg 1 args)) in
        let%spec (s2, d) = (run_object_get_own_prop s1 c l x) in
        from_prop_descriptor s2 c d
      | _ -> run_error s c Native_error_type
    end
  | Prealloc_object_define_prop ->
    let o = (get_arg 0 args) in
    let p = (get_arg 1 args) in
    let attr = (get_arg 2 args) in begin
      match o with
      | Value_object l ->
        let%string (s1, name) = (to_string s p) in
        let%spec (s2, desc) = (to_property_descriptor s1 attr) in
        let%bool (s3, x) = (object_define_own_prop s2 c l name desc true) in
        res_ter s3 (res_val (Value_object l))
      | _ -> run_error s c Native_error_type
    end
  | Prealloc_object_seal ->
    let v = get_arg 0 args in
    builtin_object_seal s c () vthis () v
  | Prealloc_object_freeze ->
    let v = get_arg 0 args in
    builtin_object_freeze s c () vthis () v
  | Prealloc_object_prevent_extensions ->
    let v = get_arg 0 args in
    builtin_object_prevent_extensions s c () vthis () v
  | Prealloc_object_is_sealed ->
    let v = get_arg 0 args in
    builtin_object_is_sealed s c () vthis () v
  | Prealloc_object_is_frozen ->
    let v = get_arg 0 args in
    builtin_object_is_frozen s c () vthis () v
  | Prealloc_object_is_extensible ->
    let  v = (get_arg 0 args) in
    builtin_object_is_extensible s c () vthis () v
  | Prealloc_object_keys ->
    let v = get_arg 0 args in
    builtin_object_keys s c () vthis () v
  | Prealloc_object_proto_to_string ->
    (match vthis with
     | Value_undef ->
       res_out s (res_val (Value_string ("[object Undefined]")))
     | Value_null ->
       res_out s (res_val (Value_string ("[object Null]")))
     | Value_bool b0 ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s0= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Value_string (strappend "[object " (strappend s0 "]"))))
     | Value_number n ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s0= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Value_string (strappend "[object " (strappend s0 "]"))))
     | Value_string s0 ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s2= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Value_string (strappend "[object " (strappend s2 "]"))))
     | Value_object o ->
       let%object (s1, l) = (to_object s vthis) in
       let%some s0= (run_object_method object_class_ s1 l) in
       res_ter s1 (res_val (Value_string (strappend "[object " (strappend s0 "]")))))
  | Prealloc_object_proto_value_of -> to_object s vthis
  | Prealloc_object_proto_has_own_prop ->
    let  v = (get_arg 0 args) in
        let%string (s1, x) = (to_string s v) in
            let%object (s2, l) = (to_object s1 vthis) in
                let%spec (s3, d) = (run_object_get_own_prop s2 c l x) in begin
                    match d with
                    | Full_descriptor_undef ->
                      res_ter s3 (res_val (Value_bool false))
                    | Full_descriptor_some a ->
                      res_ter s3 (res_val (Value_bool true))
    end
  | Prealloc_object_proto_is_prototype_of ->
    let v = get_arg 0 args in
    builtin_object_prototype_is_prototype_of s c () vthis () v
  | Prealloc_object_proto_prop_is_enumerable ->
    let  v = (get_arg 0 args) in
    let%string (s1, x) = (to_string s v) in
    let%object (s2, l) = (to_object s1 vthis) in
    let%spec (s3, d) = (run_object_get_own_prop s2 c l x) in begin
      match d with
      | Full_descriptor_undef ->
        res_ter s3 (res_val (Value_bool false))
      | Full_descriptor_some a ->
        res_ter s3 (res_val (Value_bool (attributes_enumerable a)))
    end
  | Prealloc_function_proto ->
    res_out s (res_val Value_undef)
  | Prealloc_function_proto_to_string ->
    if is_callable_dec s vthis
    then (fun s -> Debug.not_yet_implemented_because __LOC__ s; Result_not_yet_implemented)
        ("Function.prototype.toString() is implementation dependent.")
    else run_error s c Native_error_type
  | Prealloc_function_proto_apply ->
    let  thisArg = (get_arg 0 args) in
    let  argArray = (get_arg 1 args) in
    if is_callable_dec s vthis
    then
      (match vthis with
      | Value_object thisobj ->
        (match argArray with
        | Value_undef -> run_call s c thisobj thisArg []
        | Value_null -> run_call s c thisobj thisArg []
        | Value_bool b0 -> run_error s c Native_error_type
        | Value_number n -> run_error s c Native_error_type
        | Value_string s0 -> run_error s c Native_error_type
        | Value_object array ->
          let%value (s0, v) = (run_object_get s c array ("length")) in
          let%spec (s1, ilen) = (to_uint32 s0 c v) in
          let%spec (s2, arguments_) = (run_get_args_for_apply s1 c array 0. ilen) in
          run_call s2 c thisobj thisArg arguments_)
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s
          ("Value is callable, but isn\'t an object."))
            else run_error s c Native_error_type
  | Prealloc_function_proto_call ->
    if is_callable_dec s vthis
    then
      (match vthis with
      | Value_object thisobj ->
        let (thisArg, a) = get_arg_first_and_rest args in
        run_call s c thisobj thisArg a
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s
          ("Value is callable, but isn\'t an object.")
      )
    else run_error s c Native_error_type
  | Prealloc_function_proto_bind ->
    if is_callable_dec s vthis
    then
      (match vthis with
      | Value_object thisobj ->
        let (vthisArg, a) = get_arg_first_and_rest args in
        let o1 = (object_new (Value_object (Object_loc_prealloc Prealloc_object_proto)) ("Object")) in
        let o2 = o1 in
        let o3 = (object_with_details o2 None None None (Some thisobj) (Some vthisArg) (Some a) None) in
        let o4 = (object_set_class o3 ("Function")) in
        let o5 = (object_set_proto o4 (Value_object (Object_loc_prealloc Prealloc_function_proto))) in
        let o6 = (object_with_invokation o5 (Some Construct_after_bind) (Some Call_after_bind) (Some Builtin_has_instance_after_bind)) in
        let o7 = (object_set_extensible o6 true) in
        let (l, s_2) = object_alloc s o7 in
        let vlength = (
          let%some class0 = (run_object_method object_class_ s_2 thisobj) in
          if string_eq class0 ("Function")
          then
            let%number (s10, n) = (run_object_get s_2 c thisobj ("length")) in
            let%spec (s11, ilen) = (to_int32 s10 c (Value_number n)) in
            if  ilen < (number_of_int (LibList.length a))
            then res_spec s11 0.
            else res_spec s11 (ilen -.  (number_of_int (LibList.length a)))
          else res_spec s_2 0.) in
        let%spec (s10, length0) = (vlength) in
        let a0 = ({ attributes_data_value = (Value_number  length0);
                    attributes_data_writable = false;
                    attributes_data_enumerable = false;
                    attributes_data_configurable = false }) in
        let%some s11 = (run_object_heap_map_properties s10 l
          (fun p -> HeapStr.write p ("length") (Attributes_data_of a0))) in
        let vthrower = (Value_object (Object_loc_prealloc Prealloc_throw_type_error)) in
        let a1 = ({ attributes_accessor_get = vthrower;
                    attributes_accessor_set = vthrower;
                    attributes_accessor_enumerable = false;
                    attributes_accessor_configurable = false }) in
        let%bool (s12, x) = (object_define_own_prop s11 c l ("caller") (descriptor_of_attributes (Attributes_accessor_of a1)) false) in
        let%bool (s13, x0) = (object_define_own_prop s12 c l ("arguments") (descriptor_of_attributes (Attributes_accessor_of a1)) false) in
        res_ter s13 (res_val (Value_object l))
      | _ ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
          s
          ("Value is callable, but isn\'t an object.")
      )
    else run_error s c Native_error_type
  | Prealloc_bool ->
      let v = get_arg 0 args in
      res_out s (res_val (Value_bool (to_boolean v)))
  | Prealloc_bool_proto_to_string ->
    (match vthis with
     | Value_undef -> run_error s c Native_error_type
     | Value_null -> run_error s c Native_error_type
     | Value_bool b0 -> res_ter s (res_val (Value_string (if b0 then "true" else "false")))
     | Value_number n -> run_error s c Native_error_type
     | Value_string s0 -> run_error s c Native_error_type
     | Value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s c Native_error_type) (fun s0 ->
           if string_eq s0 ("Boolean")
           then ifx_some_or_default (run_object_method object_prim_value_ s l)
             (run_error s c Native_error_type) (fun wo ->
               match wo with
               | Some v ->
                 (match v with
                 | Value_undef -> run_error s c Native_error_type
                 | Value_null -> run_error s c Native_error_type
                 | Value_bool b0 ->
                   res_ter s
                     (res_val (Value_string (if b0 then "true" else "false")))
                 | Value_number n ->
                   run_error s c Native_error_type
                 | Value_string s1 ->
                   run_error s c Native_error_type
                 | Value_object o -> run_error s c Native_error_type)
               | None -> run_error s c Native_error_type)
             else run_error s c Native_error_type))
  | Prealloc_bool_proto_value_of ->
    (match vthis with
     | Value_undef -> run_error s c Native_error_type
     | Value_null -> run_error s c Native_error_type
     | Value_bool b0 ->
       res_ter s (res_val (Value_bool b0))
     | Value_number n -> run_error s c Native_error_type
     | Value_string s0 -> run_error s c Native_error_type
     | Value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s c Native_error_type) (fun s0 ->
             if string_eq s0 ("Boolean")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
               (run_error s c Native_error_type) (fun wo ->
                 match wo with
                 | Some v ->
                   (match v with
                    | Value_undef -> run_error s c Native_error_type
                    | Value_null -> run_error s c Native_error_type
                    | Value_bool b0 -> res_ter s (res_val (Value_bool b0))
                    | Value_number n -> run_error s c Native_error_type
                    | Value_string s1 -> run_error s c Native_error_type
                    | Value_object o -> run_error s c Native_error_type)
                 | None -> run_error s c Native_error_type)
             else run_error s c Native_error_type))
  | Prealloc_number ->
    if list_eq_nil_decidable args
    then res_out s (res_val (Value_number JsNumber.zero))
    else let v = get_arg 0 args in to_number s v
  | Prealloc_number_proto_value_of ->
    (match vthis with
     | Value_undef -> run_error s c Native_error_type
     | Value_null -> run_error s c Native_error_type
     | Value_bool b0 -> run_error s c Native_error_type
     | Value_number n ->
       res_ter s (res_val (Value_number n))
     | Value_string s0 -> run_error s c Native_error_type
     | Value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s c Native_error_type) (fun s0 ->
             if string_eq s0 ("Number")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
                 (run_error s c Native_error_type) (fun wo ->
                     match wo with
                     | Some v ->
                       (match v with
                        | Value_undef -> run_error s c Native_error_type
                        | Value_null -> run_error s c Native_error_type
                        | Value_bool b0 -> run_error s c Native_error_type
                        | Value_number n -> res_ter s (res_val (Value_number n))
                        | Value_string s1 -> run_error s c Native_error_type
                        | Value_object o -> run_error s c Native_error_type)
                     | None -> run_error s c Native_error_type)
             else run_error s c Native_error_type))
  | Prealloc_array ->
    run_construct_prealloc s c Prealloc_array args
  | Prealloc_array_is_array ->
    let  arg = (get_arg 0 args) in begin
        match arg with
        | Value_object arg0 ->
          let%some class0= (run_object_method object_class_ s arg0) in
          if string_eq class0 ("Array")
          then res_ter s (res_val (Value_bool true))
          else res_ter s (res_val (Value_bool false))
        | _ -> res_ter s (res_val (Value_bool false))
    end
  | Prealloc_array_proto_to_string ->
    let%object (s0, array) = (to_object s vthis) in
    let%value (s1, vfunc) = (run_object_get s0 c array ("join")) in
    if is_callable_dec s1 vfunc
    then (match vfunc with
        | Value_object func -> run_call s1 c func (Value_object array) []
        | _ ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Result_impossible)
            s1
            ("Value is callable, but isn\'t an object."))
    else run_call_prealloc s1 c Prealloc_object_proto_to_string
        (Object_loc_prealloc Prealloc_object_proto_to_string) (Value_object array) []
  | Prealloc_array_proto_join ->
    let  vsep = (get_arg 0 args) in
    let%object (s0, l) = (to_object s vthis) in
    let%value (s1, vlen) = (run_object_get s0 c l ("length")) in
    let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
    let rsep = (if not (value_compare vsep Value_undef) then vsep else Value_string (",")) in
    let%string (s3, sep) = (to_string s2 rsep) in
    if ilen = 0.0
    then res_ter s3 (res_val (Value_string ""))
    else
      let sR = (valueToStringForJoin s3 c l 0.) in
      let%spec (s4, sR0) = (sR) in
      run_array_join_elements s4 c l 1. ilen sep sR0
  | Prealloc_array_proto_pop ->
    let%object (s0, l) = (to_object s vthis) in
    let%value (s1, vlen) = (run_object_get s0 c l ("length")) in
    let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
    if ilen = 0.0
    then
      let%not_throw (s3, x) = (object_put s2 c l ("length") (Value_number JsNumber.zero) throw_true) in
      res_out s3 (res_val Value_undef)
    else
      let%string (s3, sindx) = (to_string s2 (Value_number (ilen -. 1.))) in
      let%value (s4, velem) = (run_object_get s3 c l sindx) in
      let%not_throw (s5, x) = (object_delete s4 c l sindx throw_true) in
      let%not_throw (s6, x0) = (object_put s5 c l ("length") (Value_string sindx) throw_true) in
      res_out s6 (res_val velem)
  | Prealloc_array_proto_push ->
    let%object (s0, l) = (to_object s vthis) in
    let%value (s1, vlen) = (run_object_get s0 c l ("length")) in
    let%spec (s2, ilen) = (to_uint32 s1 c vlen) in
    push s2 c l args ilen
  | Prealloc_string ->
    if list_eq_nil_decidable args
    then res_ter s (res_val (Value_string ""))
    else
      let value0 = (get_arg 0 args) in
      let%string (s0, s1) = (to_string s value0) in
      res_ter s0 (res_val (Value_string s1))
  | Prealloc_string_proto_to_string ->
    (match vthis with
     | Value_object l ->
       let%some s0= (run_object_method object_class_ s l) in
           if string_eq s0 ("String")
           then run_object_prim_value s l
           else run_error s c Native_error_type
     | _ ->
       if type_compare (type_of vthis) Type_string
       then res_ter s (res_val vthis)
       else run_error s c Native_error_type
    )
  | Prealloc_string_proto_value_of ->
    (match vthis with
     | Value_object l ->
       let%some s0= (run_object_method object_class_ s l) in
           if string_eq s0 ("String")
           then run_object_prim_value s l
           else run_error s c Native_error_type
     | _ ->
       if type_compare (type_of vthis) Type_string
       then res_ter s (res_val vthis)
       else run_error s c Native_error_type
    )
  | Prealloc_error ->
    let  v = (get_arg 0 args) in
        build_error s c (Value_object (Object_loc_prealloc
                                           Prealloc_error_proto)) v
  | Prealloc_native_error ne ->
    let  v = (get_arg 0 args) in
    build_error s c (Value_object (Object_loc_prealloc
                                       (Prealloc_native_error_proto ne))) v
  | Prealloc_throw_type_error -> builtin_throw_type_error s c () () ()
  | Prealloc_proxy -> builtin_proxy_constructor s c () () Value_undef (get_arg 0 args) (get_arg 1 args)
  | Prealloc_proxy_revocable -> builtin_proxy_revocable s c () () () (get_arg 0 args) (get_arg 1 args)
  | Builtin_proxy_revocation -> builtin_proxy_revocation_function s c l () ()
  | Prealloc_global_eval -> run_eval s c false args
  | Prealloc_reflect_apply -> builtin_reflect_apply s c () () () (get_arg 0 args) (get_arg 1 args) (get_arg 2 args)
  | Prealloc_reflect_construct -> builtin_reflect_construct s c () () () (get_arg 0 args) (get_arg 1 args) (get_arg_opt 2 args)
  | Prealloc_reflect_define_property -> builtin_reflect_define_property s c () () () (get_arg 0 args) (get_arg 1 args) (get_arg 2 args)
  | Prealloc_reflect_delete_property -> builtin_reflect_delete_property s c () () () (get_arg 0 args) (get_arg 1 args)
  | Prealloc_reflect_get -> builtin_reflect_get s c () () () (get_arg 0 args) (get_arg 1 args) (get_arg_opt 2 args)
  | Prealloc_reflect_get_own_property_descriptor -> builtin_reflect_get_own_property_descriptor s c () () () (get_arg 0 args) (get_arg 1 args)
  | Prealloc_reflect_get_prototype_of -> builtin_reflect_get_prototype_of s c () () () (get_arg 0 args)
  | Prealloc_reflect_has -> builtin_reflect_has s c () () () (get_arg 0 args) (get_arg 1 args)
  | Prealloc_reflect_is_extensible -> builtin_reflect_is_extensible s c () () () (get_arg 0 args)
  | Prealloc_reflect_own_keys -> builtin_reflect_own_keys s c () () () (get_arg 0 args)
  | Prealloc_reflect_prevent_extensions -> builtin_reflect_prevent_extensions s c () () () (get_arg 0 args)
  | Prealloc_reflect_set -> builtin_reflect_set s c () () () (get_arg 0 args) (get_arg 1 args) (get_arg 2 args) (get_arg_opt 3 args)
  | Prealloc_reflect_set_prototype_of -> builtin_reflect_set_prototype_of s c () () () (get_arg 0 args) (get_arg 1 args)
  | _ ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Result_not_yet_implemented)
      (strappend ("Call prealloc_") (strappend (string_of_prealloc b) (" not yet implemented")))

(** val run_call :
    state -> execution_ctx -> object_loc -> value -> value list
    -> result **)

and run_call s c l vthis args =
  let%some co = run_object_method object_call_ s l in
  let%some c0 = co in
  match c0 with
  | Call_default -> entering_func_code s c l vthis args
  | Call_after_bind ->
    let%some oarg = run_object_method object_bound_args_ s l in
    let%some boundArgs = oarg in
    let%some obnd = run_object_method object_bound_this_ s l in
    let%some boundThis = obnd in
    let%some otrg = run_object_method object_target_function_ s l in
    let%some target = otrg in
    let arguments_ = (LibList.append boundArgs args) in run_call s c target boundThis arguments_
  | Call_prealloc b -> run_call_prealloc s c b l vthis args
  | _ -> object_internal_call s l vthis args

(** val run_javascript_from_state : state -> prog -> result **)

and run_javascript_from_state s _term_ =
  let c = execution_ctx_initial (prog_intro_strictness _term_) in
  let%void s_2 = execution_ctx_binding_inst s c Codetype_global None _term_ [] in
  run_prog s_2 c _term_

(** val run_javascript_from_result : result -> prog -> result **)

and run_javascript_from_result w _term_ =
  if_success w (fun s _ -> run_javascript_from_state s _term_)

(** val run_javascript : prog -> result **)

and run_javascript _term_ =
  run_javascript_from_state state_initial _term_
