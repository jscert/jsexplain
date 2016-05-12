open Datatypes
open JsCommon
open JsCommonAux
open JsInit
open JsInterpreterMonads
open JsPreliminary
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open LibProd
open List0
open Shared

(*------------JS preliminary -----------*)

(** val convert_number_to_bool : number -> bool **)

let convert_number_to_bool n =
  if    (number_comparable n JsNumber.zero) 
     || (number_comparable n JsNumber.neg_zero)
     || (number_comparable n JsNumber.nan)
  then false
  else true

(** val convert_string_to_bool : string -> bool **)

let convert_string_to_bool s =
  if string_eq s "" then false else true
  (* Arthur hack string.empty *)

(** val convert_prim_to_boolean : prim -> bool **)

let convert_prim_to_boolean _foo_ = match _foo_ with
| Coq_prim_undef -> false
| Coq_prim_null -> false
| Coq_prim_bool b -> b
| Coq_prim_number n -> convert_number_to_bool n
| Coq_prim_string s -> convert_string_to_bool s

(** val convert_value_to_boolean : value -> bool **)

let convert_value_to_boolean _foo_ = match _foo_ with
| Coq_value_prim p -> convert_prim_to_boolean p
| Coq_value_object o -> true

(** val convert_prim_to_number : prim -> number **)

let convert_prim_to_number _foo_ = match _foo_ with
| Coq_prim_undef -> JsNumber.nan
| Coq_prim_null -> JsNumber.zero
| Coq_prim_bool b -> if b then JsNumber.one else JsNumber.zero
| Coq_prim_number n -> n
| Coq_prim_string s -> JsNumber.from_string s

(** val convert_number_to_integer : number -> number **)

let convert_number_to_integer n =
  if number_comparable n JsNumber.nan
  then JsNumber.zero
  else if   (number_comparable n JsNumber.zero)
         || (number_comparable n JsNumber.neg_zero)
         || (number_comparable n JsNumber.infinity)
         || (number_comparable n JsNumber.neg_infinity)
       then n
       else  (JsNumber.sign n) *. (JsNumber.floor (JsNumber.absolute n))

(** val convert_bool_to_string : bool -> string **)

let convert_bool_to_string _foo_ = match _foo_ with
| true -> "true"
| false -> "false"

(** val convert_prim_to_string : prim -> string **)

let convert_prim_to_string _foo_ = match _foo_ with
| Coq_prim_undef ->
  "undefined"
| Coq_prim_null -> "null"
| Coq_prim_bool b -> convert_bool_to_string b
| Coq_prim_number n -> JsNumber.to_string n
| Coq_prim_string s -> s

(** val equality_test_for_same_type : coq_type -> value -> value -> bool **)

let equality_test_for_same_type ty v1 v2 =
  match ty with
  | Coq_type_undef -> true
  | Coq_type_null -> true
  | Coq_type_bool -> value_compare v1 v2
  | Coq_type_number ->
    (match v1 with
     | Coq_value_prim p ->
       (match p with
        | Coq_prim_undef -> false
        | Coq_prim_null -> false
        | Coq_prim_bool b -> false
        | Coq_prim_number n1 ->
          (match v2 with
           | Coq_value_prim p0 ->
             (match p0 with
              | Coq_prim_undef -> false
              | Coq_prim_null -> false
              | Coq_prim_bool b -> false
              | Coq_prim_number n2 ->
                if number_comparable n1 JsNumber.nan
                then false
                else if number_comparable n2 JsNumber.nan
                     then false
                     else if   (number_comparable n1 JsNumber.zero)
                            && (number_comparable n2 JsNumber.neg_zero)
                          then true
                          else if   (number_comparable n1 JsNumber.neg_zero)
                                &&  (number_comparable n2 JsNumber.zero)
                               then true
                               else number_comparable n1 n2
              | Coq_prim_string s -> false)
           | Coq_value_object o -> false)
        | Coq_prim_string s -> false)
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
  if (number_comparable n1 JsNumber.nan) || (number_comparable n2 JsNumber.nan)
  then Coq_prim_undef
  else if number_comparable n1 n2
       then Coq_prim_bool false
       else if   (number_comparable n1 JsNumber.zero)
              && (number_comparable n2 JsNumber.neg_zero)
            then Coq_prim_bool false
            else if (number_comparable n1 JsNumber.neg_zero)
                 && (number_comparable n2 JsNumber.zero)
                 then Coq_prim_bool false
                 else if number_comparable n1 JsNumber.infinity
                      then Coq_prim_bool false
                      else if number_comparable n2 JsNumber.infinity
                           then Coq_prim_bool true
                           else if number_comparable n2 JsNumber.neg_infinity
                                then Coq_prim_bool false
                                else if number_comparable n1 JsNumber.neg_infinity
                                     then Coq_prim_bool true
                                     else Coq_prim_bool (n1 < n2)

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
let inequality_test_string s1 s2 = (not (string_eq s1 s2))


(** val inequality_test_primitive : prim -> prim -> prim **)

let inequality_test_primitive w1 w2 =
  match w1 with
  | Coq_prim_undef ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_null ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_bool b ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_number n ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_string s1 ->
    (match w2 with
     | Coq_prim_undef ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_null ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_bool b ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_number n ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_string s2 -> Coq_prim_bool (inequality_test_string s1 s2))

(** val typeof_prim : prim -> string **)

let typeof_prim _foo_ = match _foo_ with
| Coq_prim_undef ->
  "undefined"
| Coq_prim_null -> "object"
| Coq_prim_bool b -> "boolean"
| Coq_prim_number n -> "number"
| Coq_prim_string s -> "string"

(** val string_of_propname : propname -> prop_name **)

let string_of_propname _foo_ = match _foo_ with
| Coq_propname_identifier s -> s
| Coq_propname_string s -> s
| Coq_propname_number n -> JsNumber.to_string n

(*---------------------------------*)

(** val build_error : state -> value -> value -> result **)

let build_error s vproto vmsg =
  let o = object_new vproto ("Error") in
  let (l, s_2) = object_alloc s o in
  if value_compare vmsg (Coq_value_prim Coq_prim_undef)
  then result_out (Coq_out_ter (s_2, (res_val (Coq_value_object l))))
  else (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_impossible)
         ("Need [to_string] (this function shall be put in [runs_type].)")

(** val run_error : state -> native_error -> 'a1 specres **)

let run_error s ne =
  let%object (s_2, l) = (build_error s (Coq_value_object (Coq_object_loc_prealloc
                                                            (Coq_prealloc_native_error_proto ne))) (Coq_value_prim Coq_prim_undef)) in
  Coq_result_some (Coq_specret_out (Coq_out_ter (s_2,
                                                 (res_throw (Coq_resvalue_value (Coq_value_object l))))))

(** val out_error_or_void :
    state -> strictness_flag -> native_error -> result **)

let out_error_or_void s str ne =
  if str then run_error s ne else result_out (out_void s)

(** val out_error_or_cst :
    state -> strictness_flag -> native_error -> value -> result **)

let out_error_or_cst s str ne v =
  if str then run_error s ne else result_out (Coq_out_ter (s, (res_val v)))

(** val run_object_method :
    (coq_object -> 'a1) -> state -> object_loc -> 'a1 option **)

let run_object_method proj s l =
  LibOption.map proj (object_binds_pickable_option s l)

(*---DEBUG
  let run_object_method proj s l =
   let opt = object_binds_pickable_option s l in
     begin match opt with
       | None -> Debug.run_object_method l
       | _ -> ()
     end;
     LibOption.map proj opt
*)


(** val run_object_heap_set_extensible :
    bool -> state -> object_loc -> state option **)

let run_object_heap_set_extensible b s l =
  LibOption.map (fun o -> object_write s l (object_set_extensible o b))
    (object_binds_pickable_option s l)

(* DEBUG
   let run_object_heap_set_extensible b s l =
   let opt = object_binds_pickable_option s l in
     begin match opt with
       | None -> Debug.run_object_heap_set_extensible l
       | _ -> ()
     end;
     LibOption.map (fun o -> object_write s l (object_set_extensible o b)) opt
*)

(** val object_has_prop :
    state -> execution_ctx -> object_loc -> prop_name -> result **)

let rec object_has_prop s c l x =
  let%some b = (run_object_method object_has_prop_ s l) in
  match b with Coq_builtin_has_prop_default ->
    let%run (s1, d) = (run_object_get_prop s c l x) in
    res_ter s1
      (res_val (Coq_value_prim (Coq_prim_bool
                                  (not
                                     (full_descriptor_compare d Coq_full_descriptor_undef)))))

(** val object_get_builtin :
    state -> execution_ctx -> builtin_get -> value -> object_loc
    -> prop_name -> result **)

and object_get_builtin s c b vthis l x =
  let def s0 l0 =
    let%run (s1, d) = (run_object_get_prop s0 c l0 x) in
    match d with
    | Coq_full_descriptor_undef ->
      res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
    | Coq_full_descriptor_some a ->
      (match a with
       | Coq_attributes_data_of ad ->
         res_ter s1 (res_val ad.attributes_data_value)
       | Coq_attributes_accessor_of aa ->
         (match aa.attributes_accessor_get with
          | Coq_value_prim p ->
            (match p with
             | Coq_prim_undef ->
               res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
             | Coq_prim_null -> Coq_result_impossible
             | Coq_prim_bool b0 -> Coq_result_impossible
             | Coq_prim_number n -> Coq_result_impossible
             | Coq_prim_string s2 -> Coq_result_impossible)
          | Coq_value_object lf -> run_call s1 c lf vthis [])) in
  let function0 s0 =
    let%value (s_2, v) = (def s0 l) in
    if spec_function_get_error_case_dec s_2 x v
    then run_error s_2 Coq_native_error_type
    else res_ter s_2 (res_val v) in
  match b with
  | Coq_builtin_get_default -> def s l
  | Coq_builtin_get_function -> function0 s
  | Coq_builtin_get_args_obj ->
    let%some lmapo = (run_object_method object_parameter_map_ s l) in
    let%some lmap = (lmapo) in
    let%run (s0, d) = (run_object_get_own_prop s c lmap x) in
    match d with
    | Coq_full_descriptor_undef -> function0 s0
    | Coq_full_descriptor_some a -> run_object_get s0 c lmap x

(** val run_object_get :
    state -> execution_ctx -> object_loc -> prop_name -> result **)

and run_object_get s c l x =
  let%some b = (run_object_method object_get_ s l) in
  object_get_builtin s c b (Coq_value_object l) l x

(** val run_object_get_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

and run_object_get_prop s c l x =
  let%some b = (run_object_method object_get_prop_ s l) in
  match b with Coq_builtin_get_prop_default ->
    let%run (s1, d) = (run_object_get_own_prop s c l x) in
    if full_descriptor_compare d Coq_full_descriptor_undef
    then let%some proto = (run_object_method object_proto_ s1 l) in
      match proto with
      | Coq_value_prim p ->
        (match p with
         | Coq_prim_null -> res_spec s1 Coq_full_descriptor_undef
         | _ ->
           (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
             s1
             ("Found a non-null primitive value as a prototype in [run_object_get_prop]."))
      | Coq_value_object lproto ->
        run_object_get_prop s1 c lproto x
    else res_spec s1 d

(** val object_proto_is_prototype_of :
    state -> object_loc -> object_loc -> result **)

and object_proto_is_prototype_of s l0 l =
  let%some b = (run_object_method object_proto_ s l) in
  match b with
  | Coq_value_prim p ->
    (match p with
     | Coq_prim_null ->
       result_out (Coq_out_ter (s,
                                (res_val (Coq_value_prim (Coq_prim_bool false)))))
     | _ ->
       (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
         s
         ("[run_object_method] returned a primitive in [object_proto_is_prototype_of_body]."))
  | Coq_value_object l_2 ->
    if object_loc_compare l_2 l0
    then result_out (Coq_out_ter (s,
                                  (res_val (Coq_value_prim (Coq_prim_bool true)))))
    else object_proto_is_prototype_of s l0 l_2

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
        let%object (s2, lfunc) = (result_out (Coq_out_ter (s1, (res_val vfo)))) in
        let%value (s3, v) = (run_call s2 c lfunc (Coq_value_object l) []) in begin
          match v with
          | Coq_value_prim w ->
            result_out (Coq_out_ter (s3, (res_val (Coq_value_prim w))))
          | Coq_value_object l0 -> k s3
        end
      | None -> k s1 in
    let gmeth = (method_of_preftype gpref) in
    sub0 s gmeth (fun s_2 ->
        let lmeth = method_of_preftype lpref in
        sub0 s_2 lmeth (fun s_3 -> run_error s_3 Coq_native_error_type))

(** val to_primitive :
    state -> execution_ctx -> value -> preftype option -> result **)

and to_primitive s c v prefo =
  match v with
  | Coq_value_prim w ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim w))))
  | Coq_value_object l ->
    let%prim (s0, r) = (object_default_value s c l prefo) in
    res_ter s0 (res_val (Coq_value_prim r))

(** val to_number :
    state -> execution_ctx -> value -> result **)

and to_number s c _foo_ = match _foo_ with
  | Coq_value_prim w ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim (Coq_prim_number (convert_prim_to_number w))))))
  | Coq_value_object l ->
    let%prim (s1, w) = (to_primitive s c (Coq_value_object l) (Some Coq_preftype_number)) in
    res_ter s1 (res_val (Coq_value_prim (Coq_prim_number (convert_prim_to_number w))))

(** val to_integer :
    state -> execution_ctx -> value -> result **)

and to_integer s c v =
  let%number (s1, n) = to_number s c v in
  res_ter s1
    (res_val (Coq_value_prim (Coq_prim_number
                                (convert_number_to_integer n))))

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
  | Coq_value_prim w ->
    result_out (Coq_out_ter (s,
                             (res_val (Coq_value_prim (Coq_prim_string (convert_prim_to_string w))))))
  | Coq_value_object l ->
    let%prim (s1, w) = (to_primitive s c (Coq_value_object l) (Some Coq_preftype_string)) in
    res_ter s1
      (res_val (Coq_value_prim (Coq_prim_string (convert_prim_to_string w))))

(** val object_can_put :
    state -> execution_ctx -> object_loc -> prop_name -> result **)

and object_can_put s c l x =
  let%some b = (run_object_method object_can_put_ s l) in
  match b with Coq_builtin_can_put_default ->
    let%run (s1, d) = (run_object_get_own_prop s c l x) in begin
      match d with
      | Coq_full_descriptor_undef ->
        let%some vproto = (run_object_method object_proto_ s1 l) in begin
          match vproto with
          | Coq_value_prim p ->
            (match p with
             | Coq_prim_null ->
               let%some b0= (run_object_method object_extensible_ s1 l) in
               res_ter s1 (res_val (Coq_value_prim (Coq_prim_bool b0)))
             | _ ->
               (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                 s1
                 ("Non-null primitive get as a prototype value in [object_can_put]."))
          | Coq_value_object lproto ->
            let%run (s2, d_2) = (run_object_get_prop s1 c lproto x) in
            match d_2 with
            | Coq_full_descriptor_undef ->
              let%some b0= (run_object_method object_extensible_ s2 l) in
              res_ter s2 (res_val (Coq_value_prim (Coq_prim_bool b0)))
            | Coq_full_descriptor_some a ->
              (match a with
               | Coq_attributes_data_of ad ->
                 let%some ext = (run_object_method object_extensible_ s2 l) in
                 res_ter s2
                   (if ext
                    then res_val (Coq_value_prim (Coq_prim_bool
                                                    ad.attributes_data_writable))
                    else res_val (Coq_value_prim (Coq_prim_bool false)))
               | Coq_attributes_accessor_of aa ->
                 res_ter s2
                   (res_val (Coq_value_prim (Coq_prim_bool
                                               (not
                                                  (value_compare aa.attributes_accessor_set
                                                     (Coq_value_prim Coq_prim_undef)))))))
        end
      | Coq_full_descriptor_some a ->
        (match a with
         | Coq_attributes_data_of ad ->
           res_ter s1
             (res_val (Coq_value_prim (Coq_prim_bool
                                         ad.attributes_data_writable)))
         | Coq_attributes_accessor_of aa ->
           res_ter s1
             (res_val (Coq_value_prim (Coq_prim_bool
                                         (not
                                            (value_compare aa.attributes_accessor_set (Coq_value_prim
                                                                                            Coq_prim_undef)))))))
    end

(** val run_object_define_own_prop_array_loop :
    state -> execution_ctx -> object_loc -> float -> float ->
    descriptor -> bool -> bool -> (state -> prop_name -> descriptor ->
    strictness_flag -> __ specres) -> result **)

and run_object_define_own_prop_array_loop s c l newLen oldLen newLenDesc newWritable throwcont def =
  if newLen < oldLen
  then let  oldLen_2 = (oldLen -. 1.) in
    let%string (s0, slen) = (to_string s c (Coq_value_prim (Coq_prim_number
                                                              (of_int oldLen_2)))) in
    let%bool (s1, deleteSucceeded) = (object_delete s0 c l slen false) in
    if not deleteSucceeded
    then let newLenDesc0 =
           (descriptor_with_value
              newLenDesc
              (Some (Coq_value_prim
                       (Coq_prim_number (of_int (oldLen_2 +. 1.)))))) in
      let newLenDesc1 = (if not newWritable
                         then descriptor_with_writable newLenDesc0 (Some false)
                         else newLenDesc0) in
      let%bool (s2, x) = (def s1 ("length")
                            newLenDesc1 false) in
      out_error_or_cst s2 throwcont Coq_native_error_type
        (Coq_value_prim (Coq_prim_bool false))
    else run_object_define_own_prop_array_loop s1 c l
        newLen oldLen_2 newLenDesc newWritable throwcont def
  else if not newWritable
  then def s ("length")
      { descriptor_value = None; descriptor_writable = (Some false);
        descriptor_get = None; descriptor_set = None;
        descriptor_enumerable = None; descriptor_configurable = None }
      false
  else res_ter s (res_val (Coq_value_prim (Coq_prim_bool true)))

(** val object_define_own_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    descriptor -> strictness_flag -> result **)

and object_define_own_prop s c l x desc throwcont =
  let reject s0 throwcont0 =
    out_error_or_cst
      s0 throwcont0 Coq_native_error_type (Coq_value_prim
                                             (Coq_prim_bool false)) in
  let def s0 x0 desc0 throwcont0 =
    let%run (s1, d) = (run_object_get_own_prop s0 c l x0) in
    let%some ext = (run_object_method object_extensible_ s1 l) in
    match d with
    | Coq_full_descriptor_undef ->
      if ext
      then let a = (if (descriptor_is_generic_dec desc0) || (descriptor_is_data_dec desc0)
                    then Coq_attributes_data_of
                        (attributes_data_of_descriptor desc0)
                    else Coq_attributes_accessor_of
                        (attributes_accessor_of_descriptor desc0)) in
        let%some s2 = (object_heap_map_properties_pickable_option s1 l
                         (fun p -> HeapStr.write p x0 a)) in
        res_ter s2
          (res_val (Coq_value_prim (Coq_prim_bool true)))
      else reject s1 throwcont0
    | Coq_full_descriptor_some a ->
      let object_define_own_prop_write s2 a0 =
          let a_2 = attributes_update a0 desc0 in
          let%some s3 = (object_heap_map_properties_pickable_option s2 l (fun p -> HeapStr.write p x0 a_2)) in
          res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool true))) in
      if descriptor_contains_dec (descriptor_of_attributes a) desc0
      then res_ter s1 (res_val (Coq_value_prim (Coq_prim_bool true)))
      else if attributes_change_enumerable_on_non_configurable_dec a desc0
      then reject s1 throwcont0
      else if descriptor_is_generic_dec desc0
      then object_define_own_prop_write s1 a
      else if not (bool_eq (attributes_is_data_dec a) (descriptor_is_data_dec desc0))
      then if attributes_configurable a
        then let a_2 = (match a with
            | Coq_attributes_data_of ad ->
              Coq_attributes_accessor_of (attributes_accessor_of_attributes_data ad)
            | Coq_attributes_accessor_of aa ->
              Coq_attributes_data_of (attributes_data_of_attributes_accessor aa)) in
          let%some s2 = (object_heap_map_properties_pickable_option
                           s1 l (fun p -> HeapStr.write p x0 a_2)) in
          object_define_own_prop_write s2 a_2
        else reject s1 throwcont0
      else if (attributes_is_data_dec a) && (descriptor_is_data_dec desc0)
      then (match a with
          | Coq_attributes_data_of ad ->
            if attributes_change_data_on_non_configurable_dec ad desc0
            then reject s1 throwcont0
            else object_define_own_prop_write s1 a
          | Coq_attributes_accessor_of a0 ->
            (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
              s0
              ("data is not accessor in [defineOwnProperty]"))
      else if (not (attributes_is_data_dec a)) && (descriptor_is_accessor_dec desc0)
      then (match a with
          | Coq_attributes_data_of a0 ->
            (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
              s0
              ("accessor is not data in [defineOwnProperty]")
          | Coq_attributes_accessor_of aa ->
            if attributes_change_accessor_on_non_configurable_dec aa desc0
            then reject s1 throwcont0
            else object_define_own_prop_write s1 a)
      else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s0
          ("cases are mutually exclusives in [defineOwnProperty]") in
  let%some b = (run_object_method object_define_own_prop_ s l) in
  match b with
  | Coq_builtin_define_own_prop_default -> def s x desc throwcont
  | Coq_builtin_define_own_prop_array ->
    let%run (s0, d) = (run_object_get_own_prop s c l ("length")) in begin
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
             | Coq_value_prim w ->
               let oldLen0 = (JsNumber.to_uint32 (convert_prim_to_number w)) in
               let descValueOpt = (desc.descriptor_value) in
               if string_eq x ("length")
               then (match descValueOpt with
                   | Some descValue ->
                     let%run (s1, newLen) = (to_uint32 s0 c descValue) in
                     let%number (s2, newLenN) = to_number s1 c descValue in
                     if not (number_comparable (of_int newLen) newLenN)
                     then run_error s2 Coq_native_error_range
                     else let newLenDesc =
                            (descriptor_with_value desc (Some (Coq_value_prim (Coq_prim_number (of_int newLen))))) in
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
                         then res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool false)))
                         else run_object_define_own_prop_array_loop s3 c l newLen oldLen0 newLenDesc0 newWritable throwcont def
                   | None -> def s0 ("length") desc throwcont)
               else let%run (s1, ilen) = (to_uint32 s0 c (Coq_value_prim (Coq_prim_string x))) in
                 let%string (s2, slen) = (to_string s1 c (Coq_value_prim (Coq_prim_number (of_int ilen)))) in
                 if (string_eq x slen) && (not ( ilen = 4294967295.))
                 then let%run (s3, index) = (to_uint32 s2 c (Coq_value_prim (Coq_prim_string x))) in
                   if  (le_int_decidable oldLen0 index) && (not a.attributes_data_writable)
                   then reject s3 throwcont
                   else let%bool (s4, b0) = (def s3 x desc false) in
                     if not b0
                     then reject s4 throwcont
                     else if le_int_decidable oldLen0 index
                     then let a0 =
                            descriptor_with_value (descriptor_of_attributes (Coq_attributes_data_of a))
                              (Some (Coq_value_prim (Coq_prim_number (of_int (index +. 1.))))) in
                       def s4 ("length") a0 false
                     else res_ter s4 (res_val (Coq_value_prim (Coq_prim_bool true)))
                 else def s2 x desc throwcont
             | Coq_value_object l0 ->
               (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                 s0
                 ("Spec asserts length of array is number.")
           end
         | Coq_attributes_accessor_of a ->
           (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
             s0
             ("Array length property descriptor cannot be accessor."))
    end
  | Coq_builtin_define_own_prop_args_obj ->
    let%some lmapo = (run_object_method object_parameter_map_ s l) in
    let%some lmap = (lmapo) in
    let%run (s0, d) = (run_object_get_own_prop s c lmap x) in
    let%bool (s1, b0) = (def s0 x desc false) in
    if b0
    then let follow s2 = res_ter s2 (res_val (Coq_value_prim (Coq_prim_bool true))) in
      match d with
      | Coq_full_descriptor_undef -> follow s1
      | Coq_full_descriptor_some a ->
        if descriptor_is_accessor_dec desc
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

(** val run_to_descriptor :
    state -> execution_ctx -> value -> descriptor specres **)

and run_to_descriptor s c _foo_ = match _foo_ with
  | Coq_value_prim p -> throw_result (run_error s Coq_native_error_type)
  | Coq_value_object l ->
    let sub0 s0 desc name conv k =
      let%bool (s1, has) = object_has_prop s0 c l name in
      if not has
      then k s1 desc
      else let%value (s2, v0) = run_object_get s1 c l name in
        let%run (s3, r) = conv s2 v0 desc in k s3 r
    in
    sub0 s descriptor_intro_empty ("enumerable")
      (fun s1 v1 desc ->
         let b = convert_value_to_boolean v1 in
         res_spec s1 (descriptor_with_enumerable desc (Some b))) (fun s1_2 desc ->
          sub0 s1_2 desc ("configurable")
            (fun s2 v2 desc0 ->
               let b = convert_value_to_boolean v2 in
               res_spec s2 (descriptor_with_configurable desc0 (Some b)))
            (fun s2_2 desc0 ->
               sub0 s2_2 desc0 ("value")
                 (fun s3 v3 desc1 ->
                    res_spec s3 (descriptor_with_value desc1 (Some v3)))
                 (fun s3_2 desc1 ->
                    sub0 s3_2 desc1
                      ("writable")
                      (fun s4 v4 desc2 ->
                         let b = convert_value_to_boolean v4 in
                         res_spec s4 (descriptor_with_writable desc2 (Some b)))
                      (fun s4_2 desc2 ->
                         sub0 s4_2 desc2 ("get") (fun s5 v5 desc3 ->
                             if (not (is_callable_dec s5 v5))
                             && (not (value_compare v5 (Coq_value_prim Coq_prim_undef)))
                             then throw_result (run_error s5 Coq_native_error_type)
                             else res_spec s5 (descriptor_with_get desc3 (Some v5)))
                           (fun s5_2 desc3 ->
                              sub0 s5_2 desc3 ("set") (fun s6 v6 desc4 ->
                                  if (not (is_callable_dec s6 v6))
                                  && (not (value_compare v6 (Coq_value_prim Coq_prim_undef)))
                                  then throw_result (run_error s6 Coq_native_error_type)
                                  else res_spec s6 (descriptor_with_set desc4 (Some v6)))
                                (fun s7 desc4 ->
                                   if ((not (option_compare value_compare desc4.descriptor_get None))
                                       ||
                                       (not (option_compare value_compare desc4.descriptor_set None)))
                                      &&
                                      ((not (option_compare value_compare desc4.descriptor_value None))
                                       ||
                                       (not (option_compare bool_eq desc4.descriptor_writable None)))
                                   then throw_result (run_error s7 Coq_native_error_type)
                                   else res_spec s7 desc4))))))

(** val prim_new_object : state -> prim -> result **)

and prim_new_object s _foo_ = match _foo_ with
  | Coq_prim_bool b ->
    result_out
      (let o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_bool_proto)) ("Boolean")) in
       let o = (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_bool b))) in
       let (l, s1) = object_alloc s o in
       Coq_out_ter (s1, (res_val (Coq_value_object l))))
  | Coq_prim_number n ->
    result_out
      (let o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_number_proto)) ("Number")) in
       let o = (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_number n))) in
       let (l, s1) = object_alloc s o in
       Coq_out_ter (s1, (res_val (Coq_value_object l))))
  | Coq_prim_string s0 ->
    let o2 = (object_new (Coq_value_object (Coq_object_loc_prealloc Coq_prealloc_string_proto)) ("String")) in
    let o1 = (object_with_get_own_property o2 Coq_builtin_get_own_prop_string) in
    let o = (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_string s0))) in
    let (l, s1) = object_alloc s o in
    let%some s_2 = (object_heap_map_properties_pickable_option
                      s1 l
                      (fun p ->
                         HeapStr.write p ("length")
                           (Coq_attributes_data_of
                              (attributes_data_intro_constant
                                 (Coq_value_prim
                                    (Coq_prim_number (number_of_int (strlength s0)))))))) in
    res_ter s_2 (res_val (Coq_value_object l))
  | _ ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
      s
      ("[prim_new_object] received an null or undef.")

(** val to_object : state -> value -> result **)

and to_object s _foo_ = match _foo_ with
  | Coq_value_prim w ->
    (match w with
     | Coq_prim_undef -> run_error s Coq_native_error_type
     | Coq_prim_null -> run_error s Coq_native_error_type
     | Coq_prim_bool b -> prim_new_object s w
     | Coq_prim_number n -> prim_new_object s w
     | Coq_prim_string s0 -> prim_new_object s w)
  | Coq_value_object l ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_object l))))

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
  let%some e = (env_record_binds_pickable_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        result_out (Coq_out_ter (s,
                                 (res_val (Coq_value_prim (Coq_prim_bool
                                                             (HeapStr.indom_dec ed x))))))
      | Coq_env_record_object (l0, pt) -> object_has_prop s c l0 x

(** val lexical_env_get_identifier_ref :
    state -> execution_ctx -> lexical_env -> prop_name ->
    strictness_flag -> ref specres **)

and lexical_env_get_identifier_ref s c x x0 str =
  match x with
  | [] ->
    res_spec s (ref_create_value (Coq_value_prim Coq_prim_undef) x0 str)
  | l :: x_2 ->
    let%bool (s1, has) = (env_record_has_binding s c l x0) in
        if has
        then res_spec s1 (ref_create_env_loc l x0 str)
        else lexical_env_get_identifier_ref s1 c x_2 x0 str

(** val object_delete_default :
    state -> execution_ctx -> object_loc -> prop_name ->
    strictness_flag -> result **)

and object_delete_default s c l x str =
  let%run (s1, d) = (run_object_get_own_prop s c l x) in
      match d with
      | Coq_full_descriptor_undef ->
        res_ter s1 (res_val (Coq_value_prim (Coq_prim_bool true)))
      | Coq_full_descriptor_some a ->
        if attributes_configurable a
        then let%some
             s_2 = (object_heap_map_properties_pickable_option s1 l (fun p ->
                 HeapStr.rem p x)) in
                res_ter s_2 (res_val (Coq_value_prim (Coq_prim_bool true)))
        else out_error_or_cst s1 str Coq_native_error_type (Coq_value_prim
                                                              (Coq_prim_bool false))

(** val object_delete :
    state -> execution_ctx -> object_loc -> prop_name ->
    strictness_flag -> result **)

and object_delete s c l x str =
  let%some b = (run_object_method object_delete_ s l) in
      match b with
      | Coq_builtin_delete_default -> object_delete_default s c l x str
      | Coq_builtin_delete_args_obj ->
        let%some mo = (run_object_method object_parameter_map_ s l) in
            let%some m = (mo) in
                let%run (s1, d) = (run_object_get_own_prop s c m x) in
                    let%bool (s2, b0) = (object_delete_default s1 c l x str) in
                        if b0
                        then (match d with
                            | Coq_full_descriptor_undef ->
                              res_ter s2
                                (res_val (Coq_value_prim (Coq_prim_bool b0)))
                            | Coq_full_descriptor_some a ->
                              let%bool 
                                (s3, b_2) = (object_delete s2 c m x false) in
                                   res_ter s3
                                     (res_val (Coq_value_prim (Coq_prim_bool b0))))
                        else res_ter s2 (res_val (Coq_value_prim (Coq_prim_bool b0)))

(** val env_record_delete_binding :
    state -> execution_ctx -> env_loc -> prop_name -> result **)

and env_record_delete_binding s c l x =
  let%some e = (env_record_binds_pickable_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        (match HeapStr.read_option ed x with
         | Some p ->
           let (mu, v) = p in
           (match mu with
            | Coq_mutability_uninitialized_immutable ->
              result_out (Coq_out_ter (s,
                                       (res_val (Coq_value_prim (Coq_prim_bool false)))))
            | Coq_mutability_immutable ->
              result_out (Coq_out_ter (s,
                                       (res_val (Coq_value_prim (Coq_prim_bool false)))))
            | Coq_mutability_nondeletable ->
              result_out (Coq_out_ter (s,
                                       (res_val (Coq_value_prim (Coq_prim_bool false)))))
            | Coq_mutability_deletable ->
              let s_2 =
                env_record_write s l (Coq_env_record_decl
                                        (decl_env_record_rem ed x))
              in
              result_out (Coq_out_ter (s_2,
                                       (res_val (Coq_value_prim (Coq_prim_bool true))))))
         | None ->
           result_out (Coq_out_ter (s,
                                    (res_val (Coq_value_prim (Coq_prim_bool true))))))
      | Coq_env_record_object (l0, pt) ->
        object_delete s c l0 x throw_false

(** val env_record_implicit_this_value : state -> env_loc -> value option **)

and env_record_implicit_this_value s l =
  ifx_some_or_default (env_record_binds_pickable_option s l) None (fun e ->
      Some
        (match e with
         | Coq_env_record_decl ed -> Coq_value_prim Coq_prim_undef
         | Coq_env_record_object (l0, provide_this) ->
           if provide_this
           then Coq_value_object l0
           else Coq_value_prim Coq_prim_undef))

(** val identifier_resolution :
    state -> execution_ctx -> prop_name -> ref specres **)

and identifier_resolution s c x =
  let x0 = c.execution_ctx_lexical_env in
  let str = c.execution_ctx_strict in
  lexical_env_get_identifier_ref s c x0 x str

(** val env_record_get_binding_value :
    state -> execution_ctx -> env_loc -> prop_name ->
    strictness_flag -> result **)

and env_record_get_binding_value s c l x str =
  let%some e = (env_record_binds_pickable_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        let%some rm = (HeapStr.read_option ed x) in
            let (mu, v) = rm in
            if mutability_compare mu Coq_mutability_uninitialized_immutable
            then out_error_or_cst s str Coq_native_error_ref (Coq_value_prim
                                                                Coq_prim_undef)
            else res_ter s (res_val v)
      | Coq_env_record_object (l0, pt) ->
        let%bool (s1, has) = (object_has_prop s c l0 x) in
            if has
            then run_object_get s1 c l0 x
            else out_error_or_cst s1 str Coq_native_error_ref (Coq_value_prim
                                                                 Coq_prim_undef)

(** val ref_get_value :
    state -> execution_ctx -> resvalue -> value specres **)

and ref_get_value s c _foo_ = match _foo_ with
  | Coq_resvalue_empty ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
      s
      ("[ref_get_value] received an empty result.")
  | Coq_resvalue_value v -> res_spec s v
  | Coq_resvalue_ref r ->
    let 
      for_base_or_object = (fun tt ->
        match r.ref_base with
        | Coq_ref_base_type_value v ->
          if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_primitive_base
          then let%value (s2, v) = (prim_value_get s c v r.ref_name) in res_spec s2 v
          else (match v with
              | Coq_value_prim p ->
                (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                  s
                  ("[ref_get_value] received a primitive value whose kind is not primitive.")
              | Coq_value_object l ->
                let%value (s2, v) = (run_object_get s c l r.ref_name) in res_spec s2 v)
        | Coq_ref_base_type_env_loc l ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("[ref_get_value] received a reference to a value whose base type is an environnment record.")) in
         match ref_kind_of r with
         | Coq_ref_kind_null ->
           (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
             s
             ("[ref_get_value] received a reference whose base is [null].")
         | Coq_ref_kind_undef -> throw_result (run_error s Coq_native_error_ref)
         | Coq_ref_kind_primitive_base -> for_base_or_object ()
         | Coq_ref_kind_object -> for_base_or_object ()
         | Coq_ref_kind_env_record ->
           (match r.ref_base with
            | Coq_ref_base_type_value v ->
              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s
                ("[ref_get_value] received a reference to an environnment record whose base type is a value.")
            | Coq_ref_base_type_env_loc l ->
              let%value
                
                (s2, v) = (env_record_get_binding_value s c l r.ref_name r.ref_strict) in res_spec s2 v)

(* DEBUG
   and ref_get_value runs s c r =
   let res = ref_get_value runs s c r in match res with
   | JsInterpreterMonads.Coq_result_some crs ->
     begin match crs with
       | (Coq_specret_val (_,rs)) ->
         begin match rs with
           | Coq_value_prim cvp ->
             begin match cvp with
               | Coq_prim_undef -> Debug.ref_get_value_2 r; res
               | _ -> res
             end
         | _ -> res
         end
       | _ -> res
     end
     | _ -> res
*)


(** val run_expr_get_value :
    state -> execution_ctx -> expr -> value specres **)

and run_expr_get_value s c e =
  let%success (s0, rv) = (run_expr s c e) in
      ref_get_value s0 c rv

(** val object_put_complete :
    builtin_put -> state -> execution_ctx -> value -> object_loc
    -> prop_name -> value -> strictness_flag -> result_void **)

and object_put_complete b s c vthis l x v str =
  match b with Coq_builtin_put_default ->
    let%bool (s1, b0) = (object_can_put s c l x) in
        if b0
        then let%run (s2, d) = (run_object_get_own_prop s1 c l x) in
            let 
              follow = (fun x0 ->
                let%run (s3, d_2) = (run_object_get_prop s2 c l x) in
                    let  follow_2 = (fun x1 ->
                        match vthis with
                        | Coq_value_prim wthis ->
                          out_error_or_void s3 str Coq_native_error_type
                        | Coq_value_object lthis ->
                          let 
                            desc = (descriptor_intro_data v true true true) in
                               let%success
                                 
                                 (s4, rv) = (object_define_own_prop s3 c l x desc str) in  res_void s4) in
                        match d_2 with
                        | Coq_full_descriptor_undef -> follow_2 ()
                        | Coq_full_descriptor_some a ->
                          (match a with
                           | Coq_attributes_data_of a0 -> follow_2 ()
                           | Coq_attributes_accessor_of aa_2 ->
                             (match aa_2.attributes_accessor_set with
                              | Coq_value_prim p ->
                                (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                                  s3
                                  ("[object_put_complete] found a primitive in an `set\' accessor.")
                              | Coq_value_object lfsetter ->
                                let%success
                                   (s4, rv) = (run_call s3 c lfsetter vthis
                                     (v :: [])) in  res_void s4))) in
                 match d with
                 | Coq_full_descriptor_undef -> follow ()
                 | Coq_full_descriptor_some a ->
                   (match a with
                    | Coq_attributes_data_of ad ->
                      (match vthis with
                       | Coq_value_prim wthis ->
                         out_error_or_void s2 str Coq_native_error_type
                       | Coq_value_object lthis ->
                         let  desc = ({ descriptor_value = (Some v);
                                       descriptor_writable = None; descriptor_get = None;
                                       descriptor_set = None; descriptor_enumerable = None;
                                       descriptor_configurable = None }) in
                             let%success
                               
                               (s3, rv) = (object_define_own_prop s2 c l x desc str) in  res_void s3)
                    | Coq_attributes_accessor_of a0 -> follow ())
        else out_error_or_void s1 str Coq_native_error_type

(** val object_put :
    state -> execution_ctx -> object_loc -> prop_name -> value
    -> strictness_flag -> result_void **)

and object_put s c l x v str =
  let%some b = (run_object_method object_put_ s l) in
      object_put_complete b s c (Coq_value_object l) l x v str

(** val env_record_set_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> value ->
    strictness_flag -> result_void **)

and env_record_set_mutable_binding s c l x v str =
  let%some e = (env_record_binds_pickable_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        let%some rm = (HeapStr.read_option ed x) in
            let (mu, v_old) = rm in
            if not (mutability_compare mu Coq_mutability_immutable)
            then res_void (env_record_write_decl_env s l x mu v)
            else out_error_or_void s str Coq_native_error_type
      | Coq_env_record_object (l0, pt) -> object_put s c l0 x v str

(** val prim_value_put :
    state -> execution_ctx -> prim -> prop_name -> value ->
    strictness_flag -> result_void **)

and prim_value_put s c w x v str =
  let%object (s1, l) = (to_object s (Coq_value_prim w)) in
      object_put_complete Coq_builtin_put_default s1 c (Coq_value_prim w)
        l x v str

(** val ref_put_value :
    state -> execution_ctx -> resvalue -> value -> result_void **)

and ref_put_value s c rv v =
  match rv with
  | Coq_resvalue_empty ->
    (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
      s
      ("[ref_put_value] received an empty result.")
  | Coq_resvalue_value v0 -> run_error s Coq_native_error_ref
  | Coq_resvalue_ref r ->
    if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_undef
    then if r.ref_strict
      then run_error s Coq_native_error_ref
      else object_put s c (Coq_object_loc_prealloc
                             Coq_prealloc_global) r.ref_name v throw_false
    else if 
          (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_primitive_base)
       || (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_null)
       || (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_object)
    then (match r.ref_base with
        | Coq_ref_base_type_value v_2 ->
          if ref_kind_comparable (ref_kind_of r)
              Coq_ref_kind_primitive_base
          then (match v_2 with
              | Coq_value_prim w ->
                prim_value_put s c w r.ref_name v r.ref_strict
              | Coq_value_object o ->
                (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                  s
                  ("[ref_put_value] impossible case"))
          else (match v_2 with
              | Coq_value_prim p ->
                (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                  s
                  ("[ref_put_value] impossible case")
              | Coq_value_object l ->
                object_put s c l r.ref_name v r.ref_strict)
        | Coq_ref_base_type_env_loc l ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("[ref_put_value] contradicts ref_is_property"))
    else (match r.ref_base with
        | Coq_ref_base_type_value v0 ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("[ref_put_value] impossible spec")
        | Coq_ref_base_type_env_loc l ->
          env_record_set_mutable_binding s c l r.ref_name v
            r.ref_strict)

(** val env_record_create_mutable_binding :
    state -> execution_ctx -> env_loc -> prop_name -> bool
    option -> result_void **)

and env_record_create_mutable_binding s c l x deletable_opt =
  let  deletable = (unsome_default false deletable_opt) in
      let%some e = (env_record_binds_pickable_option s l) in
          match e with
          | Coq_env_record_decl ed ->
            if HeapStr.indom_dec ed x
            then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s
                ("Already declared environnment record in [env_record_create_mutable_binding].")
            else let
                 s_2 = (env_record_write_decl_env s l x
                   (mutability_of_bool deletable) (Coq_value_prim
                                                     Coq_prim_undef)) in res_void s_2
          | Coq_env_record_object (l0, pt) ->
            let%bool (s1, has) = (object_has_prop s c l0 x) in
                if has
                then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                    s1
                    ("Already declared binding in [env_record_create_mutable_binding].")
                else let  a = ({ attributes_data_value = (Coq_value_prim
                                                              Coq_prim_undef); attributes_data_writable = true;
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
  let%some e = (env_record_binds_pickable_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        if HeapStr.indom_dec ed x
        then (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("Already declared environnment record in [env_record_create_immutable_binding].")
        else res_void
            (env_record_write_decl_env s l x
               Coq_mutability_uninitialized_immutable (Coq_value_prim
                                                         Coq_prim_undef))
      | Coq_env_record_object (o, p) ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("[env_record_create_immutable_binding] received an environnment record object.")

(** val env_record_initialize_immutable_binding :
    state -> env_loc -> prop_name -> value -> result_void **)

and env_record_initialize_immutable_binding s l x v =
  let%some e = (env_record_binds_pickable_option s l) in
      match e with
      | Coq_env_record_decl ed ->
        let%some evs = (decl_env_record_pickable_option ed x) in
            if prod_compare mutability_compare value_compare evs
                (Coq_mutability_uninitialized_immutable, (Coq_value_prim
                                                            Coq_prim_undef))
            then let s_2 = (env_record_write_decl_env s l x Coq_mutability_immutable v) in res_void s_2
            else (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                s
                ("Non suitable binding in [env_record_initialize_immutable_binding].")
      | Coq_env_record_object (o, p) ->
        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
          s
          ("[env_record_initialize_immutable_binding] received an environnment record object.")

(** val call_object_new : state -> value -> result **)

and call_object_new s v =
  match type_of v with
  | Coq_type_undef ->
    result_out
      (let o = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                          Coq_prealloc_object_proto))
            ("Object")) in
             let  p = (object_alloc s o) in
                 let (l, s_2) = p in Coq_out_ter (s_2, (res_val (Coq_value_object l))))
  | Coq_type_null ->
    result_out
      (let o = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                          Coq_prealloc_object_proto))
            ("Object")) in
             let  p = (object_alloc s o) in
                 let (l, s_2) = p in Coq_out_ter (s_2, (res_val (Coq_value_object l))))
  | Coq_type_bool -> to_object s v
  | Coq_type_number -> to_object s v
  | Coq_type_string -> to_object s v
  | Coq_type_object -> result_out (Coq_out_ter (s, (res_val v)))

(** val array_args_map_loop :
    state -> execution_ctx -> object_loc -> value list -> float
    -> result_void **)

and array_args_map_loop s c l args ind =
  match args with
  | [] -> res_void s
  | h :: rest ->
    let%some s_2 = (object_heap_map_properties_pickable_option s l (fun p ->
           HeapStr.write p (JsNumber.to_string (of_int ind))
             (Coq_attributes_data_of (attributes_data_intro_all_true h)))) in array_args_map_loop s_2 c l rest (ind +. 1.)

(** val string_of_prealloc : prealloc -> string **)

and string_of_prealloc _foo_ = match _foo_ with
  | Coq_prealloc_global -> "global"
  | Coq_prealloc_global_eval ->
    "global_eval"
  | Coq_prealloc_global_parse_int ->
    "global_parse_int"
  | Coq_prealloc_global_parse_float ->
    "global_parse_float"
  | Coq_prealloc_global_is_finite ->
    "global_is_finite"
  | Coq_prealloc_global_is_nan ->
    "global_is_nan"
  | Coq_prealloc_global_decode_uri ->
    "global_decode_uri"
  | Coq_prealloc_global_decode_uri_component ->
    "global_decode_uri_component"
  | Coq_prealloc_global_encode_uri ->
    "global_encode_uri"
  | Coq_prealloc_global_encode_uri_component ->
    "global_encode_uri_component"
  | Coq_prealloc_object -> "object"
  | Coq_prealloc_object_get_proto_of ->
    "object_get_proto_of"
  | Coq_prealloc_object_get_own_prop_descriptor ->
    "object_get_own_prop_descriptor"
  | Coq_prealloc_object_get_own_prop_name ->
    "object_get_own_prop_name"
  | Coq_prealloc_object_create ->
    "object_create"
  | Coq_prealloc_object_define_prop ->
    "object_define_prop"
  | Coq_prealloc_object_define_props ->
    "object_define_props"
  | Coq_prealloc_object_seal ->
    "object_seal"
  | Coq_prealloc_object_freeze ->
    "object_freeze"
  | Coq_prealloc_object_prevent_extensions ->
    "object_prevent_extensions"
  | Coq_prealloc_object_is_sealed ->
    "object_is_sealed"
  | Coq_prealloc_object_is_frozen ->
    "object_is_frozen"
  | Coq_prealloc_object_is_extensible ->
    "object_is_extensible"
  | Coq_prealloc_object_keys ->
    "object_keys"
  | Coq_prealloc_object_keys_call ->
    "object_keys_call"
  | Coq_prealloc_object_proto ->
    "object_proto_"
  | Coq_prealloc_object_proto_to_string ->
    "object_proto_to_string"
  | Coq_prealloc_object_proto_value_of ->
    "object_proto_value_of"
  | Coq_prealloc_object_proto_has_own_prop ->
    "object_proto_has_own_prop"
  | Coq_prealloc_object_proto_is_prototype_of ->
    "object_proto_is_prototype_of"
  | Coq_prealloc_object_proto_prop_is_enumerable ->
    "object_proto_prop_is_enumerable"
  | Coq_prealloc_function ->
    "function"
  | Coq_prealloc_function_proto ->
    "function_proto"
  | Coq_prealloc_function_proto_to_string ->
    "function_proto_to_string"
  | Coq_prealloc_function_proto_apply ->
    "function_proto_apply"
  | Coq_prealloc_function_proto_call ->
    "function_proto_call"
  | Coq_prealloc_function_proto_bind ->
    "function_proto_bind"
  | Coq_prealloc_bool -> "bool"
  | Coq_prealloc_bool_proto ->
    "bool_proto"
  | Coq_prealloc_bool_proto_to_string ->
    "bool_proto_to_string"
  | Coq_prealloc_bool_proto_value_of ->
    "bool_proto_value_of"
  | Coq_prealloc_number -> "number"
  | Coq_prealloc_number_proto ->
    "number_proto"
  | Coq_prealloc_number_proto_to_string ->
    "number_proto_to_string"
  | Coq_prealloc_number_proto_value_of ->
    "number_proto_value_of"
  | Coq_prealloc_number_proto_to_fixed ->
    "number_proto_to_fixed"
  | Coq_prealloc_number_proto_to_exponential ->
    "number_proto_to_exponential"
  | Coq_prealloc_number_proto_to_precision ->
    "number_proto_to_precision"
  | Coq_prealloc_array -> "array"
  | Coq_prealloc_array_is_array ->
    "array_is_array"
  | Coq_prealloc_array_proto ->
    "array_proto"
  | Coq_prealloc_array_proto_to_string ->
    "array_proto_to_string"
  | Coq_prealloc_array_proto_join ->
    "array_proto_join"
  | Coq_prealloc_array_proto_pop ->
    "array_proto_pop"
  | Coq_prealloc_array_proto_push ->
    "array_proto_push"
  | Coq_prealloc_string -> "string"
  | Coq_prealloc_string_proto ->
    "string_proto"
  | Coq_prealloc_string_proto_to_string ->
    "string_proto_to_string"
  | Coq_prealloc_string_proto_value_of ->
    "string_proto_value_of"
  | Coq_prealloc_string_proto_char_at ->
    "string_proto_char_at"
  | Coq_prealloc_string_proto_char_code_at ->
    "string_proto_char_code_at"
  | Coq_prealloc_math -> "math"
  | Coq_prealloc_mathop m -> "mathop"
  | Coq_prealloc_date -> "date"
  | Coq_prealloc_regexp -> "regexp"
  | Coq_prealloc_error -> "error"
  | Coq_prealloc_error_proto ->
    "error_proto"
  | Coq_prealloc_native_error n ->
    "native_error"
  | Coq_prealloc_native_error_proto n ->
    "native_error_proto"
  | Coq_prealloc_error_proto_to_string ->
    "error_proto_to_string"
  | Coq_prealloc_throw_type_error ->
    "throw_type_error"
  | Coq_prealloc_json -> "json"

(** val run_construct_prealloc :
    state -> execution_ctx -> prealloc -> value list -> result **)

and run_construct_prealloc s c b args =
  match b with
  | Coq_prealloc_object ->
    let v = (get_arg 0 args) in call_object_new s v
  | Coq_prealloc_bool ->
    result_out
      (let  v = (get_arg 0 args) in
           let  b0 = (convert_value_to_boolean v) in
               let
                  o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                                  Coq_prealloc_bool_proto))
                    ("Boolean")) in
                     let
                        o = (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_bool
                                                                          b0))) in
                           let  p = (object_alloc s o) in
                               let (l, s_2) = p in
                               Coq_out_ter (s_2, (res_val (Coq_value_object l))))
  | Coq_prealloc_number ->
    let 
      follow = (fun s_2 v ->
        let
           o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                           Coq_prealloc_number_proto))
             ("Number")) in
              let  o = (object_with_primitive_value o1 v) in
                  let (l, s1) = object_alloc s_2 o in
                  result_out (Coq_out_ter (s1, (res_val (Coq_value_object l))))) in
         if list_eq_nil_decidable args
         then follow s (Coq_value_prim (Coq_prim_number JsNumber.zero))
         else let  v = (get_arg 0 args) in
             let%number (x, x0) = (to_number s c v) in
                 follow x (Coq_value_prim (Coq_prim_number x0))
  | Coq_prealloc_array ->
    let
      
      o_2 = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                       Coq_prealloc_array_proto)) ("Array")) in
         let 
           o = (object_for_array o_2 Coq_builtin_define_own_prop_array) in
              let  p = (object_alloc s o) in
                  let (l, s_2) = p in
                  let  follow = (fun s_3 length0 ->
                      let%some
                         s0 = (object_heap_map_properties_pickable_option s_3 l (fun p0 ->
                             HeapStr.write p0 ("length")
                               (Coq_attributes_data_of { attributes_data_value =
                                                           (Coq_value_prim (Coq_prim_number (of_int length0)));
                                                         attributes_data_writable = true;
                                                         attributes_data_enumerable = false;
                                                         attributes_data_configurable = false }))) in
                            res_ter s0 (res_val (Coq_value_object l))) in
                      let  arg_len = (LibList.length args) in
                          if nat_eq arg_len 1
                          then let  v = (get_arg 0 args) in
                              match v with
                              | Coq_value_prim p0 ->
                                (match p0 with
                                 | Coq_prim_undef ->
                                   let%some
                                     
                                     s0 = (object_heap_map_properties_pickable_option s_2 l
                                        (fun p1 ->
                                           HeapStr.write p1 ("0") (Coq_attributes_data_of
                                                                  (attributes_data_intro_all_true v)))) in
                                        follow s0 1.0
                                 | Coq_prim_null ->
                                   let%some
                                     
                                     s0 = (object_heap_map_properties_pickable_option s_2 l
                                        (fun p1 ->
                                           HeapStr.write p1 ("0") (Coq_attributes_data_of
                                                                  (attributes_data_intro_all_true v)))) in
                                        follow s0 1.0
                                 | Coq_prim_bool b0 ->
                                   let%some
                                     
                                     s0 = (object_heap_map_properties_pickable_option s_2 l
                                        (fun p1 ->
                                           HeapStr.write p1 ("0") (Coq_attributes_data_of
                                                                  (attributes_data_intro_all_true v)))) in
                                        follow s0 1.0
                                 | Coq_prim_number vlen ->
                                   let%run
                                      (s0, ilen) = (to_uint32 s_2 c (Coq_value_prim
                                                         (Coq_prim_number vlen))) in
                                         if number_comparable (of_int ilen) vlen
                                         then follow s0 ilen
                                         else run_error s0 Coq_native_error_range
                                 | Coq_prim_string s0 ->
                                   let%some
                                     
                                     s1 = (object_heap_map_properties_pickable_option s_2 l
                                        (fun p1 ->
                                           HeapStr.write p1 ("0") (Coq_attributes_data_of
                                                                  (attributes_data_intro_all_true v)))) in
                                        follow s1 1.0)
                              | Coq_value_object o0 ->
                                let%some
                                   s0 = (object_heap_map_properties_pickable_option s_2 l
                                     (fun p0 ->
                                        HeapStr.write p0 ("0") (Coq_attributes_data_of
                                                               (attributes_data_intro_all_true v)))) in
                                      follow s0 1.0
                          else let%some
                               s0 = (object_heap_map_properties_pickable_option s_2 l
                                 (fun p0 ->
                                    HeapStr.write p0
                                      ("length")
                                      (Coq_attributes_data_of { attributes_data_value =
                                                                  (Coq_value_prim (Coq_prim_number
                                                                                     (number_of_int arg_len)));
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
                      o = (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_string
                                                                        s1))) in
                         let (l, s2) = object_alloc s0 o in
                         let
                           
                           lenDesc = (attributes_data_intro_constant (Coq_value_prim
                                                              (Coq_prim_number (number_of_int (strlength s1))))) in
                              let%some
                                 s_2 = (object_heap_map_properties_pickable_option s2 l (fun p ->
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
        build_error s (Coq_value_object (Coq_object_loc_prealloc
                                           Coq_prealloc_error_proto)) v
  | Coq_prealloc_native_error ne ->
    let  v = (get_arg 0 args) in
        build_error s (Coq_value_object (Coq_object_loc_prealloc
                                           (Coq_prealloc_native_error_proto ne))) v
  | _ ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_impossible)
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
      | None -> run_error s Coq_native_error_type
    end
  | Coq_construct_prealloc b -> run_construct_prealloc s c b args

(** val run_call_default :
    state -> execution_ctx -> object_loc -> result **)

and run_call_default s c lf =
  let
    
    def = (result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_undef))))) in
       let%some oC = (run_object_method object_code_ s lf) in
           match oC with
           | Some bd ->
             if list_eq_nil_decidable (prog_elements (funcbody_prog bd))
             then def
             else ifx_success_or_return
                 (run_prog s c (funcbody_prog bd)) (fun s_2 ->
                     result_out (Coq_out_ter (s_2,
                                              (res_val (Coq_value_prim Coq_prim_undef))))) (fun s_2 rv ->
                     result_out (Coq_out_ter (s_2, (res_normal rv))))
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
            let
              
              o2 = (object_with_invokation o1 (Some Coq_construct_default) (Some
                                                                         Coq_call_default) (Some Coq_builtin_has_instance_function)) in
                 let
                    o3 = (object_with_details o2 (Some x) (Some names) (Some bd) None None
                      None None) in
                       let  p = (object_alloc s o3) in
                           let (l, s1) = p in
                           let  a1 = ({ attributes_data_value = (Coq_value_prim
                                                                    (Coq_prim_number
                                                                       (number_of_int (LibList.length names))));
                                         attributes_data_writable = false; attributes_data_enumerable =
                                                                             false; attributes_data_configurable = false }) in
                               let%bool
                                  (s2, b2) = (object_define_own_prop s1 c l
                                    ("length")
                                    (descriptor_of_attributes (Coq_attributes_data_of a1))
                                    false) in
                                     let%bool 
                                       (s3, b3) = (creating_function_object_proto s2 c l) in
                                          if not str
                                          then res_ter s3 (res_val (Coq_value_object l))
                                          else let  vthrower = (Coq_value_object (Coq_object_loc_prealloc
                                                                                Coq_prealloc_throw_type_error)) in
                                              let 
                                                a2 = ({ attributes_accessor_get = vthrower;
                                                            attributes_accessor_set = vthrower;
                                                            attributes_accessor_enumerable = false;
                                                            attributes_accessor_configurable = false }) in
                                                   let%bool
                                                     
                                                     (s4, b4) = (object_define_own_prop s3 c l
                                                        ("caller")
                                                        (descriptor_of_attributes
                                                           (Coq_attributes_accessor_of a2)) false) in
                                                        let%bool
                                                          
                                                          (s5, b5) = (object_define_own_prop s4 c l
                                                             ("arguments")
                                                             (descriptor_of_attributes
                                                                (Coq_attributes_accessor_of a2)) false) in
                                                             res_ter s5 (res_val (Coq_value_object l))

(** val binding_inst_formal_params :
    state -> execution_ctx -> env_loc -> value list -> string
    list -> strictness_flag -> result_void **)

and binding_inst_formal_params s c l args names str =
  match names with
  | [] -> res_void s
  | argname :: names_2 ->
    let  v = (hd (Coq_value_prim Coq_prim_undef) args) in
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
                                      then let%run
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
                                                                     (Coq_value_prim Coq_prim_undef);
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
                                                       (descriptor_is_accessor_dec (descriptor_of_attributes a))                                                  
                                                    || (not (attributes_writable a))
                                                    || (not (attributes_enumerable a))
                                                then run_error s3 Coq_native_error_type
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
       else let%some o = (object_binds_pickable_option s l) in
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
                        (convert_prim_to_string (Coq_prim_number
                                                   (number_of_int len_2)))
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
                                                   (convert_prim_to_string (Coq_prim_number
                                                                              (number_of_int len_2)))
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
            let  a = ({ attributes_data_value = (Coq_value_prim (Coq_prim_number
                                                                     (number_of_int (LibList.length args))));
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
                                                                    bconfig) (Coq_value_prim Coq_prim_undef) str) in  bivd s2

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
          let  str = (funcbody_is_strict bd) in
              let  follow = (fun s_2 vthis_2 ->
                  let%some lexo = (run_object_method object_scope_ s_2 lf) in
                      let%some lex = (lexo) in
                          let  p = (lexical_env_alloc_decl s_2 lex) in
                              let (lex_2, s1) = p in
                              let 
                                c_2 = (execution_ctx_intro_same lex_2 vthis_2 str) in
                                   let%void
                                      s2= (execution_ctx_binding_inst s1 c_2 Coq_codetype_func
                                        (Some lf) (funcbody_prog bd) args) in
                                         run_call_default s2 c_2 lf) in
                  if str
                  then follow s vthis
                  else (match vthis with
                      | Coq_value_prim p ->
                        (match p with
                         | Coq_prim_undef ->
                           follow s (Coq_value_object (Coq_object_loc_prealloc
                                                         Coq_prealloc_global))
                         | Coq_prim_null ->
                           follow s (Coq_value_object (Coq_object_loc_prealloc
                                                         Coq_prealloc_global))
                         | Coq_prim_bool b -> let%value (s2, v) = (to_object s vthis) in follow s2 v
                         | Coq_prim_number n -> let%value (s2, v) = (to_object s vthis) in follow s2 v
                         | Coq_prim_string s0 ->
                           let%value (s2, v) = (to_object s vthis) in follow s2 v)
                      | Coq_value_object lthis -> follow s vthis)

(** val run_object_get_own_prop :
    state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

and run_object_get_own_prop s c l x =
  let%some b = (run_object_method object_get_own_prop_ s l) in
      let  def = (fun s_2 ->
          let%some p = (run_object_method object_properties_ s_2 l) in
              res_spec s_2
                (ifx_some_or_default
                   (convert_option_attributes
                      (HeapStr.read_option p x))
                   Coq_full_descriptor_undef (fun x -> x))) in
          match b with
          | Coq_builtin_get_own_prop_default -> def s
          | Coq_builtin_get_own_prop_args_obj ->
            let%run (s1, d) = (def s) in begin
                match d with
                | Coq_full_descriptor_undef ->
                  res_spec s1 Coq_full_descriptor_undef
                | Coq_full_descriptor_some a ->
                  let%some 
                    lmapo = (run_object_method object_parameter_map_ s1 l) in
                       let%some lmap = (lmapo) in
                           let%run 
                             (s2, d0) = (run_object_get_own_prop s1 c lmap x) in
                                let  follow = (fun s_2 a0 ->
                                    res_spec s_2 (Coq_full_descriptor_some a0)) in
                                    match d0 with
                                    | Coq_full_descriptor_undef -> follow s2 a
                                    | Coq_full_descriptor_some amap ->
                                      let%value 
                                        (s3, v) = (run_object_get s2 c lmap x) in
                                           match a with
                                           | Coq_attributes_data_of ad ->
                                             follow s3 (Coq_attributes_data_of
                                                          (attributes_data_with_value ad v))
                                           | Coq_attributes_accessor_of aa ->
                                             (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                                               s3
                                               ("[run_object_get_own_prop]:  received an accessor property descriptor in a point where the specification suppose it never happens.")
            end
          | Coq_builtin_get_own_prop_string ->
            let%run (s0, d) = (def s) in
                match d with
                | Coq_full_descriptor_undef ->
                  let%run
                    
                    (s1, k) = (to_int32 s0 c (Coq_value_prim (Coq_prim_string x))) in
                       let%string
                         
                         (s2, s3) = (to_string s1 c (Coq_value_prim
                                            (Coq_prim_number (JsNumber.absolute k)))) in
                            if not (string_eq x s3)
                            then res_spec s2 Coq_full_descriptor_undef
                            else let%string (s4, str) = (run_object_prim_value s2 l) in
                                let%run
                                   (s5, k0) = (to_int32 s4 c (Coq_value_prim
                                                    (Coq_prim_string x))) in
                                      let  len = (number_of_int (strlength str)) in
                                          if le_int_decidable len k0
                                          then res_spec s5 Coq_full_descriptor_undef
                                          else let resultStr =
                                                 string_sub str (int_of_float k0) 1
                                                 (* TODO: check k0 is not negative *)
                                            in
                                            let a = { attributes_data_value =
                                                        (Coq_value_prim (Coq_prim_string
                                                                           resultStr)); attributes_data_writable =
                                                                                          false; attributes_data_enumerable = true;
                                                      attributes_data_configurable = false }
                                            in
                                            res_spec s5 (Coq_full_descriptor_some
                                                           (Coq_attributes_data_of a))
                | Coq_full_descriptor_some a -> res_spec s0 d

(** val run_function_has_instance :
    state -> object_loc -> value -> result **)

and run_function_has_instance s lv _foo_ = match _foo_ with
  | Coq_value_prim p -> run_error s Coq_native_error_type
  | Coq_value_object lo ->
    let%some vproto = (run_object_method object_proto_ s lv) in
        match vproto with
        | Coq_value_prim p ->
          (match p with
           | Coq_prim_null ->
             res_ter s (res_val (Coq_value_prim (Coq_prim_bool false)))
           | _ ->
             (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
               s
               ("Primitive found in the prototype chain in [run_object_has_instance_loop]."))
        | Coq_value_object proto ->
          if object_loc_compare proto lo
          then res_ter s (res_val (Coq_value_prim (Coq_prim_bool true)))
          else run_function_has_instance s proto (Coq_value_object
                                                    lo)

(** val run_object_has_instance :
    state -> execution_ctx -> builtin_has_instance -> object_loc
    -> value -> result **)

and run_object_has_instance s c b l v =
  match b with
  | Coq_builtin_has_instance_function ->
    (match v with
     | Coq_value_prim w ->
       result_out (Coq_out_ter (s,
                                (res_val (Coq_value_prim (Coq_prim_bool false)))))
     | Coq_value_object lv ->
       let%value
         
         (s1, vproto) = (run_object_get s c l
            ("prototype")) in
            match vproto with
            | Coq_value_prim p -> run_error s1 Coq_native_error_type
            | Coq_value_object lproto ->
              run_function_has_instance s1 lv (Coq_value_object
                                                 lproto))
  | Coq_builtin_has_instance_after_bind ->
    let%some ol = (run_object_method object_target_function_ s l) in
        let%some l0= (ol) in
            let%some ob = (run_object_method object_has_instance_ s l0) in
                match ob with
                | Some b0 -> run_object_has_instance s c b0 l0 v
                | None -> run_error s Coq_native_error_type

(** val from_prop_descriptor :
    state -> execution_ctx -> full_descriptor -> result **)

and from_prop_descriptor s c _foo_ = match _foo_ with
  | Coq_full_descriptor_undef ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_undef))))
  | Coq_full_descriptor_some a ->
    let%object 
      (s1, l) = (run_construct_prealloc s c Coq_prealloc_object []) in
         let  follow = (fun s0 x ->
             let
                a1 = (attributes_data_intro_all_true (Coq_value_prim (Coq_prim_bool
                                                                  (attributes_enumerable a)))) in
                   let%bool
                      (s0_2, x0) = (object_define_own_prop s0 c l
                        ("enumerable")
                        (descriptor_of_attributes (Coq_attributes_data_of a1))
                        throw_false) in
                         let
                            a2 = (attributes_data_intro_all_true (Coq_value_prim (Coq_prim_bool
                                                                              (attributes_configurable a)))) in
                               let%bool
                                  (s_2, x1) = (object_define_own_prop s0_2 c l
                                    ("configurable")
                                    (descriptor_of_attributes (Coq_attributes_data_of a2))
                                    throw_false) in
                                     res_ter s_2 (res_val (Coq_value_object l))) in
             match a with
             | Coq_attributes_data_of ad ->
               let 
                 a1 = (attributes_data_intro_all_true ad.attributes_data_value) in
                    let%bool
                       (s2, x) = (object_define_own_prop s1 c l
                         ("value")
                         (descriptor_of_attributes (Coq_attributes_data_of a1))
                         throw_false) in
                          let
                             a2 = (attributes_data_intro_all_true (Coq_value_prim (Coq_prim_bool
                                                                               ad.attributes_data_writable))) in
                                let%bool
                                   (s3, v) = (object_define_own_prop s2 c l
                                     ("writable")
                                     (descriptor_of_attributes (Coq_attributes_data_of a2))
                                     throw_false) in follow s3 v
             | Coq_attributes_accessor_of aa ->
               let
                 
                 a1 = (attributes_data_intro_all_true aa.attributes_accessor_get) in
                    let%bool
                       (s2, x) = (object_define_own_prop s1 c l ("get")
                         (descriptor_of_attributes (Coq_attributes_data_of a1))
                         throw_false) in
                          let
                            
                            a2 = (attributes_data_intro_all_true aa.attributes_accessor_set) in
                               let%bool
                                  (s3, v) = (object_define_own_prop s2 c l ("set")
                                    (descriptor_of_attributes (Coq_attributes_data_of a2))
                                    throw_false) in follow s3 v

(** val run_equal :
    state -> execution_ctx -> value -> value -> result **)

and run_equal s c v1 v2 =
  let conv_number = fun s0 v -> to_number s0 c v in
  let conv_primitive = fun s0 v -> to_primitive s0 c v None in
  let  checkTypesThen = (fun s0 v3 v4 k ->
      let ty1 = type_of v3 in
      let ty2 = type_of v4 in
      if type_compare ty1 ty2
      then result_out (Coq_out_ter (s0,
                                    (res_val (Coq_value_prim (Coq_prim_bool
                                                                (equality_test_for_same_type ty1 v3 v4))))))
      else k ty1 ty2) in
      checkTypesThen s v1 v2 (fun ty1 ty2 ->
          let 
            dc_conv = (fun v3 f v4 ->
              let%value (s0, v2_2) = (f s v4) in run_equal s0 c v3 v2_2) in
               let so = fun b ->
                 result_out (Coq_out_ter (s,
                                          (res_val (Coq_value_prim (Coq_prim_bool b)))))
               in
               if (type_compare ty1 Coq_type_null)
                  && (type_compare ty2 Coq_type_undef)
               then so true
               else if (type_compare ty1 Coq_type_undef)
                   && (type_compare ty2 Coq_type_null)
               then so true
               else if (type_compare ty1 Coq_type_number)
                  && (type_compare ty2 Coq_type_string)
               then dc_conv v1 conv_number v2
               else if 
                     (type_compare ty1 Coq_type_string)
                 &&  (type_compare ty2 Coq_type_number)
               then dc_conv v2 conv_number v1
               else if type_compare ty1 Coq_type_bool
               then dc_conv v2 conv_number v1
               else if type_compare ty2 Coq_type_bool
               then dc_conv v1 conv_number v2
               else if 
                   (   (type_compare ty1 Coq_type_string)
                    || (type_compare ty1 Coq_type_number))
                &&
                   (type_compare ty2 Coq_type_object)
               then dc_conv v1 conv_primitive v2
               else if 
                    (type_compare ty1 Coq_type_object)
                 && (   (type_compare ty2 Coq_type_string)
                     || (type_compare ty2 Coq_type_number))
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
  let%run (s1, ww) = (convert_twice_primitive s c v1 v2) in
  let w1, w2 = ww in
  if  (type_compare (type_of (Coq_value_prim w1)) Coq_type_string)
   || (type_compare (type_of (Coq_value_prim w2)) Coq_type_string)
  then let%run (s2, ss) = (convert_twice_string s1 c (Coq_value_prim w1) (Coq_value_prim w2)) in
    let (s3, s4) = ss in
    res_out (Coq_out_ter (s2, (res_val (Coq_value_prim (Coq_prim_string (strappend s3 s4))))))
  else let%run (s2, nn) = (convert_twice_number s1 c (Coq_value_prim w1) (Coq_value_prim w2)) in
    let (n1, n2) = nn in
    res_out (Coq_out_ter (s2, (res_val (Coq_value_prim (Coq_prim_number (n1 +. n2))))))

and run_binary_op_arith mathop s c v1 v2 =
  let%run (s1, nn) = (convert_twice_number s c v1 v2) in
  let (n1, n2) = nn in
  res_out (Coq_out_ter (s1, (res_val (Coq_value_prim (Coq_prim_number (mathop n1 n2))))))

and run_binary_op_shift b_unsigned mathop s c v1 v2 =
    let%run (s1, k1) = ((if b_unsigned then to_uint32 else to_int32) s c v1) in
    let%run (s2, k2) = (to_uint32 s1 c v2) in
    let k2_2 = JsNumber.modulo_32 k2 in
    res_ter s2 (res_val (Coq_value_prim (Coq_prim_number (of_int (mathop k1 k2_2)))))

and run_binary_op_bitwise mathop s c v1 v2 =
    let%run (s1, k1) = (to_int32 s c v1) in
    let%run (s2, k2) = (to_int32 s1 c v2) in
    res_ter s2 (res_val (Coq_value_prim (Coq_prim_number (of_int (mathop k1 k2)))))

and run_binary_op_compare b_swap b_neg s c v1 v2 =
      let%run (s1, ww) = convert_twice_primitive s c v1 v2 in
      let (w1, w2) = ww in
      let p = if b_swap then (w2, w1) else (w1, w2) in
      let (wa, wb) = p in
      let wr = inequality_test_primitive wa wb in
      let v =
        if prim_compare wr Coq_prim_undef
        then res_val (Coq_value_prim (Coq_prim_bool false))
        else if (b_neg) && (prim_compare wr (Coq_prim_bool true))
        then res_val (Coq_value_prim (Coq_prim_bool false))
        else if (b_neg) && (prim_compare wr (Coq_prim_bool false))
        then res_val (Coq_value_prim (Coq_prim_bool true))
        else res_val (Coq_value_prim wr) in
      res_out (Coq_out_ter (s1, v))

and run_binary_op_instanceof s c v1 v2 =
  match v2 with
  | Coq_value_prim p -> run_error s Coq_native_error_type
  | Coq_value_object l ->
    let%some b = (run_object_method object_has_instance_ s l) in
    match b with
    | None -> run_error s Coq_native_error_type
    | Some has_instance_id -> run_object_has_instance s c has_instance_id l v1

and run_binary_op_in s c v1 v2 =
  match v2 with
  | Coq_value_prim p -> run_error s Coq_native_error_type
  | Coq_value_object l ->
    let%string (s2, x) = (to_string s c v1) in
    object_has_prop s2 c l x

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
    res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool (not b0))))
  | Coq_binary_op_strict_equal ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim (Coq_prim_bool (strict_equality_test v1 v2))))))
  | Coq_binary_op_strict_disequal ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim (Coq_prim_bool (not (strict_equality_test v1 v2)))))))
  | Coq_binary_op_coma -> result_out (Coq_out_ter (s, (res_val v2)))
  | Coq_binary_op_and -> assert false
  | Coq_binary_op_or  -> assert false

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

and run_typeof_value s _foo_ = match _foo_ with
  | Coq_value_prim w -> typeof_prim w
  | Coq_value_object l ->
    if is_callable_dec s (Coq_value_object l)
    then "function"
    else "object"

(** val run_unary_op :
    state -> execution_ctx -> unary_op -> expr -> result **)

and run_unary_op s c op e =
  if prepost_unary_op_dec op
  then let%success (s1, rv1)= (run_expr s c e) in
      let%run (s2, v2) = (ref_get_value s1 c rv1) in
          let%number (s3, n1) = (to_number s2 c v2) in
              let%some po = (run_prepost_op op) in
                  let (number_op, is_pre) = po in
                  let  n2 = (number_op n1) in
                      let 
                        v = (Coq_prim_number (if is_pre then n2 else n1)) in
                           let%void
                              s4= (ref_put_value s3 c rv1 (Coq_value_prim
                                                        (Coq_prim_number n2))) in
                                 result_out (Coq_out_ter (s4,
                                                          (res_val (Coq_value_prim v))))
  else (match op with
      | Coq_unary_op_delete ->
        let%success (s0, rv)= (run_expr s c e) in begin
            match rv with
            | Coq_resvalue_empty ->
              res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool true)))
            | Coq_resvalue_value v ->
              res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool true)))
            | Coq_resvalue_ref r ->
              if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_undef
              then if r.ref_strict
                then run_error s0 Coq_native_error_syntax
                else res_ter s0
                    (res_val (Coq_value_prim (Coq_prim_bool true)))
              else (match r.ref_base with
                  | Coq_ref_base_type_value v ->
                    let%object (s1, l) = (to_object s0 v) in
                        object_delete s1 c l r.ref_name
                          r.ref_strict
                  | Coq_ref_base_type_env_loc l ->
                    if r.ref_strict
                    then run_error s0 Coq_native_error_syntax
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
                (res_val (Coq_value_prim (Coq_prim_string
                                            (run_typeof_value s1 v))))
            | Coq_resvalue_ref r ->
              if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_undef
              then res_ter s1
                  (res_val (Coq_value_prim (Coq_prim_string
                                              ("undefined"))))
              else let%run 
                  (s2, v) = (ref_get_value s1 c (Coq_resvalue_ref r)) in
                     res_ter s2
                       (res_val (Coq_value_prim (Coq_prim_string
                                                   (run_typeof_value s2 v))))
        end
      | _ ->
        let%run (s1, v) = (run_expr_get_value s c e) in
            match op with
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_add -> to_number s1 c v
            | Coq_unary_op_neg ->
              let%number (s2, n) = (to_number s1 c v) in
                  res_ter s2
                    (res_val (Coq_value_prim (Coq_prim_number
                                                (JsNumber.neg n))))
            | Coq_unary_op_bitwise_not ->
              let%run (s2, k) = (to_int32 s1 c v) in
                  res_ter s2
                    (res_val (Coq_value_prim (Coq_prim_number
                                                (of_int (JsNumber.int32_bitwise_not k)))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                                            (not (convert_value_to_boolean v)))))
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
  | [] -> result_out (Coq_out_ter (s, (res_val (Coq_value_object l))))
  | p :: pds_2 ->
    let (pn, pb) = p in
    let  x = (string_of_propname pn) in
        let  follows = (fun s1 desc ->
            let%success 
              (s2, rv) = (object_define_own_prop s1 c l x desc false) in  init_object s2 c l pds_2) in
            match pb with
            | Coq_propbody_val e0 ->
              let%run (s1, v0) = (run_expr_get_value s c e0) in
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
  | [] -> result_out (Coq_out_ter (s, (res_val (Coq_value_object l))))
  | o :: oes_2 ->
    (match o with
     | Some e ->
       let 
         loop_result = (fun s0 ->
           run_array_element_list s0 c l oes_2 0.) in
            let%run (s0, v) = (run_expr_get_value s c e) in
                let%value
                   (s1, vlen) = (run_object_get s0 c l
                     ("length")) in
                      let%run (s2, ilen) = (to_uint32 s1 c vlen) in
                          let%string
                             (s3, slen) = (to_string s2 c (Coq_value_prim (Coq_prim_number
                                                               (ilen +. n)))) in
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
  let  elementList = (elision_tail_remove oes) in
      let  elisionLength = (elision_tail_count oes) in
          let%object 
            (s0, l0) = (run_array_element_list s c l elementList 0.) in
               let%value
                  (s1, vlen) = (run_object_get s0 c l0
                    ("length")) in
                     let%run (s2, ilen) = (to_uint32 s1 c vlen) in
                         let%run
                           
                           (s3, len) = (to_uint32 s2 c (Coq_value_prim (Coq_prim_number
                                                              (ilen +. number_of_int elisionLength)))) in
                              let%not_throw
                                 (s4, x) = (object_put s3 c l0
                                   ("length")
                                   (Coq_value_prim (Coq_prim_number (of_int len)))
                                   throw_false) in
                                    result_out (Coq_out_ter (s4,
                                                             (res_val (Coq_value_object l0))))

(** val run_var_decl_item :
    state -> execution_ctx -> prop_name -> expr option -> result **)

and run_var_decl_item s c x _foo_ = match _foo_ with
  | Some e ->
    let%run (s1, ir) = (identifier_resolution s c x) in
        let%run (s2, v) = (run_expr_get_value s1 c e) in
            let%void s3= (ref_put_value s2 c (Coq_resvalue_ref ir) v) in
                result_out (Coq_out_ter (s3,
                                         (res_val (Coq_value_prim (Coq_prim_string x)))))
  | None ->
    result_out (Coq_out_ter (s,
                             (res_val (Coq_value_prim (Coq_prim_string x)))))

(** val run_var_decl :
    state -> execution_ctx -> (prop_name * expr option) list ->
    result **)

and run_var_decl s c _foo_ = match _foo_ with
  | [] -> result_out (Coq_out_ter (s, res_empty))
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
    let%run (s2, v) = (run_expr_get_value s1 c e) in
        run_list_expr s2 c (v :: vs) es_2

(** val run_block :
    state -> execution_ctx -> stat list -> result **)

and run_block s c _foo_ = match _foo_ with
  | [] -> res_ter s (res_normal Coq_resvalue_empty)
  | t :: ts_rev_2 ->
    let%success (s0, rv0)= (run_block s c ts_rev_2) in
        ifx_success_state rv0 (run_stat s0 c t) (fun x x0 ->
            result_out (Coq_out_ter (x, (res_normal x0))))

and run_binary_op_and s c e1 e2 =
  let%run (s1, v1) = (run_expr_get_value s c e1) in
  let b1 = (convert_value_to_boolean v1) in
  if not b1
    then res_ter s1 (res_val v1)
    else let%run (s2, v) = (run_expr_get_value s1 c e2) in
    res_ter s2 (res_val v)

and run_binary_op_or s c e1 e2 =
  let%run (s1, v1) = (run_expr_get_value s c e1) in
  let b1 = (convert_value_to_boolean v1) in
  if b1 
    then res_ter s1 (res_val v1)
    else let%run (s2, v) = (run_expr_get_value s1 c e2) in
    res_ter s2 (res_val v)

(** val run_expr_binary_op :
    state -> execution_ctx -> binary_op -> expr -> expr ->
    result **)

and run_expr_binary_op s c op e1 e2 =
  match op with
  | Coq_binary_op_and -> run_binary_op_and s c e1 e2
  | Coq_binary_op_or -> run_binary_op_or s c e1 e2
  | _ ->
    let%run (s1,v1) = run_expr_get_value s c e1 in
    let%run (s2,v2) = run_expr_get_value s1 c e2 in
    run_binary_op s2 c op v1 v2

(** val run_expr_access :
    state -> execution_ctx -> expr -> expr -> result **)

(* TODO DEPRECATEd
   and run_expr_access s c e1 e2 =
   let%run (s1, v1) = (run_expr_get_value s c e1) in
    let%run (s2, v2) = (run_expr_get_value s1 c e2) in
      if or_decidable (value_compare v1 (Coq_value_prim Coq_prim_undef))
           (value_compare v1 (Coq_value_prim Coq_prim_null))
      then run_error s2 Coq_native_error_type
      else let%string (s3, x) = (to_string s2 c v2) in
             res_ter s3
               (res_ref (ref_create_value v1 x c.execution_ctx_strict))
*)

and run_expr_access s c e1 e2 =
  let%run (s1,v1) = run_expr_get_value s c e1 in
  let%run (s2,v2) = run_expr_get_value s1 c e2 in
  if    (value_compare v1 (Coq_value_prim Coq_prim_undef))
     || (value_compare v1 (Coq_value_prim Coq_prim_null))
  then run_error s2 Coq_native_error_type
  else let%string (s3,x) = to_string s2 c v2 in
    res_ter s3 (res_ref (ref_create_value v1 x c.execution_ctx_strict))

(** val run_expr_assign :
    state -> execution_ctx -> binary_op option -> expr -> expr
    -> result **)

and run_expr_assign s c opo e1 e2 =
  let%success (s1, rv1)= (run_expr s c e1) in
      let 
        follow = (fun s0 rv_2 ->
          match rv_2 with
          | Coq_resvalue_empty ->
            (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
              s0
              ("Non-value result in [run_expr_assign].")
          | Coq_resvalue_value v ->
            let%void s_2= (ref_put_value s0 c rv1 v) in
                result_out (Coq_out_ter (s_2, (res_val v)))
          | Coq_resvalue_ref r ->
            (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
              s0
              ("Non-value result in [run_expr_assign].")) in
           match opo with
           | Some op ->
             let%run (s2, v1) = (ref_get_value s1 c rv1) in
                 let%run (s3, v2) = (run_expr_get_value s2 c e2) in
                     let%success (s4, v) = (run_binary_op s3 c op v1 v2) in  follow s4 v
           | None ->
             let%run (x, x0 )= (run_expr_get_value s1 c e2) in
                 follow x (Coq_resvalue_value x0)

(** val run_expr_function :
    state -> execution_ctx -> prop_name option -> string list
    -> funcbody -> result **)

and run_expr_function s c fo args bd =
  match fo with
  | Some fn ->
    let 
      p = (lexical_env_alloc_decl s c.execution_ctx_lexical_env) in
         let (lex_2, s_2) = p in
         let follow = fun l ->
           let%some e = (env_record_binds_pickable_option s_2 l) in
               let%void s1= (env_record_create_immutable_binding s_2 l fn) in
                   let%object
                      (s2, l0) = (creating_function_object s1 c args bd lex_2
                        (funcbody_is_strict bd)) in
                         let%void
                            s3= (env_record_initialize_immutable_binding s2 l fn
                              (Coq_value_object l0)) in
                               result_out (Coq_out_ter (s3,
                                                        (res_val (Coq_value_object l0))))
         in
         destr_list lex_2 (fun x ->
             (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
               s_2
               ("Empty lexical environnment allocated in [run_expr_function]."))
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
  | Coq_value_prim p ->
    (match p with
     | Coq_prim_undef ->
       result_out (Coq_out_ter (s,
                                (res_val (Coq_value_prim Coq_prim_undef))))
     | Coq_prim_null ->
       result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_null))))
     | Coq_prim_bool b ->
       result_out (Coq_out_ter (s,
                                (res_val (Coq_value_prim (Coq_prim_bool b)))))
     | Coq_prim_number n ->
       result_out (Coq_out_ter (s,
                                (res_val (Coq_value_prim (Coq_prim_number n)))))
     | Coq_prim_string s0 ->
       let str = (is_direct_call && c.execution_ctx_strict) in
            match parse_pickable s0 str with
            | Some p0 ->
              entering_eval_code s c is_direct_call (Coq_funcbody_intro
                                                       (p0, s0)) (fun s1 c_2 ->
                  let%ter (s2, r) = (run_prog s1 c_2 p0) in
                      match r.res_type with
                      | Coq_restype_normal ->
                        ifx_empty_label s2 r (fun x ->
                            match r.res_value with
                            | Coq_resvalue_empty ->
                              res_ter s2 (res_val (Coq_value_prim Coq_prim_undef))
                            | Coq_resvalue_value v -> res_ter s2 (res_val v)
                            | Coq_resvalue_ref r0 ->
                              (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                                s2
                                ("Reference found in the result of an `eval\' in [run_eval]."))
                      | Coq_restype_throw -> res_ter s2 (res_throw r.res_value)
                      | _ ->
                        (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                          s2
                          ("Forbidden result type returned by an `eval\' in [run_eval]."))
            | None -> run_error s Coq_native_error_syntax)
  | Coq_value_object o ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_object o))))

(** val run_expr_call :
    state -> execution_ctx -> expr -> expr list -> result **)

and run_expr_call s c e1 e2s =
  let  is_eval_direct = (is_syntactic_eval e1) in
      let%success (s1, rv)= (run_expr s c e1) in
          let%run (s2, f) = (ref_get_value s1 c rv) in
              let%run (s3, vs) = (run_list_expr s2 c [] e2s) in
                  match f with
                  | Coq_value_prim p -> run_error s3 Coq_native_error_type
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
                          follow (Coq_value_prim Coq_prim_undef)
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
                    else run_error s3 Coq_native_error_type

(** val run_expr_conditionnal :
    state -> execution_ctx -> expr -> expr -> expr -> result **)

and run_expr_conditionnal s c e1 e2 e3 =
  let%run (s1, v1) = (run_expr_get_value s c e1) in
      let  b = (convert_value_to_boolean v1) in
          let  e = (if b then e2 else e3) in
              let%run (s0, r) = (run_expr_get_value s1 c e) in
                  res_ter s0 (res_val r)

(** val run_expr_new :
    state -> execution_ctx -> expr -> expr list -> result **)

and run_expr_new s c e1 e2s =
  let%run (s1, v) = (run_expr_get_value s c e1) in
      let%run (s2, args) = (run_list_expr s1 c [] e2s) in
          match v with
          | Coq_value_prim p -> run_error s2 Coq_native_error_type
          | Coq_value_object l ->
            let%some coo = (run_object_method object_construct_ s2 l) in
                match coo with
                | Some co -> run_construct s2 c co l args
                | None -> run_error s2 Coq_native_error_type

(** val run_stat_label :
    state -> execution_ctx -> label -> stat -> result **)

and run_stat_label s c lab t =
  let%break (s1, r1) = run_stat s c t in
      result_out (Coq_out_ter (s1,
                               (if label_compare r1.res_label lab
                                then res_normal r1.res_value
                                else r1)))

(** val run_stat_with :
    state -> execution_ctx -> expr -> stat -> result **)

and run_stat_with s c e1 t2 =
  let%run (s1, v1) = (run_expr_get_value s c e1) in
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
  let%run (s1, v1) = (run_expr_get_value s c e1) in
      let  b = (convert_value_to_boolean v1) in
          if b
          then run_stat s1 c t2
          else (match to0 with
              | Some t3 -> run_stat s1 c t3
              | None ->
                result_out (Coq_out_ter (s1, (res_normal Coq_resvalue_empty))))

(** val run_stat_while :
    state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

and run_stat_while s c rv labs e1 t2 =
  let%run (s1, v1) = (run_expr_get_value s c e1) in
      let  b = (convert_value_to_boolean v1) in
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
  | [] -> result_out (Coq_out_ter (s, (res_normal rv)))
  | y :: scs_2 ->
    match y with Coq_switchclause_intro (e, ts) ->
      ifx_success_state rv (run_block s c (rev ts)) (fun s1 rv1 ->
          run_stat_switch_end s1 c rv1 scs_2)

(** val run_stat_switch_no_default :
    state -> execution_ctx -> value -> resvalue -> switchclause
    list -> result **)

and run_stat_switch_no_default s c vi rv _foo_ = match _foo_ with
  | [] -> result_out (Coq_out_ter (s, (res_normal rv)))
  | y :: scs_2 ->
    match y with Coq_switchclause_intro (e, ts) ->
      let%run (s1, v1) = (run_expr_get_value s c e) in
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
      let%run (s1, v1) = (run_expr_get_value s c e) in
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
           else let%run (s1, v1) = (run_expr_get_value s c e) in
               let  b = (strict_equality_test v1 vi) in
                   if b
                   then follow s1
                   else run_stat_switch_with_default_A s1 c false vi rv
                       scs_2 ts0 scs2

(** val run_stat_switch :
    state -> execution_ctx -> label_set -> expr -> switchbody ->
    result **)

and run_stat_switch s c labs e sb =
  let%run (s1, vi) = run_expr_get_value s c e in
      let  follow = (fun w ->
      let%success (s0, r) =
        let%break (s2, r) = w in
        if res_label_in r labs
        then result_out (Coq_out_ter (s2, (res_normal r.res_value)))
        else result_out (Coq_out_ter (s2, r)) in
      res_ter s0 (res_normal r)) in
      match sb with
      | Coq_switchbody_nodefault scs ->
        follow
          (run_stat_switch_no_default s1 c vi Coq_resvalue_empty scs)
      | Coq_switchbody_withdefault (scs1, ts, scs2) ->
        follow
          (run_stat_switch_with_default_A s1 c false vi
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
                let%run (s2, v1) = (run_expr_get_value s1 c e1) in
                    let  b = (convert_value_to_boolean v1) in
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
  let%run (s1, v1) = (run_expr_get_value s c e) in
      res_ter s1 (res_throw (Coq_resvalue_value v1))

(** val run_stat_return :
    state -> execution_ctx -> expr option -> result **)

and run_stat_return s c _foo_ = match _foo_ with
  | Some e ->
    let%run (s1, v1) = (run_expr_get_value s c e) in
        res_ter s1 (res_return (Coq_resvalue_value v1))
  | None ->
    result_out (Coq_out_ter (s,
                             (res_return (Coq_resvalue_value (Coq_value_prim Coq_prim_undef)))))

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
                          let%run 
                            (s2, v3) = (run_expr_get_value s1 c e3) in loop s2
                        | None -> loop s1)
                    else res_ter s1 r) in
      match eo2 with
      | Some e2 ->
        let%run (s0, v2) = (run_expr_get_value s c e2) in
            let  b = (convert_value_to_boolean v2) in
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
     let%run (s0, v1) = (run_expr_get_value s c e1) in follows s0
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
    result_out (Coq_out_ter (s, (res_val c.execution_ctx_this_binding)))
  | Coq_expr_identifier x ->
    let%run (s0, r) = (identifier_resolution s c x) in
        res_ter s0 (res_ref r)
  | Coq_expr_literal i ->
    result_out (Coq_out_ter (s,
                             (res_val (Coq_value_prim (convert_literal_to_prim i)))))
  | Coq_expr_object pds ->
    let%object 
      (s1, l) = (run_construct_prealloc s c Coq_prealloc_object []) in init_object s1 c l pds
  | Coq_expr_array oes ->
    let%object 
      (s1, l) = (run_construct_prealloc s c Coq_prealloc_array []) in init_array s1 c l oes
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
    let%run (s0, r) = (run_expr_get_value s c e) in
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
  | Coq_stat_break so -> result_out (Coq_out_ter (s, (res_break so)))
  | Coq_stat_continue so -> result_out (Coq_out_ter (s, (res_continue so)))
  | Coq_stat_try (t1, t2o, t3o) -> run_stat_try s c t1 t2o t3o
  | Coq_stat_for (ls, eo1, eo2, eo3, s0) ->
    run_stat_for s c ls eo1 eo2 eo3 s0
  | Coq_stat_for_var (ls, ds, eo2, eo3, s0) ->
    run_stat_for_var s c ls ds eo2 eo3 s0
  | Coq_stat_for_in (ls, e1, e2, s0) ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_impossible)
      ("stat_for_in")
  | Coq_stat_for_in_var (ls, x, e1o, e2, s0) ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_impossible)
      ("stat_for_in_var")
  | Coq_stat_debugger -> result_out (Coq_out_ter (s, res_empty))
  | Coq_stat_switch (labs, e, sb) -> run_stat_switch s c labs e sb

(** val run_elements :
    state -> execution_ctx -> elements -> result **)

and run_elements s c _foo_ = match _foo_ with
  | [] -> result_out (Coq_out_ter (s, (res_normal Coq_resvalue_empty)))
  | el :: els_rev_2 ->
    let%success (s0, rv0)= (run_elements s c els_rev_2) in
        match el with
        | Coq_element_stat t ->
          let%ter (s1, r1) = (run_stat s0 c t) in
              let r2 = res_overwrite_value_if_empty rv0 r1 in
              res_out (Coq_out_ter (s1, r2))
        | Coq_element_func_decl (name, args, bd) -> res_ter s0 (res_normal rv0)

(** val run_prog : state -> execution_ctx -> prog -> result **)

and run_prog s c _term_ = match _term_ with
  | Coq_prog_intro (str, els) -> run_elements s c (rev els)

(** val push :
    state -> execution_ctx -> object_loc -> value list -> float
    -> result **)

and push s c l args ilen =
  let  vlen = (of_int ilen) in
      match args with
      | [] ->
        let%not_throw
           (s0, x) = (object_put s c l ("length")
             (Coq_value_prim (Coq_prim_number vlen)) throw_true) in
              result_out (Coq_out_ter (s0,
                                       (res_val (Coq_value_prim (Coq_prim_number vlen)))))
      | v :: vs ->
        let%string 
          (s0, slen) = (to_string s c (Coq_value_prim (Coq_prim_number vlen))) in
             let%not_throw  (s1, x) = (object_put s0 c l slen v throw_true) in
                 push s1 c l vs (ilen +. 1.)

(** val run_object_is_sealed :
    state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

and run_object_is_sealed s c l _foo_ = match _foo_ with
  | [] ->
    let%some ext = (run_object_method object_extensible_ s l) in
        res_ter s (res_val (Coq_value_prim (Coq_prim_bool (not ext))))
  | x :: xs_2 ->
    let%run (s0, d) = (run_object_get_own_prop s c l x) in
        match d with
        | Coq_full_descriptor_undef ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s0
            ("[run_object_is_sealed]:  Undefined descriptor found in a place where it shouldn\'t.")
        | Coq_full_descriptor_some a ->
          if attributes_configurable a
          then res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool false)))
          else run_object_is_sealed s0 c l xs_2

(** val run_object_seal :
    state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

and run_object_seal s c l _foo_ = match _foo_ with
  | [] ->
    let%some s0= (run_object_heap_set_extensible false s l) in
        res_ter s0 (res_val (Coq_value_object l))
  | x :: xs_2 ->
    let%run (s0, d) = (run_object_get_own_prop s c l x) in
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
    let%run (s0, d) = (run_object_get_own_prop s c l x) in
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
        res_ter s (res_val (Coq_value_prim (Coq_prim_bool (not ext))))
  | x :: xs_2 ->
    let%run (s0, d) = (run_object_get_own_prop s c l x) in
        let  check_configurable = (fun a ->
            if attributes_configurable a
            then res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool false)))
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
                 then res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool false)))
                 else check_configurable (Coq_attributes_data_of ad)
               | Coq_attributes_accessor_of aa ->
                 check_configurable (Coq_attributes_accessor_of aa))

(** val run_get_args_for_apply :
    state -> execution_ctx -> object_loc -> float -> float ->
    value list specres **)

and run_get_args_for_apply s c l index n =
  if  index < n
  then let%string
       (s0, sindex) = (to_string s c (Coq_value_prim (Coq_prim_number
                                        (of_int index)))) in
          let%value (s1, v) = (run_object_get s0 c l sindex) in
              let
                
                tail_args = (run_get_args_for_apply s1 c l (index +. 1.) n) in
                   let%run (s2, tail) = (tail_args) in res_spec s2 (v :: tail)
  else res_spec s []

(** val valueToStringForJoin :
    state -> execution_ctx -> object_loc -> float -> string
    specres **)

and valueToStringForJoin s c l k =
  let%string
    
    (s0, prop) = (to_string s c (Coq_value_prim (Coq_prim_number (of_int k)))) in
       let%value (s1, v) = (run_object_get s0 c l prop) in
           match v with
           | Coq_value_prim p ->
             (match p with
              | Coq_prim_undef -> res_spec s1 ""
              | Coq_prim_null -> res_spec s1 ""
              | Coq_prim_bool b ->
                let%string (s2, s3) = (to_string s1 c v) in res_spec s2 s3
              | Coq_prim_number n ->
                let%string (s2, s3) = (to_string s1 c v) in res_spec s2 s3
              | Coq_prim_string s2 ->
                let%string (s3, s4) = (to_string s1 c v) in res_spec s3 s4)
           | Coq_value_object o ->
             let%string (s2, s3) = (to_string s1 c v) in res_spec s2 s3

(** val run_array_join_elements :
    state -> execution_ctx -> object_loc -> float -> float ->
    string -> string -> result **)

and run_array_join_elements s c l k length0 sep sR =
  if  k < length0
  then let  ss = (strappend sR sep) in
      let  sE = (valueToStringForJoin s c l k) in
          let%run (s0, element) = (sE) in
              let  sR0 = (strappend ss element) in
                  run_array_join_elements s0 c l (k +. 1.)
                    length0 sep sR0
  else res_ter s (res_val (Coq_value_prim (Coq_prim_string sR)))

(** val run_call_prealloc :
    state -> execution_ctx -> prealloc -> value -> value list ->
    result **)

and run_call_prealloc s c b vthis args =
  match b with
  | Coq_prealloc_global_is_finite ->
    let  v = (get_arg 0 args) in
        let%number (s0, n) = (to_number s c v) in
            res_ter s0
              (res_val (Coq_value_prim (Coq_prim_bool
                                          (not
                                             (   (number_comparable n JsNumber.nan)
                                              || (number_comparable n JsNumber.infinity)
                                              || (number_comparable n JsNumber.neg_infinity))))))
  | Coq_prealloc_global_is_nan ->
    let  v = (get_arg 0 args) in
        let%number (s0, n) = (to_number s c v) in
            res_ter s0
              (res_val (Coq_value_prim (Coq_prim_bool
                                          (number_comparable n JsNumber.nan))))
  | Coq_prealloc_object ->
    let  value0 = (get_arg 0 args) in begin
        match value0 with
        | Coq_value_prim p ->
          (match p with
           | Coq_prim_undef -> run_construct_prealloc s c b args
           | Coq_prim_null -> run_construct_prealloc s c b args
           | Coq_prim_bool b0 -> to_object s value0
           | Coq_prim_number n -> to_object s value0
           | Coq_prim_string s0 -> to_object s value0)
        | Coq_value_object o -> to_object s value0
    end
  | Coq_prealloc_object_get_proto_of ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some proto = (run_object_method object_proto_ s l) in
              res_ter s (res_val proto)
    end
  | Coq_prealloc_object_get_own_prop_descriptor ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%string 
            (s1, x) = (to_string s c (get_arg 1 args)) in
               let%run (s2, d) = (run_object_get_own_prop s1 c l x) in
                   from_prop_descriptor s2 c d
    end
  | Coq_prealloc_object_define_prop ->
    let  o = (get_arg 0 args) in
        let  p = (get_arg 1 args) in
            let 
              attr = (get_arg 2 args) in begin
                 match o with
                 | Coq_value_prim p0 -> run_error s Coq_native_error_type
                 | Coq_value_object l ->
                   let%string (s1, name) = (to_string s c p) in
                       let%run (s2, desc) = (run_to_descriptor s1 c attr) in
                           let%bool 
                             (s3, x) = (object_define_own_prop s2 c l name desc true) in res_ter s3 (res_val (Coq_value_object l))
    end
  | Coq_prealloc_object_seal ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some 
            _x_ = (object_properties_keys_as_list_pickable_option s l) in  run_object_seal s c l _x_
    end
  | Coq_prealloc_object_freeze ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some 
            _x_ = (object_properties_keys_as_list_pickable_option s l) in  run_object_freeze s c l _x_
    end
  | Coq_prealloc_object_prevent_extensions ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some o = (object_binds_pickable_option s l) in
              let o1 = object_with_extension o false in
              let s_2 = object_write s l o1 in
              res_ter s_2 (res_val (Coq_value_object l))
    end
  | Coq_prealloc_object_is_sealed ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some 
            _x_ = (object_properties_keys_as_list_pickable_option s l) in  run_object_is_sealed s c l _x_
    end
  | Coq_prealloc_object_is_frozen ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some 
            _x_ = (object_properties_keys_as_list_pickable_option s l) in  run_object_is_frozen s c l _x_
    end
  | Coq_prealloc_object_is_extensible ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p -> run_error s Coq_native_error_type
        | Coq_value_object l ->
          let%some r = (run_object_method object_extensible_ s l) in
              res_ter s (res_val (Coq_value_prim (Coq_prim_bool r)))
    end
  | Coq_prealloc_object_proto_to_string ->
    (match vthis with
     | Coq_value_prim p ->
       (match p with
        | Coq_prim_undef ->
          result_out (Coq_out_ter (s,
                                   (res_val (Coq_value_prim (Coq_prim_string
                                                               ("[object Undefined]"))))))
        | Coq_prim_null ->
          result_out (Coq_out_ter (s,
                                   (res_val (Coq_value_prim (Coq_prim_string
                                                               ("[object Null]"))))))
        | Coq_prim_bool b0 ->
          let%object (s1, l) = (to_object s vthis) in
              let%some s0= (run_object_method object_class_ s1 l) in
                  res_ter s1
                    (res_val (Coq_value_prim (Coq_prim_string
                                                (strappend
                                                   ("[object ")
                                                   (strappend s0 ("]"))))))
        | Coq_prim_number n ->
          let%object (s1, l) = (to_object s vthis) in
              let%some s0= (run_object_method object_class_ s1 l) in
                  res_ter s1
                    (res_val (Coq_value_prim (Coq_prim_string
                                                (strappend
                                                   ("[object ")
                                                   (strappend s0 ("]"))))))
        | Coq_prim_string s0 ->
          let%object (s1, l) = (to_object s vthis) in
              let%some s2= (run_object_method object_class_ s1 l) in
                  res_ter s1
                    (res_val (Coq_value_prim (Coq_prim_string
                                                (strappend
                                                   ("[object ")
                                                   (strappend s2 ("]")))))))
     | Coq_value_object o ->
       let%object (s1, l) = (to_object s vthis) in
           let%some s0= (run_object_method object_class_ s1 l) in
               res_ter s1
                 (res_val (Coq_value_prim (Coq_prim_string
                                             (strappend
                                                ("[object ")
                                                (strappend s0 ("]")))))))
  | Coq_prealloc_object_proto_value_of -> to_object s vthis
  | Coq_prealloc_object_proto_has_own_prop ->
    let  v = (get_arg 0 args) in
        let%string (s1, x) = (to_string s c v) in
            let%object (s2, l) = (to_object s1 vthis) in
                let%run (s3, d) = (run_object_get_own_prop s2 c l x) in begin
                    match d with
                    | Coq_full_descriptor_undef ->
                      res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool false)))
                    | Coq_full_descriptor_some a ->
                      res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool true)))
    end
  | Coq_prealloc_object_proto_is_prototype_of ->
    let  v = (get_arg 0 args) in begin
        match v with
        | Coq_value_prim p ->
          result_out (Coq_out_ter (s,
                                   (res_val (Coq_value_prim (Coq_prim_bool false)))))
        | Coq_value_object l ->
          let%object (s1, lo) = (to_object s vthis) in
              object_proto_is_prototype_of s1 lo l
    end
  | Coq_prealloc_object_proto_prop_is_enumerable ->
    let  v = (get_arg 0 args) in
        let%string (s1, x) = (to_string s c v) in
            let%object (s2, l) = (to_object s1 vthis) in
                let%run (s3, d) = (run_object_get_own_prop s2 c l x) in begin
                    match d with
                    | Coq_full_descriptor_undef ->
                      res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool false)))
                    | Coq_full_descriptor_some a ->
                      res_ter s3
                        (res_val (Coq_value_prim (Coq_prim_bool
                                                    (attributes_enumerable a))))
    end
  | Coq_prealloc_function_proto ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_undef))))
  | Coq_prealloc_function_proto_to_string ->
    if is_callable_dec s vthis
    then (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_impossible)
        ("Function.prototype.toString() is implementation dependent.")
    else run_error s Coq_native_error_type
  | Coq_prealloc_function_proto_apply ->
    let  thisArg = (get_arg 0 args) in
        let  argArray = (get_arg 1 args) in
            if is_callable_dec s vthis
            then (match vthis with
                | Coq_value_prim p ->
                  (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                    s
                    ("Value is callable, but isn\'t an object.")
                | Coq_value_object thisobj ->
                  (match argArray with
                   | Coq_value_prim p ->
                     (match p with
                      | Coq_prim_undef ->
                        run_call s c thisobj thisArg []
                      | Coq_prim_null ->
                        run_call s c thisobj thisArg []
                      | Coq_prim_bool b0 -> run_error s Coq_native_error_type
                      | Coq_prim_number n -> run_error s Coq_native_error_type
                      | Coq_prim_string s0 -> run_error s Coq_native_error_type)
                   | Coq_value_object array ->
                     let%value
                       (s0, v) = (run_object_get s c array
                          ("length")) in
                          let%run (s1, ilen) = (to_uint32 s0 c v) in
                              let%run
                                (s2, arguments_) = (run_get_args_for_apply s1 c array 0. ilen) in
                                   run_call s2 c thisobj thisArg arguments_))
            else run_error s Coq_native_error_type
  | Coq_prealloc_function_proto_call ->
    if is_callable_dec s vthis
    then (match vthis with
        | Coq_value_prim p ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("Value is callable, but isn\'t an object.")
        | Coq_value_object thisobj ->
          let (thisArg, a) = get_arg_first_and_rest args in
          run_call s c thisobj thisArg a)
    else run_error s Coq_native_error_type
  | Coq_prealloc_function_proto_bind ->
    if is_callable_dec s vthis
    then (match vthis with
        | Coq_value_prim p ->
          (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
            s
            ("Value is callable, but isn\'t an object.")
        | Coq_value_object thisobj ->
          let (vthisArg, a) = get_arg_first_and_rest args in
          let
             o1 = (object_new (Coq_value_object (Coq_object_loc_prealloc
                                             Coq_prealloc_object_proto))
               ("Object")) in
                let 
                  o2 = (object_with_get o1 Coq_builtin_get_function) in
                     let
                        o3 = (object_with_details o2 None None None (Some thisobj) (Some
                                                                                vthisArg) (Some a) None) in
                           let
                             
                             o4 = (object_set_class o3
                                ("Function")) in
                                let
                                   o5 = (object_set_proto o4 (Coq_value_object
                                                          (Coq_object_loc_prealloc
                                                             Coq_prealloc_function_proto))) in
                                      let
                                         o6 = (object_with_invokation o5 (Some
                                                                      Coq_construct_after_bind) (Some
                                                                                                   Coq_call_after_bind) (Some
                                                                                                                           Coq_builtin_has_instance_after_bind)) in
                                            let 
                                              o7 = (object_set_extensible o6 true) in
                                                 let (l, s_2) = object_alloc s o7 in
                                                 let
                                                    vlength = (let%some
                                                      class0 = (run_object_method object_class_ s_2 thisobj) in
                                                         if string_eq class0
                                                             ("Function")
                                                         then let%number
                                                             (s10, n) = (run_object_get s_2 c thisobj
                                                                ("length")) in
                                                                let%run
                                                                  (s11, ilen) = (to_int32 s10 c (Coq_value_prim
                                                                                     (Coq_prim_number n))) in
                                                                     if  ilen <
                                                                         (number_of_int (LibList.length a))
                                                                     then res_spec s11 0.
                                                                     else res_spec s11
                                                                         (ilen -.
                                                                          (number_of_int (LibList.length a)))
                                                         else res_spec s_2 0.) in
                                                       let%run (s10, length0) = (vlength) in
                                                           let 
                                                             a0 = ({ attributes_data_value =
                                                                           (Coq_value_prim (Coq_prim_number
                                                                                              (of_int length0)));
                                                                         attributes_data_writable = false;
                                                                         attributes_data_enumerable = false;
                                                                         attributes_data_configurable = false }) in
                                                                let%some
                                                                  s11 = (object_heap_map_properties_pickable_option
                                                                     s10 l (fun p ->
                                                                         HeapStr.write p
                                                                           ("length")
                                                                           (Coq_attributes_data_of a0))) in
                                                                     let 
                                                                       vthrower = (Coq_value_object
                                                                                    (Coq_object_loc_prealloc
                                                                                       Coq_prealloc_throw_type_error)) in
                                                                          let  a1 = ({ attributes_accessor_get =
                                                                                          vthrower; attributes_accessor_set =
                                                                                                      vthrower;
                                                                                        attributes_accessor_enumerable = false;
                                                                                        attributes_accessor_configurable =
                                                                                          false }) in
                                                                              let%bool
                                                                                 (s12, x) = (object_define_own_prop s11 c l
                                                                                   ("caller")
                                                                                   (descriptor_of_attributes
                                                                                      (Coq_attributes_accessor_of a1))
                                                                                   false) in
                                                                                    let%bool
                                                                                       (s13, x0) = (object_define_own_prop s12 c
                                                                                         l
                                                                                         ("arguments")
                                                                                         (descriptor_of_attributes
                                                                                            (Coq_attributes_accessor_of
                                                                                               a1)) false) in
                                                                                          res_ter s13
                                                                                            (res_val (Coq_value_object l)))
    else run_error s Coq_native_error_type
  | Coq_prealloc_bool ->
    result_out
      (let v = (get_arg 0 args) in Coq_out_ter (s,
                                                           (res_val (Coq_value_prim (Coq_prim_bool
                                                                                       (convert_value_to_boolean v))))))
  | Coq_prealloc_bool_proto_to_string ->
    (match vthis with
     | Coq_value_prim p ->
       (match p with
        | Coq_prim_undef -> run_error s Coq_native_error_type
        | Coq_prim_null -> run_error s Coq_native_error_type
        | Coq_prim_bool b0 ->
          res_ter s
            (res_val (Coq_value_prim (Coq_prim_string
                                        (convert_bool_to_string b0))))
        | Coq_prim_number n -> run_error s Coq_native_error_type
        | Coq_prim_string s0 -> run_error s Coq_native_error_type)
     | Coq_value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s Coq_native_error_type) (fun s0 ->
             if string_eq s0
                 ("Boolean")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
                 (run_error s Coq_native_error_type) (fun wo ->
                     match wo with
                     | Some v ->
                       (match v with
                        | Coq_value_prim p ->
                          (match p with
                           | Coq_prim_undef -> run_error s Coq_native_error_type
                           | Coq_prim_null -> run_error s Coq_native_error_type
                           | Coq_prim_bool b0 ->
                             res_ter s
                               (res_val (Coq_value_prim (Coq_prim_string
                                                           (convert_bool_to_string b0))))
                           | Coq_prim_number n ->
                             run_error s Coq_native_error_type
                           | Coq_prim_string s1 ->
                             run_error s Coq_native_error_type)
                        | Coq_value_object o -> run_error s Coq_native_error_type)
                     | None -> run_error s Coq_native_error_type)
             else run_error s Coq_native_error_type))
  | Coq_prealloc_bool_proto_value_of ->
    (match vthis with
     | Coq_value_prim p ->
       (match p with
        | Coq_prim_undef -> run_error s Coq_native_error_type
        | Coq_prim_null -> run_error s Coq_native_error_type
        | Coq_prim_bool b0 ->
          res_ter s (res_val (Coq_value_prim (Coq_prim_bool b0)))
        | Coq_prim_number n -> run_error s Coq_native_error_type
        | Coq_prim_string s0 -> run_error s Coq_native_error_type)
     | Coq_value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s Coq_native_error_type) (fun s0 ->
             if string_eq s0
                 ("Boolean")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
                 (run_error s Coq_native_error_type) (fun wo ->
                     match wo with
                     | Some v ->
                       (match v with
                        | Coq_value_prim p ->
                          (match p with
                           | Coq_prim_undef -> run_error s Coq_native_error_type
                           | Coq_prim_null -> run_error s Coq_native_error_type
                           | Coq_prim_bool b0 ->
                             res_ter s
                               (res_val (Coq_value_prim (Coq_prim_bool b0)))
                           | Coq_prim_number n ->
                             run_error s Coq_native_error_type
                           | Coq_prim_string s1 ->
                             run_error s Coq_native_error_type)
                        | Coq_value_object o -> run_error s Coq_native_error_type)
                     | None -> run_error s Coq_native_error_type)
             else run_error s Coq_native_error_type))
  | Coq_prealloc_number ->
    if list_eq_nil_decidable args
    then result_out (Coq_out_ter (s,
                                  (res_val (Coq_value_prim (Coq_prim_number JsNumber.zero)))))
    else let v = get_arg 0 args in to_number s c v
  | Coq_prealloc_number_proto_value_of ->
    (match vthis with
     | Coq_value_prim p ->
       (match p with
        | Coq_prim_undef -> run_error s Coq_native_error_type
        | Coq_prim_null -> run_error s Coq_native_error_type
        | Coq_prim_bool b0 -> run_error s Coq_native_error_type
        | Coq_prim_number n ->
          res_ter s (res_val (Coq_value_prim (Coq_prim_number n)))
        | Coq_prim_string s0 -> run_error s Coq_native_error_type)
     | Coq_value_object l ->
       ifx_some_or_default (run_object_method object_class_ s l)
         (run_error s Coq_native_error_type) (fun s0 ->
             if string_eq s0 ("Number")
             then ifx_some_or_default (run_object_method object_prim_value_ s l)
                 (run_error s Coq_native_error_type) (fun wo ->
                     match wo with
                     | Some v ->
                       (match v with
                        | Coq_value_prim p ->
                          (match p with
                           | Coq_prim_undef -> run_error s Coq_native_error_type
                           | Coq_prim_null -> run_error s Coq_native_error_type
                           | Coq_prim_bool b0 -> run_error s Coq_native_error_type
                           | Coq_prim_number n ->
                             res_ter s
                               (res_val (Coq_value_prim (Coq_prim_number n)))
                           | Coq_prim_string s1 ->
                             run_error s Coq_native_error_type)
                        | Coq_value_object o -> run_error s Coq_native_error_type)
                     | None -> run_error s Coq_native_error_type)
             else run_error s Coq_native_error_type))
  | Coq_prealloc_array ->
    run_construct_prealloc s c Coq_prealloc_array args
  | Coq_prealloc_array_is_array ->
    let  arg = (get_arg 0 args) in begin
        match arg with
        | Coq_value_prim p ->
          res_ter s (res_val (Coq_value_prim (Coq_prim_bool false)))
        | Coq_value_object arg0 ->
          let%some class0= (run_object_method object_class_ s arg0) in
              if string_eq class0 ("Array")
              then res_ter s (res_val (Coq_value_prim (Coq_prim_bool true)))
              else res_ter s (res_val (Coq_value_prim (Coq_prim_bool false)))
    end
  | Coq_prealloc_array_proto_to_string ->
    let%object (s0, array) = (to_object s vthis) in
        let%value
          (s1, vfunc) = (run_object_get s0 c array ("join")) in
             if is_callable_dec s1 vfunc
             then (match vfunc with
                 | Coq_value_prim p ->
                   (fun s m -> Debug.impossible_with_heap_because __LOC__ s m; Coq_result_impossible)
                     s1
                     ("Value is callable, but isn\'t an object.")
                 | Coq_value_object func ->
                   run_call s1 c func (Coq_value_object array) [])
             else run_call_prealloc s1 c
                 Coq_prealloc_object_proto_to_string (Coq_value_object array)
                 []
  | Coq_prealloc_array_proto_join ->
    let  vsep = (get_arg 0 args) in
        let%object (s0, l) = (to_object s vthis) in
            let%value
               (s1, vlen) = (run_object_get s0 c l
                 ("length")) in
                  let%run (s2, ilen) = (to_uint32 s1 c vlen) in
                      let
                         rsep = (if not
                            (value_compare vsep (Coq_value_prim Coq_prim_undef))
                         then vsep
                         else Coq_value_prim (Coq_prim_string (","))) in
                            let%string (s3, sep) = (to_string s2 c rsep) in
                                if ilen = 0.0
                                then res_ter s3
                                    (res_val (Coq_value_prim (Coq_prim_string "")))
                                else let 
                                    sR = (valueToStringForJoin s3 c l 0.) in
                                       let%run (s4, sR0) = (sR) in
                                           run_array_join_elements s4 c l 1. ilen sep sR0
  | Coq_prealloc_array_proto_pop ->
    let%object (s0, l) = (to_object s vthis) in
        let%value
           (s1, vlen) = (run_object_get s0 c l
             ("length")) in
              let%run (s2, ilen) = (to_uint32 s1 c vlen) in
                  if ilen = 0.0
                  then let%not_throw
                      (s3, x) = (object_put s2 c l
                         ("length")
                         (Coq_value_prim (Coq_prim_number JsNumber.zero)) throw_true) in
                         result_out (Coq_out_ter (s3,
                                                  (res_val (Coq_value_prim Coq_prim_undef))))
                  else let%string
                       (s3, sindx) = (to_string s2 c (Coq_value_prim (Coq_prim_number
                                                         (of_int (ilen -. 1.))))) in
                          let%value 
                            (s4, velem) = (run_object_get s3 c l sindx) in
                               let%not_throw
                                 (s5, x) = (object_delete_default s4 c l sindx throw_true) in
                                    let%not_throw
                                      (s6, x0) = (object_put s5 c l
                                         ("length")
                                         (Coq_value_prim (Coq_prim_string sindx)) throw_true) in
                                         result_out (Coq_out_ter (s6, (res_val velem)))
  | Coq_prealloc_array_proto_push ->
    let%object (s0, l) = (to_object s vthis) in
        let%value
           (s1, vlen) = (run_object_get s0 c l
             ("length")) in
              let%run (s2, ilen) = (to_uint32 s1 c vlen) in
                  push s2 c l args ilen
  | Coq_prealloc_string ->
    if list_eq_nil_decidable args
    then res_ter s (res_val (Coq_value_prim (Coq_prim_string "")))
    else let  value0 = (get_arg 0 args) in
        let%string (s0, s1) = (to_string s c value0) in
            res_ter s0 (res_val (Coq_value_prim (Coq_prim_string s1)))
  | Coq_prealloc_string_proto_to_string ->
    (match vthis with
     | Coq_value_prim p ->
       if type_compare (type_of vthis) Coq_type_string
       then res_ter s (res_val vthis)
       else run_error s Coq_native_error_type
     | Coq_value_object l ->
       let%some s0= (run_object_method object_class_ s l) in
           if string_eq s0 ("String")
           then run_object_prim_value s l
           else run_error s Coq_native_error_type)
  | Coq_prealloc_string_proto_value_of ->
    (match vthis with
     | Coq_value_prim p ->
       if type_compare (type_of vthis) Coq_type_string
       then res_ter s (res_val vthis)
       else run_error s Coq_native_error_type
     | Coq_value_object l ->
       let%some s0= (run_object_method object_class_ s l) in
           if string_eq s0 ("String")
           then run_object_prim_value s l
           else run_error s Coq_native_error_type)
  | Coq_prealloc_error ->
    let  v = (get_arg 0 args) in
        build_error s (Coq_value_object (Coq_object_loc_prealloc
                                           Coq_prealloc_error_proto)) v
  | Coq_prealloc_native_error ne ->
    let  v = (get_arg 0 args) in
    build_error s (Coq_value_object (Coq_object_loc_prealloc
                                       (Coq_prealloc_native_error_proto ne))) v
  | Coq_prealloc_throw_type_error -> run_error s Coq_native_error_type
  | _ ->
    (fun s -> Debug.not_yet_implemented_because __LOC__ s; Coq_result_impossible)
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

(** val run_javascript : prog -> result **)

and run_javascript p =
  let c = execution_ctx_initial (prog_intro_strictness p) in
  let%void s_2 =
    execution_ctx_binding_inst state_initial c Coq_codetype_global None p [] in
  run_prog s_2 c p
