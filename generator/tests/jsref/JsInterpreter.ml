open Datatypes
open JsCommon
open JsCommonAux
open JsInit
open JsInterpreterMonads
(*open JsNumber*) 
open JsPreliminary
open JsSyntax
open JsSyntaxAux
open JsSyntaxInfos
open LibBool
open LibFunc
open LibList
open LibOption
open LibProd
open LibReflect
open LibString
open LibTactics
open List0
open Shared
(*open String0*)
let append x y = strappend x y (* hack for compatibility, to do cleanup *)
let length x = strlength x (* hack for compatibility, to do cleanup *)

type __ = unit
let __ = ()

(** val build_error : state -> value -> value -> result **)

let build_error s vproto vmsg =
  let o = object_new vproto ("Error") in
  let (l, s') = object_alloc s o in
  if value_comparable vmsg (Coq_value_prim Coq_prim_undef)
  then result_out (Coq_out_ter (s', (res_val (Coq_value_object l))))
  else (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
         ("Need [to_string] (this function shall be put in [runs_type].)")

(** val run_error : state -> native_error -> 'a1 specres **)

let run_error s ne =
  if_object
    (build_error s (Coq_value_object (Coq_object_loc_prealloc
      (Coq_prealloc_native_error_proto ne))) (Coq_value_prim Coq_prim_undef))
    (fun s' l -> Coq_result_some (Coq_specret_out (Coq_out_ter (s',
    (res_throw (Coq_resvalue_value (Coq_value_object l)))))))

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

let run_object_method proj s l =
   let opt = object_binds_pickable_option s l in
     begin match opt with
       | None -> prerr_endline ("Warning: in run_object_method the location" ^ (Prheap.prloc l) ^ " is unfetchable." )
       | _ -> ()
     end;
     LibOption.map proj opt
 
 (** val run_object_heap_set_extensible :
    bool -> state -> object_loc -> state option **)

let run_object_heap_set_extensible b s l =
  LibOption.map (fun o -> object_write s l (object_set_extensible o b))
    (object_binds_pickable_option s l)

let run_object_heap_set_extensible b s l =
   let opt = object_binds_pickable_option s l in
     begin match opt with
       | None -> prerr_endline ("Warning: in run_object_heap_set_extensible the location " ^ (Prheap.prloc l) ^ " is unfetchable." )
       | _ -> ()
     end;
     LibOption.map (fun o -> object_write s l (object_set_extensible o b)) opt
 
 type runs_type = { runs_type_expr : (state -> execution_ctx -> expr ->
                                    result);
                   runs_type_stat : (state -> execution_ctx -> stat ->
                                    result);
                   runs_type_prog : (state -> execution_ctx -> prog ->
                                    result);
                   runs_type_call : (state -> execution_ctx -> object_loc ->
                                    value -> value list -> result);
                   runs_type_call_prealloc : (state -> execution_ctx ->
                                             prealloc -> value -> value list
                                             -> result);
                   runs_type_construct : (state -> execution_ctx -> construct
                                         -> object_loc -> value list ->
                                         result);
                   runs_type_function_has_instance : (state -> object_loc ->
                                                     value -> result);
                   runs_type_object_has_instance : (state -> execution_ctx ->
                                                   builtin_has_instance ->
                                                   object_loc -> value ->
                                                   result);
                   runs_type_get_args_for_apply : (state -> execution_ctx ->
                                                  object_loc -> float ->
                                                  float -> value list
                                                  specres);
                   runs_type_stat_while : (state -> execution_ctx -> resvalue
                                          -> label_set -> expr -> stat ->
                                          result);
                   runs_type_stat_do_while : (state -> execution_ctx ->
                                             resvalue -> label_set -> expr ->
                                             stat -> result);
                   runs_type_stat_for_loop : (state -> execution_ctx ->
                                             label_set -> resvalue -> expr
                                             option -> expr option -> stat ->
                                             result);
                   runs_type_object_delete : (state -> execution_ctx ->
                                             object_loc -> prop_name -> bool
                                             -> result);
                   runs_type_object_get_own_prop : (state -> execution_ctx ->
                                                   object_loc -> prop_name ->
                                                   full_descriptor specres);
                   runs_type_object_get_prop : (state -> execution_ctx ->
                                               object_loc -> prop_name ->
                                               full_descriptor specres);
                   runs_type_object_get : (state -> execution_ctx ->
                                          object_loc -> prop_name -> result);
                   runs_type_object_proto_is_prototype_of : (state ->
                                                            object_loc ->
                                                            object_loc ->
                                                            result);
                   runs_type_object_put : (state -> execution_ctx ->
                                          object_loc -> prop_name -> value ->
                                          strictness_flag -> result);
                   runs_type_equal : (state -> execution_ctx -> value ->
                                     value -> result);
                   runs_type_to_integer : (state -> execution_ctx -> value ->
                                          result);
                   runs_type_to_string : (state -> execution_ctx -> value ->
                                         result);
                   runs_type_array_join_elements : (state -> execution_ctx ->
                                                   object_loc -> float ->
                                                   float -> string ->
                                                   string -> result);
                   runs_type_array_element_list : (state -> execution_ctx ->
                                                  object_loc -> expr option
                                                  list -> float -> result);
                   runs_type_object_define_own_prop_array_loop : (state ->
                                                                 execution_ctx
                                                                 ->
                                                                 object_loc
                                                                 -> float ->
                                                                 float ->
                                                                 descriptor
                                                                 -> bool ->
                                                                 bool ->
                                                                 (state ->
                                                                 prop_name ->
                                                                 descriptor
                                                                 ->
                                                                 strictness_flag
                                                                 -> __
                                                                 specres) ->
                                                                 result) }

(** val runs_type_expr :
    runs_type -> state -> execution_ctx -> expr -> result **)

let runs_type_expr x = x.runs_type_expr

(** val runs_type_stat :
    runs_type -> state -> execution_ctx -> stat -> result **)

let runs_type_stat x = x.runs_type_stat

(** val runs_type_prog :
    runs_type -> state -> execution_ctx -> prog -> result **)

let runs_type_prog x = x.runs_type_prog

(** val runs_type_call :
    runs_type -> state -> execution_ctx -> object_loc -> value -> value list
    -> result **)

let runs_type_call x = x.runs_type_call

(** val runs_type_call_prealloc :
    runs_type -> state -> execution_ctx -> prealloc -> value -> value list ->
    result **)

let runs_type_call_prealloc x = x.runs_type_call_prealloc

(** val runs_type_construct :
    runs_type -> state -> execution_ctx -> construct -> object_loc -> value
    list -> result **)

let runs_type_construct x = x.runs_type_construct

(** val runs_type_function_has_instance :
    runs_type -> state -> object_loc -> value -> result **)

let runs_type_function_has_instance x = x.runs_type_function_has_instance

(** val runs_type_object_has_instance :
    runs_type -> state -> execution_ctx -> builtin_has_instance -> object_loc
    -> value -> result **)

let runs_type_object_has_instance x = x.runs_type_object_has_instance

(** val runs_type_get_args_for_apply :
    runs_type -> state -> execution_ctx -> object_loc -> float -> float ->
    value list specres **)

let runs_type_get_args_for_apply x = x.runs_type_get_args_for_apply

(** val runs_type_stat_while :
    runs_type -> state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

let runs_type_stat_while x = x.runs_type_stat_while

(** val runs_type_stat_do_while :
    runs_type -> state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

let runs_type_stat_do_while x = x.runs_type_stat_do_while

(** val runs_type_stat_for_loop :
    runs_type -> state -> execution_ctx -> label_set -> resvalue -> expr
    option -> expr option -> stat -> result **)

let runs_type_stat_for_loop x = x.runs_type_stat_for_loop

(** val runs_type_object_delete :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> bool ->
    result **)

let runs_type_object_delete x = x.runs_type_object_delete

(** val runs_type_object_get_own_prop :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

let runs_type_object_get_own_prop x = x.runs_type_object_get_own_prop

(** val runs_type_object_get_prop :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

let runs_type_object_get_prop x = x.runs_type_object_get_prop

(** val runs_type_object_get :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> result **)

let runs_type_object_get x = x.runs_type_object_get

(** val runs_type_object_proto_is_prototype_of :
    runs_type -> state -> object_loc -> object_loc -> result **)

let runs_type_object_proto_is_prototype_of x = x.runs_type_object_proto_is_prototype_of

(** val runs_type_object_put :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> value
    -> strictness_flag -> result **)

let runs_type_object_put x = x.runs_type_object_put

(** val runs_type_equal :
    runs_type -> state -> execution_ctx -> value -> value -> result **)

let runs_type_equal x = x.runs_type_equal

(** val runs_type_to_string :
    runs_type -> state -> execution_ctx -> value -> result **)

let runs_type_to_string x = x.runs_type_to_string

(** val runs_type_array_join_elements :
    runs_type -> state -> execution_ctx -> object_loc -> float -> float ->
    string -> string -> result **)

let runs_type_array_join_elements x = x.runs_type_array_join_elements

(** val runs_type_array_element_list :
    runs_type -> state -> execution_ctx -> object_loc -> expr option list ->
    float -> result **)

let runs_type_array_element_list x = x.runs_type_array_element_list

(** val runs_type_object_define_own_prop_array_loop :
    runs_type -> state -> execution_ctx -> object_loc -> float -> float ->
    descriptor -> bool -> bool -> (state -> prop_name -> descriptor ->
    strictness_flag -> __ specres) -> result **)

let runs_type_object_define_own_prop_array_loop x = x.runs_type_object_define_own_prop_array_loop

(** val object_has_prop :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> result **)

let object_has_prop runs0 s c l x =
  if_some (run_object_method object_has_prop_ s l) (fun b ->
    match b with Coq_builtin_has_prop_default ->
    if_spec (runs0.runs_type_object_get_prop s c l x) (fun s1 d ->
      res_ter s1
        (res_val (Coq_value_prim (Coq_prim_bool
          (not_decidable
            (full_descriptor_comparable d Coq_full_descriptor_undef)))))))

(** val object_get_builtin :
    runs_type -> state -> execution_ctx -> builtin_get -> value -> object_loc
    -> prop_name -> result **)

let object_get_builtin runs0 s c b vthis l x =
  let_binding (fun s0 l0 ->
    if_spec (runs0.runs_type_object_get_prop s0 c l0 x) (fun s1 d ->
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
            | Coq_value_object lf -> runs0.runs_type_call s1 c lf vthis []))))
    (fun def ->
    let_binding (fun s0 ->
      if_value (def s0 l) (fun s' v ->
        if spec_function_get_error_case_dec s' x v
        then run_error s' Coq_native_error_type
        else res_ter s' (res_val v))) (fun function0 ->
      match b with
      | Coq_builtin_get_default -> def s l
      | Coq_builtin_get_function -> function0 s
      | Coq_builtin_get_args_obj ->
        if_some (run_object_method object_parameter_map_ s l) (fun lmapo ->
          if_some lmapo (fun lmap ->
            if_spec (runs0.runs_type_object_get_own_prop s c lmap x)
              (fun s0 d ->
              match d with
              | Coq_full_descriptor_undef -> function0 s0
              | Coq_full_descriptor_some a ->
                runs0.runs_type_object_get s0 c lmap x)))))

(** val run_object_get :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> result **)

let run_object_get runs0 s c l x =
  if_some (run_object_method object_get_ s l) (fun b ->
    object_get_builtin runs0 s c b (Coq_value_object l) l x)

(** val run_object_get_prop :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

let run_object_get_prop runs0 s c l x =
  if_some (run_object_method object_get_prop_ s l) (fun b ->
    match b with Coq_builtin_get_prop_default ->
    if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s1 d ->
      if full_descriptor_comparable d Coq_full_descriptor_undef
      then if_some (run_object_method object_proto_ s1 l) (fun proto ->
             match proto with
             | Coq_value_prim p ->
               (match p with
                | Coq_prim_undef ->
                  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                    s1
                    ("Found a non-null primitive value as a prototype in [run_object_get_prop].")
                | Coq_prim_null -> res_spec s1 Coq_full_descriptor_undef
                | Coq_prim_bool b0 ->
                  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                    s1
                    ("Found a non-null primitive value as a prototype in [run_object_get_prop].")
                | Coq_prim_number n ->
                  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                    s1
                    ("Found a non-null primitive value as a prototype in [run_object_get_prop].")
                | Coq_prim_string s0 ->
                  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                    s1
                    ("Found a non-null primitive value as a prototype in [run_object_get_prop]."))
             | Coq_value_object lproto ->
               runs0.runs_type_object_get_prop s1 c lproto x)
      else res_spec s1 d))

(** val object_proto_is_prototype_of :
    runs_type -> state -> object_loc -> object_loc -> result **)

let object_proto_is_prototype_of runs0 s l0 l =
  if_some (run_object_method object_proto_ s l) (fun b ->
    match b with
    | Coq_value_prim p ->
      (match p with
       | Coq_prim_undef ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[run_object_method] returned a primitive in [object_proto_is_prototype_of_body].")
       | Coq_prim_null ->
         result_out (Coq_out_ter (s,
           (res_val (Coq_value_prim (Coq_prim_bool false)))))
       | Coq_prim_bool b0 ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[run_object_method] returned a primitive in [object_proto_is_prototype_of_body].")
       | Coq_prim_number n ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[run_object_method] returned a primitive in [object_proto_is_prototype_of_body].")
       | Coq_prim_string s0 ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[run_object_method] returned a primitive in [object_proto_is_prototype_of_body]."))
    | Coq_value_object l' ->
      if object_loc_comparable l' l0
      then result_out (Coq_out_ter (s,
             (res_val (Coq_value_prim (Coq_prim_bool true)))))
      else runs0.runs_type_object_proto_is_prototype_of s l0 l')

(** val object_default_value :
    runs_type -> state -> execution_ctx -> object_loc -> preftype option ->
    result **)

let object_default_value runs0 s c l prefo =
  if_some (run_object_method object_default_value_ s l) (fun b ->
    match b with Coq_builtin_default_value_default ->
    let gpref = unsome_default Coq_preftype_number prefo in
    let lpref = other_preftypes gpref in
    let_binding (fun s' x k ->
      if_value (run_object_get runs0 s' c l x) (fun s1 vfo ->
        if_some (run_callable s1 vfo) (fun co ->
          match co with
          | Some b0 ->
            if_object (result_out (Coq_out_ter (s1, (res_val vfo))))
              (fun s2 lfunc ->
              if_value
                (runs0.runs_type_call s2 c lfunc (Coq_value_object l) [])
                (fun s3 v ->
                match v with
                | Coq_value_prim w ->
                  result_out (Coq_out_ter (s3, (res_val (Coq_value_prim w))))
                | Coq_value_object l0 -> k s3))
          | None -> k s1))) (fun sub0 ->
      let_binding (method_of_preftype gpref) (fun gmeth ->
        sub0 s gmeth (fun s' ->
          let lmeth = method_of_preftype lpref in
          sub0 s' lmeth (fun s'' -> run_error s'' Coq_native_error_type)))))

(** val to_primitive :
    runs_type -> state -> execution_ctx -> value -> preftype option -> result **)

let to_primitive runs0 s c v prefo =
  match v with
  | Coq_value_prim w ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim w))))
  | Coq_value_object l ->
    if_prim (object_default_value runs0 s c l prefo) (fun s0 r ->
      res_ter s0 (res_val (Coq_value_prim r)))

(** val to_number :
    runs_type -> state -> execution_ctx -> value -> result **)

let to_number runs0 s c _foo_ = match _foo_ with
| Coq_value_prim w ->
  result_out (Coq_out_ter (s,
    (res_val (Coq_value_prim (Coq_prim_number (convert_prim_to_number w))))))
| Coq_value_object l ->
  if_prim
    (to_primitive runs0 s c (Coq_value_object l) (Some Coq_preftype_number))
    (fun s1 w ->
    res_ter s1
      (res_val (Coq_value_prim (Coq_prim_number (convert_prim_to_number w)))))

(** val to_integer :
    runs_type -> state -> execution_ctx -> value -> result **)

let to_integer runs0 s c v =
  if_number (to_number runs0 s c v) (fun s1 n ->
    res_ter s1
      (res_val (Coq_value_prim (Coq_prim_number
        (convert_number_to_integer n)))))

(** val to_int32 :
    runs_type -> state -> execution_ctx -> value -> float specres **)

let to_int32 runs0 s c v =
  if_number (to_number runs0 s c v) (fun s' n -> res_spec s' (JsNumber.to_int32 n))

(** val to_uint32 :
    runs_type -> state -> execution_ctx -> value -> float specres **)

let to_uint32 runs0 s c v =
  if_number (to_number runs0 s c v) (fun s' n -> res_spec s' (JsNumber.to_uint32 n))

(** val to_string :
    runs_type -> state -> execution_ctx -> value -> result **)

let to_string runs0 s c _foo_ = match _foo_ with
| Coq_value_prim w ->
  result_out (Coq_out_ter (s,
    (res_val (Coq_value_prim (Coq_prim_string (convert_prim_to_string w))))))
| Coq_value_object l ->
  if_prim
    (to_primitive runs0 s c (Coq_value_object l) (Some Coq_preftype_string))
    (fun s1 w ->
    res_ter s1
      (res_val (Coq_value_prim (Coq_prim_string (convert_prim_to_string w)))))

(** val object_can_put :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> result **)

let object_can_put runs0 s c l x =
  if_some (run_object_method object_can_put_ s l) (fun b ->
    match b with Coq_builtin_can_put_default ->
    if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s1 d ->
      match d with
      | Coq_full_descriptor_undef ->
        if_some (run_object_method object_proto_ s1 l) (fun vproto ->
          match vproto with
          | Coq_value_prim p ->
            (match p with
             | Coq_prim_undef ->
               (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                 s1
                 ("Non-null primitive get as a prototype value in [object_can_put].")
             | Coq_prim_null ->
               if_some (run_object_method object_extensible_ s1 l) (fun b0 ->
                 res_ter s1 (res_val (Coq_value_prim (Coq_prim_bool b0))))
             | Coq_prim_bool b0 ->
               (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                 s1
                 ("Non-null primitive get as a prototype value in [object_can_put].")
             | Coq_prim_number n ->
               (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                 s1
                 ("Non-null primitive get as a prototype value in [object_can_put].")
             | Coq_prim_string s0 ->
               (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                 s1
                 ("Non-null primitive get as a prototype value in [object_can_put]."))
          | Coq_value_object lproto ->
            if_spec (run_object_get_prop runs0 s1 c lproto x) (fun s2 d' ->
              match d' with
              | Coq_full_descriptor_undef ->
                if_some (run_object_method object_extensible_ s2 l)
                  (fun b0 ->
                  res_ter s2 (res_val (Coq_value_prim (Coq_prim_bool b0))))
              | Coq_full_descriptor_some a ->
                (match a with
                 | Coq_attributes_data_of ad ->
                   if_some (run_object_method object_extensible_ s2 l)
                     (fun ext ->
                     res_ter s2
                       (if ext
                        then res_val (Coq_value_prim (Coq_prim_bool
                               ad.attributes_data_writable))
                        else res_val (Coq_value_prim (Coq_prim_bool false))))
                 | Coq_attributes_accessor_of aa ->
                   res_ter s2
                     (res_val (Coq_value_prim (Coq_prim_bool
                       (not_decidable
                         (value_comparable aa.attributes_accessor_set
                           (Coq_value_prim Coq_prim_undef)))))))))
      | Coq_full_descriptor_some a ->
        (match a with
         | Coq_attributes_data_of ad ->
           res_ter s1
             (res_val (Coq_value_prim (Coq_prim_bool
               ad.attributes_data_writable)))
         | Coq_attributes_accessor_of aa ->
           res_ter s1
             (res_val (Coq_value_prim (Coq_prim_bool
               (not_decidable
                 (value_comparable aa.attributes_accessor_set (Coq_value_prim
                   Coq_prim_undef)))))))))

(** val run_object_define_own_prop_array_loop :
    runs_type -> state -> execution_ctx -> object_loc -> float -> float ->
    descriptor -> bool -> bool -> (state -> prop_name -> descriptor ->
    strictness_flag -> __ specres) -> result **)

let run_object_define_own_prop_array_loop runs0 s c l newLen oldLen newLenDesc newWritable throwcont def =
  if lt_int_decidable newLen oldLen
  then let_binding (oldLen -. 1.) (fun oldLen' ->
         if_string
           (to_string runs0 s c (Coq_value_prim (Coq_prim_number
             (of_int oldLen')))) (fun s0 slen ->
           if_bool (runs0.runs_type_object_delete s0 c l slen false)
             (fun s1 deleteSucceeded ->
             if not_decidable (bool_decidable deleteSucceeded)
             then let_binding
                    (descriptor_with_value newLenDesc (Some (Coq_value_prim
                      (Coq_prim_number (of_int (oldLen' +. 1.))))))
                    (fun newLenDesc0 ->
                    let_binding
                      (if not_decidable (bool_decidable newWritable)
                       then descriptor_with_writable newLenDesc0 (Some false)
                       else newLenDesc0) (fun newLenDesc1 ->
                      if_bool
                        (def s1 ("length")
                          newLenDesc1 false) (fun s2 x ->
                        out_error_or_cst s2 throwcont Coq_native_error_type
                          (Coq_value_prim (Coq_prim_bool false)))))
             else runs0.runs_type_object_define_own_prop_array_loop s1 c l
                    newLen oldLen' newLenDesc newWritable throwcont def)))
  else if not_decidable (bool_decidable newWritable)
       then def s ("length")
              { descriptor_value = None; descriptor_writable = (Some false);
              descriptor_get = None; descriptor_set = None;
              descriptor_enumerable = None; descriptor_configurable = None }
              false
       else res_ter s (res_val (Coq_value_prim (Coq_prim_bool true)))

(** val object_define_own_prop :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    descriptor -> strictness_flag -> result **)

let object_define_own_prop runs0 s c l x desc throwcont =
  let_binding (fun s0 throwcont0 ->
    out_error_or_cst s0 throwcont0 Coq_native_error_type (Coq_value_prim
      (Coq_prim_bool false))) (fun reject ->
    let_binding (fun s0 x0 desc0 throwcont0 ->
      if_spec (runs0.runs_type_object_get_own_prop s0 c l x0) (fun s1 d ->
        if_some (run_object_method object_extensible_ s1 l) (fun ext ->
          match d with
          | Coq_full_descriptor_undef ->
            if ext
            then let_binding
                   (if or_decidable (descriptor_is_generic_dec desc0)
                         (descriptor_is_data_dec desc0)
                    then Coq_attributes_data_of
                           (attributes_data_of_descriptor desc0)
                    else Coq_attributes_accessor_of
                           (attributes_accessor_of_descriptor desc0))
                   (fun a ->
                   if_some
                     (object_heap_map_properties_pickable_option s1 l
                       (fun p -> Heap.write p x0 a)) (fun s2 ->
                     res_ter s2
                       (res_val (Coq_value_prim (Coq_prim_bool true)))))
            else reject s1 throwcont0
          | Coq_full_descriptor_some a ->
            let_binding (fun s2 a0 ->
              let a' = attributes_update a0 desc0 in
              if_some
                (object_heap_map_properties_pickable_option s2 l (fun p ->
                  Heap.write p x0 a')) (fun s3 ->
                res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool true)))))
              (fun object_define_own_prop_write ->
              if descriptor_contains_dec (descriptor_of_attributes a) desc0
              then res_ter s1 (res_val (Coq_value_prim (Coq_prim_bool true)))
              else if attributes_change_enumerable_on_non_configurable_dec a
                        desc0
                   then reject s1 throwcont0
                   else if descriptor_is_generic_dec desc0
                        then object_define_own_prop_write s1 a
                        else if not_decidable
                                  (prop_eq_decidable
                                    (attributes_is_data_dec a)
                                    (descriptor_is_data_dec desc0))
                             then if attributes_configurable a
                                  then let_binding
                                         (match a with
                                          | Coq_attributes_data_of ad ->
                                            Coq_attributes_accessor_of
                                              (attributes_accessor_of_attributes_data
                                                ad)
                                          | Coq_attributes_accessor_of aa ->
                                            Coq_attributes_data_of
                                              (attributes_data_of_attributes_accessor
                                                aa)) (fun a' ->
                                         if_some
                                           (object_heap_map_properties_pickable_option
                                             s1 l (fun p ->
                                             Heap.write p x0 a')) (fun s2 ->
                                           object_define_own_prop_write s2 a'))
                                  else reject s1 throwcont0
                             else if and_decidable (attributes_is_data_dec a)
                                       (descriptor_is_data_dec desc0)
                                  then (match a with
                                        | Coq_attributes_data_of ad ->
                                          if attributes_change_data_on_non_configurable_dec
                                               ad desc0
                                          then reject s1 throwcont0
                                          else object_define_own_prop_write
                                                 s1 a
                                        | Coq_attributes_accessor_of a0 ->
                                          (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                                            s0
                                            ("data is not accessor in [defineOwnProperty]"))
                                  else if and_decidable
                                            (not_decidable
                                              (attributes_is_data_dec a))
                                            (descriptor_is_accessor_dec
                                              desc0)
                                       then (match a with
                                             | Coq_attributes_data_of a0 ->
                                               (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                                                 s0
                                                 ("accessor is not data in [defineOwnProperty]")
                                             | Coq_attributes_accessor_of aa ->
                                               if attributes_change_accessor_on_non_configurable_dec
                                                    aa desc0
                                               then reject s1 throwcont0
                                               else object_define_own_prop_write
                                                      s1 a)
                                       else (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                                              s0
                                              ("cases are mutually exclusives in [defineOwnProperty]")))))
      (fun def ->
      if_some (run_object_method object_define_own_prop_ s l) (fun b ->
        match b with
        | Coq_builtin_define_own_prop_default -> def s x desc throwcont
        | Coq_builtin_define_own_prop_array ->
          if_spec
            (runs0.runs_type_object_get_own_prop s c l
              ("length")) (fun s0 d ->
            match d with
            | Coq_full_descriptor_undef ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s0
                ("Array length property descriptor cannot be undefined.")
            | Coq_full_descriptor_some attr ->
              (match attr with
               | Coq_attributes_data_of a ->
                 let_binding a.attributes_data_value (fun oldLen ->
                   match oldLen with
                   | Coq_value_prim w ->
                     let_binding
                       (JsNumber.to_uint32 (convert_prim_to_number w))
                       (fun oldLen0 ->
                       let_binding desc.descriptor_value (fun descValueOpt ->
                         if string_comparable x
                              ("length")
                         then (match descValueOpt with
                               | Some descValue ->
                                 if_spec (to_uint32 runs0 s0 c descValue)
                                   (fun s1 newLen ->
                                   if_number (to_number runs0 s1 c descValue)
                                     (fun s2 newLenN ->
                                     if not_decidable
                                          (number_comparable (of_int newLen)
                                            newLenN)
                                     then run_error s2 Coq_native_error_range
                                     else let_binding
                                            (descriptor_with_value desc (Some
                                              (Coq_value_prim
                                              (Coq_prim_number
                                              (of_int newLen)))))
                                            (fun newLenDesc ->
                                            if le_int_decidable oldLen0
                                                 newLen
                                            then def s2
                                                   ("length")
                                                   newLenDesc throwcont
                                            else if not_decidable
                                                      (bool_decidable
                                                        a.attributes_data_writable)
                                                 then reject s2 throwcont
                                                 else let_binding
                                                        (match newLenDesc.descriptor_writable with
                                                         | Some b0 ->
                                                           if b0
                                                           then true
                                                           else false
                                                         | None -> true)
                                                        (fun newWritable ->
                                                        let_binding
                                                          (if not_decidable
                                                                (bool_decidable
                                                                  newWritable)
                                                           then descriptor_with_writable
                                                                  newLenDesc
                                                                  (Some true)
                                                           else newLenDesc)
                                                          (fun newLenDesc0 ->
                                                          if_bool
                                                            (def s2
                                                              ("length")
                                                              newLenDesc0
                                                              throwcont)
                                                            (fun s3 succ ->
                                                            if not_decidable
                                                                 (bool_decidable
                                                                   succ)
                                                            then res_ter s3
                                                                   (res_val
                                                                    (Coq_value_prim
                                                                    (Coq_prim_bool
                                                                    false)))
                                                            else run_object_define_own_prop_array_loop
                                                                   runs0 s3 c
                                                                   l newLen
                                                                   oldLen0
                                                                   newLenDesc0
                                                                   newWritable
                                                                   throwcont
                                                                   def))))))
                               | None ->
                                 def s0
                                   ("length")
                                   desc throwcont)
                         else if_spec
                                (to_uint32 runs0 s0 c (Coq_value_prim
                                  (Coq_prim_string x))) (fun s1 ilen ->
                                if_string
                                  (to_string runs0 s1 c (Coq_value_prim
                                    (Coq_prim_number (of_int ilen))))
                                  (fun s2 slen ->
                                  if and_decidable (string_comparable x slen)
                                       (not_decidable
                                         (float_eq ilen 
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
                                           1.)))))))))))))))))))))))))))))))))
                                  then if_spec
                                         (to_uint32 runs0 s2 c
                                           (Coq_value_prim (Coq_prim_string
                                           x))) (fun s3 index ->
                                         if and_decidable
                                              (le_int_decidable oldLen0
                                                index)
                                              (not_decidable
                                                (bool_decidable
                                                  a.attributes_data_writable))
                                         then reject s3 throwcont
                                         else if_bool
                                                (def s3 x desc false)
                                                (fun s4 b0 ->
                                                if not_decidable
                                                     (bool_decidable b0)
                                                then reject s4 throwcont
                                                else if le_int_decidable
                                                          oldLen0 index
                                                     then let a0 =
                                                            descriptor_with_value
                                                              (descriptor_of_attributes
                                                                (Coq_attributes_data_of
                                                                a)) (Some
                                                              (Coq_value_prim
                                                              (Coq_prim_number
                                                              (of_int
                                                                (index +. 1.)))))
                                                          in
                                                          def s4
                                                            ("length")
                                                            a0 false
                                                     else res_ter s4
                                                            (res_val
                                                              (Coq_value_prim
                                                              (Coq_prim_bool
                                                              true)))))
                                  else def s2 x desc throwcont))))
                   | Coq_value_object l0 ->
                     (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                       s0
                       ("Spec asserts length of array is number."))
               | Coq_attributes_accessor_of a ->
                 (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                   s0
                   ("Array length property descriptor cannot be accessor.")))
        | Coq_builtin_define_own_prop_args_obj ->
          if_some (run_object_method object_parameter_map_ s l) (fun lmapo ->
            if_some lmapo (fun lmap ->
              if_spec (runs0.runs_type_object_get_own_prop s c lmap x)
                (fun s0 d ->
                if_bool (def s0 x desc false) (fun s1 b0 ->
                  if b0
                  then let_binding (fun s2 ->
                         res_ter s2
                           (res_val (Coq_value_prim (Coq_prim_bool true))))
                         (fun follow ->
                         match d with
                         | Coq_full_descriptor_undef -> follow s1
                         | Coq_full_descriptor_some a ->
                           if descriptor_is_accessor_dec desc
                           then if_bool
                                  (runs0.runs_type_object_delete s1 c lmap x
                                    false) (fun s2 x0 -> follow s2)
                           else let_binding (fun s2 ->
                                  if option_comparable bool_comparable
                                       desc.descriptor_writable (Some false)
                                  then if_bool
                                         (runs0.runs_type_object_delete s2 c
                                           lmap x false) (fun s3 x0 ->
                                         follow s3)
                                  else follow s2) (fun follow0 ->
                                  match desc.descriptor_value with
                                  | Some v ->
                                    if_void
                                      (runs0.runs_type_object_put s1 c lmap x
                                        v throwcont) (fun s2 -> follow0 s2)
                                  | None -> follow0 s1))
                  else reject s1 throwcont)))))))

(** val run_to_descriptor :
    runs_type -> state -> execution_ctx -> value -> descriptor specres **)

let run_to_descriptor runs0 s c _foo_ = match _foo_ with
| Coq_value_prim p -> throw_result (run_error s Coq_native_error_type)
| Coq_value_object l ->
  let sub0 = fun s0 desc name conv k ->
    if_bool (object_has_prop runs0 s0 c l name) (fun s1 has ->
      if neg has
      then k s1 desc
      else if_value (run_object_get runs0 s1 c l name) (fun s2 v0 ->
             if_spec (conv s2 v0 desc) k))
  in
  sub0 s descriptor_intro_empty
    ("enumerable")
    (fun s1 v1 desc ->
    let b = convert_value_to_boolean v1 in
    res_spec s1 (descriptor_with_enumerable desc (Some b))) (fun s1' desc ->
    sub0 s1' desc
      ("configurable")
      (fun s2 v2 desc0 ->
      let b = convert_value_to_boolean v2 in
      res_spec s2 (descriptor_with_configurable desc0 (Some b)))
      (fun s2' desc0 ->
      sub0 s2' desc0 ("value")
        (fun s3 v3 desc1 ->
        res_spec s3 (descriptor_with_value desc1 (Some v3)))
        (fun s3' desc1 ->
        sub0 s3' desc1
          ("writable")
          (fun s4 v4 desc2 ->
          let b = convert_value_to_boolean v4 in
          res_spec s4 (descriptor_with_writable desc2 (Some b)))
          (fun s4' desc2 ->
          sub0 s4' desc2 ("get") (fun s5 v5 desc3 ->
            if and_decidable
                 (prop_eq_decidable (is_callable_dec s5 v5)
                   (bool_decidable false))
                 (not_decidable
                   (value_comparable v5 (Coq_value_prim Coq_prim_undef)))
            then throw_result (run_error s5 Coq_native_error_type)
            else res_spec s5 (descriptor_with_get desc3 (Some v5)))
            (fun s5' desc3 ->
            sub0 s5' desc3 ("set") (fun s6 v6 desc4 ->
              if and_decidable
                   (prop_eq_decidable (is_callable_dec s6 v6)
                     (bool_decidable false))
                   (not_decidable
                     (value_comparable v6 (Coq_value_prim Coq_prim_undef)))
              then throw_result (run_error s6 Coq_native_error_type)
              else res_spec s6 (descriptor_with_set desc4 (Some v6)))
              (fun s7 desc4 ->
              if and_decidable
                   (or_decidable
                     (not_decidable
                       (option_comparable value_comparable
                         desc4.descriptor_get None))
                     (not_decidable
                       (option_comparable value_comparable
                         desc4.descriptor_set None)))
                   (or_decidable
                     (not_decidable
                       (option_comparable value_comparable
                         desc4.descriptor_value None))
                     (not_decidable
                       (option_comparable bool_comparable
                         desc4.descriptor_writable None)))
              then throw_result (run_error s7 Coq_native_error_type)
              else res_spec s7 desc4))))))

(** val prim_new_object : state -> prim -> result **)

let prim_new_object s _foo_ = match _foo_ with
| Coq_prim_undef ->
  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
    s
    ("[prim_new_object] received an null or undef.")
| Coq_prim_null ->
  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
    s
    ("[prim_new_object] received an null or undef.")
| Coq_prim_bool b ->
  result_out
    (let_binding
      (object_new (Coq_value_object (Coq_object_loc_prealloc
        Coq_prealloc_bool_proto))
        ("Boolean")) (fun o1 ->
      let_binding
        (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_bool b)))
        (fun o ->
        let (l, s1) = object_alloc s o in
        Coq_out_ter (s1, (res_val (Coq_value_object l))))))
| Coq_prim_number n ->
  result_out
    (let_binding
      (object_new (Coq_value_object (Coq_object_loc_prealloc
        Coq_prealloc_number_proto))
        ("Number")) (fun o1 ->
      let_binding
        (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_number n)))
        (fun o ->
        let (l, s1) = object_alloc s o in
        Coq_out_ter (s1, (res_val (Coq_value_object l))))))
| Coq_prim_string s0 ->
  let_binding
    (object_new (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_string_proto))
      ("String")) (fun o2 ->
    let_binding
      (object_with_get_own_property o2 Coq_builtin_get_own_prop_string)
      (fun o1 ->
      let_binding
        (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_string
          s0))) (fun o ->
        let (l, s1) = object_alloc s o in
        if_some
          (object_heap_map_properties_pickable_option s1 l (fun p ->
            Heap.write p ("length")
              (Coq_attributes_data_of
              (attributes_data_intro_constant (Coq_value_prim
                (Coq_prim_number (number_of_int (strlength s0))))))))
          (fun s' -> res_ter s' (res_val (Coq_value_object l))))))

(** val to_object : state -> value -> result **)

let to_object s _foo_ = match _foo_ with
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

let run_object_prim_value s l =
  if_some (run_object_method object_prim_value_ s l) (fun ov ->
    if_some ov (fun v -> res_ter s (res_val v)))

(** val prim_value_get :
    runs_type -> state -> execution_ctx -> value -> prop_name -> result **)

let prim_value_get runs0 s c v x =
  if_object (to_object s v) (fun s' l ->
    object_get_builtin runs0 s' c Coq_builtin_get_default v l x)

(** val env_record_has_binding :
    runs_type -> state -> execution_ctx -> env_loc -> prop_name -> result **)

let env_record_has_binding runs0 s c l x =
  if_some (env_record_binds_pickable_option s l) (fun e ->
    match e with
    | Coq_env_record_decl ed ->
      result_out (Coq_out_ter (s,
        (res_val (Coq_value_prim (Coq_prim_bool
          (Heap.indom_decidable string_comparable ed x))))))
    | Coq_env_record_object (l0, pt) -> object_has_prop runs0 s c l0 x)

(** val lexical_env_get_identifier_ref :
    runs_type -> state -> execution_ctx -> lexical_env -> prop_name ->
    strictness_flag -> ref specres **)

let rec lexical_env_get_identifier_ref runs0 s c x x0 str =
  match x with
  | [] ->
    res_spec s (ref_create_value (Coq_value_prim Coq_prim_undef) x0 str)
  | l :: x' ->
    if_bool (env_record_has_binding runs0 s c l x0) (fun s1 has ->
      if has
      then res_spec s1 (ref_create_env_loc l x0 str)
      else lexical_env_get_identifier_ref runs0 s1 c x' x0 str)

(** val object_delete_default :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    strictness_flag -> result **)

let object_delete_default runs0 s c l x str =
  if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s1 d ->
    match d with
    | Coq_full_descriptor_undef ->
      res_ter s1 (res_val (Coq_value_prim (Coq_prim_bool true)))
    | Coq_full_descriptor_some a ->
      if attributes_configurable a
      then if_some
             (object_heap_map_properties_pickable_option s1 l (fun p ->
               Heap.rem string_comparable p x)) (fun s' ->
             res_ter s' (res_val (Coq_value_prim (Coq_prim_bool true))))
      else out_error_or_cst s1 str Coq_native_error_type (Coq_value_prim
             (Coq_prim_bool false)))

(** val object_delete :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    strictness_flag -> result **)

let object_delete runs0 s c l x str =
  if_some (run_object_method object_delete_ s l) (fun b ->
    match b with
    | Coq_builtin_delete_default -> object_delete_default runs0 s c l x str
    | Coq_builtin_delete_args_obj ->
      if_some (run_object_method object_parameter_map_ s l) (fun mo ->
        if_some mo (fun m ->
          if_spec (runs0.runs_type_object_get_own_prop s c m x) (fun s1 d ->
            if_bool (object_delete_default runs0 s1 c l x str) (fun s2 b0 ->
              if b0
              then (match d with
                    | Coq_full_descriptor_undef ->
                      res_ter s2
                        (res_val (Coq_value_prim (Coq_prim_bool b0)))
                    | Coq_full_descriptor_some a ->
                      if_bool (runs0.runs_type_object_delete s2 c m x false)
                        (fun s3 b' ->
                        res_ter s3
                          (res_val (Coq_value_prim (Coq_prim_bool b0)))))
              else res_ter s2 (res_val (Coq_value_prim (Coq_prim_bool b0))))))))

(** val env_record_delete_binding :
    runs_type -> state -> execution_ctx -> env_loc -> prop_name -> result **)

let env_record_delete_binding runs0 s c l x =
  if_some (env_record_binds_pickable_option s l) (fun e ->
    match e with
    | Coq_env_record_decl ed ->
      (match Heap.read_option string_comparable ed x with
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
            let s' =
              env_record_write s l (Coq_env_record_decl
                (decl_env_record_rem ed x))
            in
            result_out (Coq_out_ter (s',
              (res_val (Coq_value_prim (Coq_prim_bool true))))))
       | None ->
         result_out (Coq_out_ter (s,
           (res_val (Coq_value_prim (Coq_prim_bool true))))))
    | Coq_env_record_object (l0, pt) ->
      runs0.runs_type_object_delete s c l0 x throw_false)

(** val env_record_implicit_this_value : state -> env_loc -> value option **)

let env_record_implicit_this_value s l =
  if_some_or_default (env_record_binds_pickable_option s l) None (fun e ->
    Some
    (match e with
     | Coq_env_record_decl ed -> Coq_value_prim Coq_prim_undef
     | Coq_env_record_object (l0, provide_this) ->
       if provide_this
       then Coq_value_object l0
       else Coq_value_prim Coq_prim_undef))

(** val identifier_resolution :
    runs_type -> state -> execution_ctx -> prop_name -> ref specres **)

let identifier_resolution runs0 s c x =
  let x0 = c.execution_ctx_lexical_env in
  let str = c.execution_ctx_strict in
  lexical_env_get_identifier_ref runs0 s c x0 x str

(** val env_record_get_binding_value :
    runs_type -> state -> execution_ctx -> env_loc -> prop_name ->
    strictness_flag -> result **)

let env_record_get_binding_value runs0 s c l x str =
  if_some (env_record_binds_pickable_option s l) (fun e ->
    match e with
    | Coq_env_record_decl ed ->
      if_some (Heap.read_option string_comparable ed x) (fun rm ->
        let (mu, v) = rm in
        if mutability_comparable mu Coq_mutability_uninitialized_immutable
        then out_error_or_cst s str Coq_native_error_ref (Coq_value_prim
               Coq_prim_undef)
        else res_ter s (res_val v))
    | Coq_env_record_object (l0, pt) ->
      if_bool (object_has_prop runs0 s c l0 x) (fun s1 has ->
        if has
        then run_object_get runs0 s1 c l0 x
        else out_error_or_cst s1 str Coq_native_error_ref (Coq_value_prim
               Coq_prim_undef)))

(** val ref_get_value :
    runs_type -> state -> execution_ctx -> resvalue -> value specres **)

let ref_get_value runs0 s c _foo_ = match _foo_ with
| Coq_resvalue_empty ->
  (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
    s
    ("[ref_get_value] received an empty result.")
| Coq_resvalue_value v -> res_spec s v
| Coq_resvalue_ref r ->
  let_binding (fun tt ->
    match r.ref_base with
    | Coq_ref_base_type_value v ->
      if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_primitive_base
      then if_value (prim_value_get runs0 s c v r.ref_name) res_spec
      else (match v with
            | Coq_value_prim p ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s
                ("[ref_get_value] received a primitive value whose kind is not primitive.")
            | Coq_value_object l ->
              if_value (run_object_get runs0 s c l r.ref_name) res_spec)
    | Coq_ref_base_type_env_loc l ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[ref_get_value] received a reference to a value whose base type is an environnment record."))
    (fun for_base_or_object ->
    match ref_kind_of r with
    | Coq_ref_kind_null ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[ref_get_value] received a reference whose base is [null].")
    | Coq_ref_kind_undef -> throw_result (run_error s Coq_native_error_ref)
    | Coq_ref_kind_primitive_base -> for_base_or_object ()
    | Coq_ref_kind_object -> for_base_or_object ()
    | Coq_ref_kind_env_record ->
      (match r.ref_base with
       | Coq_ref_base_type_value v ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("[ref_get_value] received a reference to an environnment record whose base type is a value.")
       | Coq_ref_base_type_env_loc l ->
         if_value
           (env_record_get_binding_value runs0 s c l r.ref_name r.ref_strict)
           res_spec))

let ref_get_value runs s c r =
   let res = ref_get_value runs s c r in match res with
   | JsInterpreterMonads.Coq_result_some crs ->
     begin match crs with
       | (Coq_specret_val (_,rs)) ->
         begin match rs with
           | Coq_value_prim cvp ->
             begin match cvp with
               | Coq_prim_undef ->
           prerr_string ("Warning: ref_get_value returns the undefined value on "); prerr_string (Prheap.prresvalue r); prerr_newline(); res
               | _ -> res
             end
         | _ -> res
         end
       | _ -> res
     end
     | _ -> res
 
 (** val run_expr_get_value :
    runs_type -> state -> execution_ctx -> expr -> value specres **)

let run_expr_get_value runs0 s c e =
  if_success (runs0.runs_type_expr s c e) (fun s0 rv ->
    ref_get_value runs0 s0 c rv)

(** val object_put_complete :
    runs_type -> builtin_put -> state -> execution_ctx -> value -> object_loc
    -> prop_name -> value -> strictness_flag -> result_void **)

let object_put_complete runs0 b s c vthis l x v str =
  match b with Coq_builtin_put_default ->
  if_bool (object_can_put runs0 s c l x) (fun s1 b0 ->
    if b0
    then if_spec (runs0.runs_type_object_get_own_prop s1 c l x) (fun s2 d ->
           let_binding (fun x0 ->
             if_spec (run_object_get_prop runs0 s2 c l x) (fun s3 d' ->
               let_binding (fun x1 ->
                 match vthis with
                 | Coq_value_prim wthis ->
                   out_error_or_void s3 str Coq_native_error_type
                 | Coq_value_object lthis ->
                   let_binding (descriptor_intro_data v true true true)
                     (fun desc ->
                     if_success
                       (object_define_own_prop runs0 s3 c l x desc str)
                       (fun s4 rv -> res_void s4))) (fun follow' ->
                 match d' with
                 | Coq_full_descriptor_undef -> follow' ()
                 | Coq_full_descriptor_some a ->
                   (match a with
                    | Coq_attributes_data_of a0 -> follow' ()
                    | Coq_attributes_accessor_of aa' ->
                      (match aa'.attributes_accessor_set with
                       | Coq_value_prim p ->
                         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                           s3
                           ("[object_put_complete] found a primitive in an `set\' accessor.")
                       | Coq_value_object lfsetter ->
                         if_success
                           (runs0.runs_type_call s3 c lfsetter vthis
                             (v :: [])) (fun s4 rv -> res_void s4))))))
             (fun follow ->
             match d with
             | Coq_full_descriptor_undef -> follow ()
             | Coq_full_descriptor_some a ->
               (match a with
                | Coq_attributes_data_of ad ->
                  (match vthis with
                   | Coq_value_prim wthis ->
                     out_error_or_void s2 str Coq_native_error_type
                   | Coq_value_object lthis ->
                     let_binding { descriptor_value = (Some v);
                       descriptor_writable = None; descriptor_get = None;
                       descriptor_set = None; descriptor_enumerable = None;
                       descriptor_configurable = None } (fun desc ->
                       if_success
                         (object_define_own_prop runs0 s2 c l x desc str)
                         (fun s3 rv -> res_void s3)))
                | Coq_attributes_accessor_of a0 -> follow ())))
    else out_error_or_void s1 str Coq_native_error_type)

(** val object_put :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name -> value
    -> strictness_flag -> result_void **)

let object_put runs0 s c l x v str =
  if_some (run_object_method object_put_ s l) (fun b ->
    object_put_complete runs0 b s c (Coq_value_object l) l x v str)

(** val env_record_set_mutable_binding :
    runs_type -> state -> execution_ctx -> env_loc -> prop_name -> value ->
    strictness_flag -> result_void **)

let env_record_set_mutable_binding runs0 s c l x v str =
  if_some (env_record_binds_pickable_option s l) (fun e ->
    match e with
    | Coq_env_record_decl ed ->
      if_some (Heap.read_option string_comparable ed x) (fun rm ->
        let (mu, v_old) = rm in
        if not_decidable (mutability_comparable mu Coq_mutability_immutable)
        then res_void (env_record_write_decl_env s l x mu v)
        else out_error_or_void s str Coq_native_error_type)
    | Coq_env_record_object (l0, pt) -> object_put runs0 s c l0 x v str)

(** val prim_value_put :
    runs_type -> state -> execution_ctx -> prim -> prop_name -> value ->
    strictness_flag -> result_void **)

let prim_value_put runs0 s c w x v str =
  if_object (to_object s (Coq_value_prim w)) (fun s1 l ->
    object_put_complete runs0 Coq_builtin_put_default s1 c (Coq_value_prim w)
      l x v str)

(** val ref_put_value :
    runs_type -> state -> execution_ctx -> resvalue -> value -> result_void **)

let ref_put_value runs0 s c rv v =
  match rv with
  | Coq_resvalue_empty ->
    (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
      s
      ("[ref_put_value] received an empty result.")
  | Coq_resvalue_value v0 -> run_error s Coq_native_error_ref
  | Coq_resvalue_ref r ->
    if ref_kind_comparable (ref_kind_of r) Coq_ref_kind_undef
    then if r.ref_strict
         then run_error s Coq_native_error_ref
         else object_put runs0 s c (Coq_object_loc_prealloc
                Coq_prealloc_global) r.ref_name v throw_false
    else if or_decidable
              (ref_kind_comparable (ref_kind_of r)
                Coq_ref_kind_primitive_base)
              (or_decidable
                (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_null)
                (ref_kind_comparable (ref_kind_of r) Coq_ref_kind_object))
         then (match r.ref_base with
               | Coq_ref_base_type_value v' ->
                 if ref_kind_comparable (ref_kind_of r)
                      Coq_ref_kind_primitive_base
                 then (match v' with
                       | Coq_value_prim w ->
                         prim_value_put runs0 s c w r.ref_name v r.ref_strict
                       | Coq_value_object o ->
                         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                           s
                           ("[ref_put_value] impossible case"))
                 else (match v' with
                       | Coq_value_prim p ->
                         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                           s
                           ("[ref_put_value] impossible case")
                       | Coq_value_object l ->
                         object_put runs0 s c l r.ref_name v r.ref_strict)
               | Coq_ref_base_type_env_loc l ->
                 (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                   s
                   ("[ref_put_value] contradicts ref_is_property"))
         else (match r.ref_base with
               | Coq_ref_base_type_value v0 ->
                 (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                   s
                   ("[ref_put_value] impossible spec")
               | Coq_ref_base_type_env_loc l ->
                 env_record_set_mutable_binding runs0 s c l r.ref_name v
                   r.ref_strict)

(** val env_record_create_mutable_binding :
    runs_type -> state -> execution_ctx -> env_loc -> prop_name -> bool
    option -> result_void **)

let env_record_create_mutable_binding runs0 s c l x deletable_opt =
  let_binding (unsome_default false deletable_opt) (fun deletable ->
    if_some (env_record_binds_pickable_option s l) (fun e ->
      match e with
      | Coq_env_record_decl ed ->
        if Heap.indom_decidable string_comparable ed x
        then (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
               s
               ("Already declared environnment record in [env_record_create_mutable_binding].")
        else let_binding
               (env_record_write_decl_env s l x
                 (mutability_of_bool deletable) (Coq_value_prim
                 Coq_prim_undef)) (fun s' -> res_void s')
      | Coq_env_record_object (l0, pt) ->
        if_bool (object_has_prop runs0 s c l0 x) (fun s1 has ->
          if has
          then (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                 s1
                 ("Already declared binding in [env_record_create_mutable_binding].")
          else let_binding { attributes_data_value = (Coq_value_prim
                 Coq_prim_undef); attributes_data_writable = true;
                 attributes_data_enumerable = true;
                 attributes_data_configurable = deletable } (fun a ->
                 if_success
                   (object_define_own_prop runs0 s1 c l0 x
                     (descriptor_of_attributes (Coq_attributes_data_of a))
                     throw_true) (fun s2 rv -> res_void s2)))))

(** val env_record_create_set_mutable_binding :
    runs_type -> state -> execution_ctx -> env_loc -> prop_name -> bool
    option -> value -> strictness_flag -> result_void **)

let env_record_create_set_mutable_binding runs0 s c l x deletable_opt v str =
  if_void (env_record_create_mutable_binding runs0 s c l x deletable_opt)
    (fun s0 -> env_record_set_mutable_binding runs0 s0 c l x v str)

(** val env_record_create_immutable_binding :
    state -> env_loc -> prop_name -> result_void **)

let env_record_create_immutable_binding s l x =
  if_some (env_record_binds_pickable_option s l) (fun e ->
    match e with
    | Coq_env_record_decl ed ->
      if Heap.indom_decidable string_comparable ed x
      then (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
             s
             ("Already declared environnment record in [env_record_create_immutable_binding].")
      else res_void
             (env_record_write_decl_env s l x
               Coq_mutability_uninitialized_immutable (Coq_value_prim
               Coq_prim_undef))
    | Coq_env_record_object (o, p) ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[env_record_create_immutable_binding] received an environnment record object."))

(** val env_record_initialize_immutable_binding :
    state -> env_loc -> prop_name -> value -> result_void **)

let env_record_initialize_immutable_binding s l x v =
  if_some (env_record_binds_pickable_option s l) (fun e ->
    match e with
    | Coq_env_record_decl ed ->
      if_some (decl_env_record_pickable_option ed x) (fun evs ->
        if prod_comparable mutability_comparable value_comparable evs
             (Coq_mutability_uninitialized_immutable, (Coq_value_prim
             Coq_prim_undef))
        then let_binding
               (env_record_write_decl_env s l x Coq_mutability_immutable v)
               (fun s' -> res_void s')
        else (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
               s
               ("Non suitable binding in [env_record_initialize_immutable_binding]."))
    | Coq_env_record_object (o, p) ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s
        ("[env_record_initialize_immutable_binding] received an environnment record object."))

(** val call_object_new : state -> value -> result **)

let call_object_new s v =
  match type_of v with
  | Coq_type_undef ->
    result_out
      (let_binding
        (object_new (Coq_value_object (Coq_object_loc_prealloc
          Coq_prealloc_object_proto))
          ("Object")) (fun o ->
        let_binding (object_alloc s o) (fun p ->
          let (l, s') = p in Coq_out_ter (s', (res_val (Coq_value_object l))))))
  | Coq_type_null ->
    result_out
      (let_binding
        (object_new (Coq_value_object (Coq_object_loc_prealloc
          Coq_prealloc_object_proto))
          ("Object")) (fun o ->
        let_binding (object_alloc s o) (fun p ->
          let (l, s') = p in Coq_out_ter (s', (res_val (Coq_value_object l))))))
  | Coq_type_bool -> to_object s v
  | Coq_type_number -> to_object s v
  | Coq_type_string -> to_object s v
  | Coq_type_object -> result_out (Coq_out_ter (s, (res_val v)))

(** val array_args_map_loop :
    runs_type -> state -> execution_ctx -> object_loc -> value list -> float
    -> result_void **)

let rec array_args_map_loop runs0 s c l args ind =
  match args with
  | [] -> res_void s
  | h :: rest ->
    if_some
      (object_heap_map_properties_pickable_option s l (fun p ->
        Heap.write p (JsNumber.to_string (of_int ind))
          (Coq_attributes_data_of (attributes_data_intro_all_true h))))
      (fun s' -> array_args_map_loop runs0 s' c l rest (ind +. 1.))

(** val string_of_prealloc : prealloc -> string **)

let string_of_prealloc _foo_ = match _foo_ with
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
    runs_type -> state -> execution_ctx -> prealloc -> value list -> result **)

let run_construct_prealloc runs0 s c b args =
  match b with
  | Coq_prealloc_global ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_eval ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_parse_int ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_parse_float ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_is_finite ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_is_nan ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_decode_uri ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_decode_uri_component ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_encode_uri ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_global_encode_uri_component ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object ->
    let_binding (get_arg 0 args) (fun v -> call_object_new s v)
  | Coq_prealloc_object_get_proto_of ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_get_own_prop_descriptor ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_get_own_prop_name ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_create ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_define_prop ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_define_props ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_seal ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_freeze ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_prevent_extensions ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_is_sealed ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_is_frozen ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_is_extensible ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_keys ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_keys_call ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_proto_value_of ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_proto_has_own_prop ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_proto_is_prototype_of ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_object_proto_prop_is_enumerable ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_function ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_function_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_function_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_function_proto_apply ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_function_proto_call ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_function_proto_bind ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_bool ->
    result_out
      (let_binding (get_arg 0 args) (fun v ->
        let_binding (convert_value_to_boolean v) (fun b0 ->
          let_binding
            (object_new (Coq_value_object (Coq_object_loc_prealloc
              Coq_prealloc_bool_proto))
              ("Boolean")) (fun o1 ->
            let_binding
              (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_bool
                b0))) (fun o ->
              let_binding (object_alloc s o) (fun p ->
                let (l, s') = p in
                Coq_out_ter (s', (res_val (Coq_value_object l)))))))))
  | Coq_prealloc_bool_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_bool_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_bool_proto_value_of ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_number ->
    let_binding (fun s' v ->
      let_binding
        (object_new (Coq_value_object (Coq_object_loc_prealloc
          Coq_prealloc_number_proto))
          ("Number")) (fun o1 ->
        let_binding (object_with_primitive_value o1 v) (fun o ->
          let (l, s1) = object_alloc s' o in
          result_out (Coq_out_ter (s1, (res_val (Coq_value_object l)))))))
      (fun follow ->
      if list_eq_nil_decidable args
      then follow s (Coq_value_prim (Coq_prim_number JsNumber.zero))
      else let_binding (get_arg 0 args) (fun v ->
             if_number (to_number runs0 s c v) (fun x x0 ->
               follow x (Coq_value_prim (Coq_prim_number x0)))))
  | Coq_prealloc_number_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_number_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_number_proto_value_of ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_number_proto_to_fixed ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_number_proto_to_exponential ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_number_proto_to_precision ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_array ->
    let_binding
      (object_new (Coq_value_object (Coq_object_loc_prealloc
        Coq_prealloc_array_proto)) ("Array"))
      (fun o' ->
      let_binding (object_for_array o' Coq_builtin_define_own_prop_array)
        (fun o ->
        let_binding (object_alloc s o) (fun p ->
          let (l, s') = p in
          let_binding (fun s'' length0 ->
            if_some
              (object_heap_map_properties_pickable_option s'' l (fun p0 ->
                Heap.write p0 ("length")
                  (Coq_attributes_data_of { attributes_data_value =
                  (Coq_value_prim (Coq_prim_number (of_int length0)));
                  attributes_data_writable = true;
                  attributes_data_enumerable = false;
                  attributes_data_configurable = false }))) (fun s0 ->
              res_ter s0 (res_val (Coq_value_object l)))) (fun follow ->
            let_binding (LibList.length args) (fun arg_len ->
              if nat_eq arg_len 1
              then let_binding (get_arg 0 args) (fun v ->
                     match v with
                     | Coq_value_prim p0 ->
                       (match p0 with
                        | Coq_prim_undef ->
                          if_some
                            (object_heap_map_properties_pickable_option s' l
                              (fun p1 ->
                              Heap.write p1 ("0") (Coq_attributes_data_of
                                (attributes_data_intro_all_true v))))
                            (fun s0 ->
                            follow s0 1.0)
                        | Coq_prim_null ->
                          if_some
                            (object_heap_map_properties_pickable_option s' l
                              (fun p1 ->
                              Heap.write p1 ("0") (Coq_attributes_data_of
                                (attributes_data_intro_all_true v))))
                            (fun s0 ->
                            follow s0 1.0)
                        | Coq_prim_bool b0 ->
                          if_some
                            (object_heap_map_properties_pickable_option s' l
                              (fun p1 ->
                              Heap.write p1 ("0") (Coq_attributes_data_of
                                (attributes_data_intro_all_true v))))
                            (fun s0 ->
                            follow s0 1.0)
                        | Coq_prim_number vlen ->
                          if_spec
                            (to_uint32 runs0 s' c (Coq_value_prim
                              (Coq_prim_number vlen))) (fun s0 ilen ->
                            if number_comparable (of_int ilen) vlen
                            then follow s0 ilen
                            else run_error s0 Coq_native_error_range)
                        | Coq_prim_string s0 ->
                          if_some
                            (object_heap_map_properties_pickable_option s' l
                              (fun p1 ->
                              Heap.write p1 ("0") (Coq_attributes_data_of
                                (attributes_data_intro_all_true v))))
                            (fun s1 ->
                            follow s1 1.0))
                     | Coq_value_object o0 ->
                       if_some
                         (object_heap_map_properties_pickable_option s' l
                           (fun p0 ->
                           Heap.write p0 ("0") (Coq_attributes_data_of
                             (attributes_data_intro_all_true v)))) (fun s0 ->
                         follow s0 1.0))
              else if_some
                     (object_heap_map_properties_pickable_option s' l
                       (fun p0 ->
                       Heap.write p0
                         ("length")
                         (Coq_attributes_data_of { attributes_data_value =
                         (Coq_value_prim (Coq_prim_number
                         (number_of_int arg_len)));
                         attributes_data_writable = true;
                         attributes_data_enumerable = false;
                         attributes_data_configurable = false }))) (fun s0 ->
                     if_void (array_args_map_loop runs0 s0 c l args 0.)
                       (fun s1 -> res_ter s1 (res_val (Coq_value_object l)))))))))
  | Coq_prealloc_array_is_array ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_array_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_array_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_array_proto_join ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_array_proto_pop ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_array_proto_push ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_string ->
    let_binding
      (object_new (Coq_value_object (Coq_object_loc_prealloc
        Coq_prealloc_string_proto))
        ("String")) (fun o2 ->
      let_binding
        (object_with_get_own_property o2 Coq_builtin_get_own_prop_string)
        (fun o1 ->
        let_binding (fun s0 s1 ->
          let_binding
            (object_with_primitive_value o1 (Coq_value_prim (Coq_prim_string
              s1))) (fun o ->
            let (l, s2) = object_alloc s0 o in
            let_binding
              (attributes_data_intro_constant (Coq_value_prim
                (Coq_prim_number (number_of_int (strlength s1)))))
              (fun lenDesc ->
              if_some
                (object_heap_map_properties_pickable_option s2 l (fun p ->
                  Heap.write p ("length")
                    (Coq_attributes_data_of lenDesc))) (fun s' ->
                res_ter s' (res_val (Coq_value_object l)))))) (fun follow ->
          let_binding (LibList.length args) (fun arg_len ->
            if nat_eq arg_len 0
            then follow s ""
            else let_binding (get_arg 0 args) (fun arg ->
                   if_string (to_string runs0 s c arg) (fun s0 s1 ->
                     follow s0 s1))))))
  | Coq_prealloc_string_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_string_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_string_proto_value_of ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_string_proto_char_at ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_string_proto_char_code_at ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_math ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_mathop m ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_date ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_regexp ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_error ->
    let_binding (get_arg 0 args) (fun v ->
      build_error s (Coq_value_object (Coq_object_loc_prealloc
        Coq_prealloc_error_proto)) v)
  | Coq_prealloc_error_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_native_error ne ->
    let_binding (get_arg 0 args) (fun v ->
      build_error s (Coq_value_object (Coq_object_loc_prealloc
        (Coq_prealloc_native_error_proto ne))) v)
  | Coq_prealloc_native_error_proto n ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_error_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_throw_type_error ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))
  | Coq_prealloc_json ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Construct prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented.")))

(** val run_construct_default :
    runs_type -> state -> execution_ctx -> object_loc -> value list -> __
    specres **)

let run_construct_default runs0 s c l args =
  if_value
    (run_object_get runs0 s c l
      ("prototype"))
    (fun s1 v1 ->
    let_binding
      (if type_comparable (type_of v1) Coq_type_object
       then v1
       else Coq_value_object (Coq_object_loc_prealloc
              Coq_prealloc_object_proto)) (fun vproto ->
      let_binding
        (object_new vproto ("Object"))
        (fun o ->
        let_binding (object_alloc s1 o) (fun p ->
          let (l', s2) = p in
          if_value (runs0.runs_type_call s2 c l (Coq_value_object l') args)
            (fun s3 v2 ->
            let_binding
              (if type_comparable (type_of v2) Coq_type_object
               then v2
               else Coq_value_object l') (fun vr -> res_ter s3 (res_val vr)))))))

(** val run_construct :
    runs_type -> state -> execution_ctx -> construct -> object_loc -> value
    list -> result **)

let run_construct runs0 s c co l args =
  match co with
  | Coq_construct_default -> run_construct_default runs0 s c l args
  | Coq_construct_after_bind ->
    if_some (run_object_method object_target_function_ s l) (fun otrg ->
      if_some otrg (fun target ->
        if_some (run_object_method object_construct_ s target) (fun oco ->
          match oco with
          | Some co0 ->
            if_some (run_object_method object_bound_args_ s l) (fun oarg ->
              if_some oarg (fun boundArgs ->
                let_binding (LibList.append boundArgs args) (fun arguments ->
                  runs0.runs_type_construct s c co0 target arguments)))
          | None -> run_error s Coq_native_error_type)))
  | Coq_construct_prealloc b -> run_construct_prealloc runs0 s c b args

(** val run_call_default :
    runs_type -> state -> execution_ctx -> object_loc -> result **)

let run_call_default runs0 s c lf =
  let_binding
    (result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_undef)))))
    (fun def ->
    if_some (run_object_method object_code_ s lf) (fun oC ->
      match oC with
      | Some bd ->
        if list_eq_nil_decidable (prog_elements (funcbody_prog bd))
        then def
        else if_success_or_return
               (runs0.runs_type_prog s c (funcbody_prog bd)) (fun s' ->
               result_out (Coq_out_ter (s',
                 (res_val (Coq_value_prim Coq_prim_undef))))) (fun s' rv ->
               result_out (Coq_out_ter (s', (res_normal rv))))
      | None -> def))

(** val creating_function_object_proto :
    runs_type -> state -> execution_ctx -> object_loc -> result **)

let creating_function_object_proto runs0 s c l =
  if_object (run_construct_prealloc runs0 s c Coq_prealloc_object [])
    (fun s1 lproto ->
    let_binding { attributes_data_value = (Coq_value_object l);
      attributes_data_writable = true; attributes_data_enumerable = false;
      attributes_data_configurable = true } (fun a1 ->
      if_bool
        (object_define_own_prop runs0 s1 c lproto
          ("constructor")
          (descriptor_of_attributes (Coq_attributes_data_of a1)) false)
        (fun s2 b ->
        let_binding { attributes_data_value = (Coq_value_object lproto);
          attributes_data_writable = true; attributes_data_enumerable =
          false; attributes_data_configurable = false } (fun a2 ->
          object_define_own_prop runs0 s2 c l
            ("prototype")
            (descriptor_of_attributes (Coq_attributes_data_of a2)) false))))

(** val creating_function_object :
    runs_type -> state -> execution_ctx -> string list -> funcbody ->
    lexical_env -> strictness_flag -> result **)

let creating_function_object runs0 s c names bd x str =
  let_binding
    (object_new (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_function_proto))
      ("Function")) (fun o ->
    let_binding (object_with_get o Coq_builtin_get_function) (fun o1 ->
      let_binding
        (object_with_invokation o1 (Some Coq_construct_default) (Some
          Coq_call_default) (Some Coq_builtin_has_instance_function))
        (fun o2 ->
        let_binding
          (object_with_details o2 (Some x) (Some names) (Some bd) None None
            None None) (fun o3 ->
          let_binding (object_alloc s o3) (fun p ->
            let (l, s1) = p in
            let_binding { attributes_data_value = (Coq_value_prim
              (Coq_prim_number
              (number_of_int (LibList.length names))));
              attributes_data_writable = false; attributes_data_enumerable =
              false; attributes_data_configurable = false } (fun a1 ->
              if_bool
                (object_define_own_prop runs0 s1 c l
                  ("length")
                  (descriptor_of_attributes (Coq_attributes_data_of a1))
                  false) (fun s2 b2 ->
                if_bool (creating_function_object_proto runs0 s2 c l)
                  (fun s3 b3 ->
                  if negb str
                  then res_ter s3 (res_val (Coq_value_object l))
                  else let_binding (Coq_value_object (Coq_object_loc_prealloc
                         Coq_prealloc_throw_type_error)) (fun vthrower ->
                         let_binding { attributes_accessor_get = vthrower;
                           attributes_accessor_set = vthrower;
                           attributes_accessor_enumerable = false;
                           attributes_accessor_configurable = false }
                           (fun a2 ->
                           if_bool
                             (object_define_own_prop runs0 s3 c l
                               ("caller")
                               (descriptor_of_attributes
                                 (Coq_attributes_accessor_of a2)) false)
                             (fun s4 b4 ->
                             if_bool
                               (object_define_own_prop runs0 s4 c l
                                 ("arguments")
                                 (descriptor_of_attributes
                                   (Coq_attributes_accessor_of a2)) false)
                               (fun s5 b5 ->
                               res_ter s5 (res_val (Coq_value_object l))))))))))))))

(** val binding_inst_formal_params :
    runs_type -> state -> execution_ctx -> env_loc -> value list -> string
    list -> strictness_flag -> result_void **)

let rec binding_inst_formal_params runs0 s c l args names str =
  match names with
  | [] -> res_void s
  | argname :: names' ->
    let_binding (hd (Coq_value_prim Coq_prim_undef) args) (fun v ->
      let_binding (tl args) (fun args' ->
        if_bool (env_record_has_binding runs0 s c l argname) (fun s1 hb ->
          let_binding (fun s' ->
            if_void
              (env_record_set_mutable_binding runs0 s' c l argname v str)
              (fun s'' ->
              binding_inst_formal_params runs0 s'' c l args' names' str))
            (fun follow ->
            if hb
            then follow s1
            else if_void
                   (env_record_create_mutable_binding runs0 s1 c l argname
                     None) follow))))

(** val binding_inst_function_decls :
    runs_type -> state -> execution_ctx -> env_loc -> funcdecl list ->
    strictness_flag -> bool -> result_void **)

let rec binding_inst_function_decls runs0 s c l fds str bconfig =
  match fds with
  | [] -> res_void s
  | fd :: fds' ->
    let_binding fd.funcdecl_body (fun fbd ->
      let_binding (funcbody_is_strict fbd) (fun str_fd ->
        let_binding fd.funcdecl_parameters (fun fparams ->
          let_binding fd.funcdecl_name (fun fname ->
            if_object
              (creating_function_object runs0 s c fparams fbd
                c.execution_ctx_variable_env str_fd) (fun s1 fo ->
              let_binding (fun s2 ->
                if_void
                  (env_record_set_mutable_binding runs0 s2 c l fname
                    (Coq_value_object fo) str) (fun s3 ->
                  binding_inst_function_decls runs0 s3 c l fds' str bconfig))
                (fun follow ->
                if_bool (env_record_has_binding runs0 s1 c l fname)
                  (fun s2 has ->
                  if has
                  then if nat_eq l env_loc_global_env_record
                       then if_spec
                              (run_object_get_prop runs0 s2 c
                                (Coq_object_loc_prealloc Coq_prealloc_global)
                                fname) (fun s3 d ->
                              match d with
                              | Coq_full_descriptor_undef ->
                                (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                                  s3
                                  ("Undefined full descriptor in [binding_inst_function_decls].")
                              | Coq_full_descriptor_some a ->
                                if bool_decidable (attributes_configurable a)
                                then let_binding { attributes_data_value =
                                       (Coq_value_prim Coq_prim_undef);
                                       attributes_data_writable = true;
                                       attributes_data_enumerable = true;
                                       attributes_data_configurable =
                                       bconfig } (fun a' ->
                                       if_bool
                                         (object_define_own_prop runs0 s3 c
                                           (Coq_object_loc_prealloc
                                           Coq_prealloc_global) fname
                                           (descriptor_of_attributes
                                             (Coq_attributes_data_of a'))
                                           true) (fun s0 x -> follow s0))
                                else if or_decidable
                                          (descriptor_is_accessor_dec
                                            (descriptor_of_attributes a))
                                          (or_decidable
                                            (bool_comparable
                                              (attributes_writable a) false)
                                            (bool_comparable
                                              (attributes_enumerable a)
                                              false))
                                     then run_error s3 Coq_native_error_type
                                     else follow s3)
                       else follow s2
                  else if_void
                         (env_record_create_mutable_binding runs0 s2 c l
                           fname (Some bconfig)) follow)))))))

(** val make_arg_getter :
    runs_type -> state -> execution_ctx -> prop_name -> lexical_env -> result **)

let make_arg_getter runs0 s c x x0 =
  let xbd =
    append ("return ")
      (append x (";"))
  in
  let bd = Coq_funcbody_intro ((Coq_prog_intro (true, ((Coq_element_stat
    (Coq_stat_return (Some (Coq_expr_identifier x)))) :: []))), xbd)
  in
  creating_function_object runs0 s c [] bd x0 true

(** val make_arg_setter :
    runs_type -> state -> execution_ctx -> prop_name -> lexical_env -> result **)

let make_arg_setter runs0 s c x x0 =
  let xparam = append x ("_arg") in
  let xbd =
    append x (append (" = ") (append xparam ";"))
  in
  let bd = Coq_funcbody_intro ((Coq_prog_intro (true, ((Coq_element_stat
    (Coq_stat_expr (Coq_expr_assign ((Coq_expr_identifier x), None,
    (Coq_expr_identifier xparam))))) :: []))), xbd)
  in
  creating_function_object runs0 s c (xparam :: []) bd x0 true

(** val arguments_object_map_loop :
    runs_type -> state -> execution_ctx -> object_loc -> string list ->
    int -> value list -> lexical_env -> strictness_flag -> object_loc ->
    string list -> result_void **)

let rec arguments_object_map_loop runs0 s c l xs len args x str lmap xsmap =
  (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
    (fun _ ->
    if list_eq_nil_decidable xsmap
    then res_void s
    else if_some (object_binds_pickable_option s l) (fun o ->
           let_binding
             (object_for_args_object o lmap Coq_builtin_get_args_obj
               Coq_builtin_get_own_prop_args_obj
               Coq_builtin_define_own_prop_args_obj
               Coq_builtin_delete_args_obj) (fun o' ->
             res_void (object_write s l o'))))
    (fun len' ->
    let_binding (take_drop_last args) (fun tdl ->
      let (rmlargs, largs) = tdl in
      let_binding (fun s0 xsmap0 ->
        arguments_object_map_loop runs0 s0 c l xs len' rmlargs x str lmap
          xsmap0) (fun arguments_object_map_loop' ->
        let_binding (attributes_data_intro_all_true largs) (fun a ->
          if_bool
            (object_define_own_prop runs0 s c l
              (convert_prim_to_string (Coq_prim_number
                (number_of_int len')))
              (descriptor_of_attributes (Coq_attributes_data_of a)) false)
            (fun s1 b ->
            if ge_nat_decidable len' (LibList.length xs)
            then arguments_object_map_loop' s1 xsmap
            else let dummy = "" in
                 let_binding (nth_def dummy len' xs) (fun x0 ->
                   if or_decidable (bool_comparable str true)
                        (coq_Mem_decidable string_comparable x0 xsmap)
                   then arguments_object_map_loop' s1 xsmap
                   else if_object (make_arg_getter runs0 s1 c x0 x)
                          (fun s2 lgetter ->
                          if_object (make_arg_setter runs0 s2 c x0 x)
                            (fun s3 lsetter ->
                            let_binding { attributes_accessor_get =
                              (Coq_value_object lgetter);
                              attributes_accessor_set = (Coq_value_object
                              lsetter); attributes_accessor_enumerable =
                              false; attributes_accessor_configurable =
                              true } (fun a' ->
                              if_bool
                                (object_define_own_prop runs0 s3 c lmap
                                  (convert_prim_to_string (Coq_prim_number
                                    (number_of_int len')))
                                  (descriptor_of_attributes
                                    (Coq_attributes_accessor_of a')) false)
                                (fun s4 b' ->
                                arguments_object_map_loop' s4 (x0 :: xsmap)))))))))))
    len

(** val arguments_object_map :
    runs_type -> state -> execution_ctx -> object_loc -> string list ->
    value list -> lexical_env -> strictness_flag -> result_void **)

let arguments_object_map runs0 s c l xs args x str =
  if_object (run_construct_prealloc runs0 s c Coq_prealloc_object [])
    (fun s' lmap ->
    arguments_object_map_loop runs0 s' c l xs (LibList.length args) args x
      str lmap [])

(** val create_arguments_object :
    runs_type -> state -> execution_ctx -> object_loc -> string list ->
    value list -> lexical_env -> strictness_flag -> result **)

let create_arguments_object runs0 s c lf xs args x str =
  let_binding
    (object_create_builtin (Coq_value_object (Coq_object_loc_prealloc
      Coq_prealloc_object_proto))
      ("Arguments")
      Heap.empty) (fun o ->
    let_binding (object_alloc s o) (fun p ->
      let (l, s') = p in
      let_binding { attributes_data_value = (Coq_value_prim (Coq_prim_number
        (number_of_int (LibList.length args))));
        attributes_data_writable = true; attributes_data_enumerable = false;
        attributes_data_configurable = true } (fun a ->
        if_bool
          (object_define_own_prop runs0 s' c l
            ("length")
            (descriptor_of_attributes (Coq_attributes_data_of a)) false)
          (fun s1 b ->
          if_void (arguments_object_map runs0 s1 c l xs args x str)
            (fun s2 ->
            if str
            then let_binding (Coq_value_object (Coq_object_loc_prealloc
                   Coq_prealloc_throw_type_error)) (fun vthrower ->
                   let_binding { attributes_accessor_get = vthrower;
                     attributes_accessor_set = vthrower;
                     attributes_accessor_enumerable = false;
                     attributes_accessor_configurable = false } (fun a0 ->
                     if_bool
                       (object_define_own_prop runs0 s2 c l
                         ("caller")
                         (descriptor_of_attributes
                           (Coq_attributes_accessor_of a0)) false)
                       (fun s3 b' ->
                       if_bool
                         (object_define_own_prop runs0 s3 c l
                           ("callee")
                           (descriptor_of_attributes
                             (Coq_attributes_accessor_of a0)) false)
                         (fun s4 b'' ->
                         res_ter s4 (res_val (Coq_value_object l))))))
            else let_binding { attributes_data_value = (Coq_value_object lf);
                   attributes_data_writable = true;
                   attributes_data_enumerable = false;
                   attributes_data_configurable = true } (fun a0 ->
                   if_bool
                     (object_define_own_prop runs0 s2 c l
                       ("callee")
                       (descriptor_of_attributes (Coq_attributes_data_of a0))
                       false) (fun s3 b' ->
                     res_ter s3 (res_val (Coq_value_object l)))))))))

(** val binding_inst_arg_obj :
    runs_type -> state -> execution_ctx -> object_loc -> prog -> string
    list -> value list -> env_loc -> result_void **)

let binding_inst_arg_obj runs0 s c lf p xs args l =
  let arguments =
    "arguments"
  in
  let_binding (prog_intro_strictness p) (fun str ->
    if_object
      (create_arguments_object runs0 s c lf xs args
        c.execution_ctx_variable_env str) (fun s1 largs ->
      if str
      then if_void (env_record_create_immutable_binding s1 l arguments)
             (fun s2 ->
             env_record_initialize_immutable_binding s2 l arguments
               (Coq_value_object largs))
      else env_record_create_set_mutable_binding runs0 s1 c l arguments None
             (Coq_value_object largs) false))

(** val binding_inst_var_decls :
    runs_type -> state -> execution_ctx -> env_loc -> string list -> bool
    -> strictness_flag -> result_void **)

let rec binding_inst_var_decls runs0 s c l vds bconfig str =
  match vds with
  | [] -> res_void s
  | vd :: vds' ->
    let_binding (fun s0 ->
      binding_inst_var_decls runs0 s0 c l vds' bconfig str) (fun bivd ->
      if_bool (env_record_has_binding runs0 s c l vd) (fun s1 has ->
        if has
        then bivd s1
        else if_void
               (env_record_create_set_mutable_binding runs0 s1 c l vd (Some
                 bconfig) (Coq_value_prim Coq_prim_undef) str) bivd))

(** val execution_ctx_binding_inst :
    runs_type -> state -> execution_ctx -> codetype -> object_loc option ->
    prog -> value list -> result_void **)

let execution_ctx_binding_inst runs0 s c ct funco p args =
  match c.execution_ctx_variable_env with
  | [] ->
    (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
      s
      ("Empty [execution_ctx_variable_env] in [execution_ctx_binding_inst].")
  | l :: l0 ->
    let_binding (prog_intro_strictness p) (fun str ->
      let_binding (fun s' names ->
        let_binding (codetype_comparable ct Coq_codetype_eval)
          (fun bconfig ->
          let_binding (prog_funcdecl p) (fun fds ->
            if_void
              (binding_inst_function_decls runs0 s' c l fds str bconfig)
              (fun s1 ->
              if_bool
                (env_record_has_binding runs0 s1 c l
                  ("arguments"))
                (fun s2 bdefined ->
                let_binding (fun s'0 ->
                  let vds = prog_vardecl p in
                  binding_inst_var_decls runs0 s'0 c l vds bconfig str)
                  (fun follow2 ->
                  match ct with
                  | Coq_codetype_func ->
                    (match funco with
                     | Some func ->
                       if bdefined
                       then follow2 s2
                       else if_void
                              (binding_inst_arg_obj runs0 s2 c func p names
                                args l) follow2
                     | None ->
                       if bdefined
                       then follow2 s2
                       else (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                              s2
                              ("Weird `arguments\' object in [execution_ctx_binding_inst]."))
                  | Coq_codetype_global -> follow2 s2
                  | Coq_codetype_eval -> follow2 s2)))))) (fun follow ->
        match ct with
        | Coq_codetype_func ->
          (match funco with
           | Some func ->
             if_some (run_object_method object_formal_parameters_ s func)
               (fun nameso ->
               if_some nameso (fun names ->
                 if_void
                   (binding_inst_formal_params runs0 s c l args names str)
                   (fun s' -> follow s' names)))
           | None ->
             (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
               s
               ("Non coherent functionnal code type in [execution_ctx_binding_inst]."))
        | Coq_codetype_global ->
          (match funco with
           | Some o ->
             (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
               s
               ("Non coherent non-functionnal code type in [execution_ctx_binding_inst].")
           | None -> follow s [])
        | Coq_codetype_eval ->
          (match funco with
           | Some o ->
             (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
               s
               ("Non coherent non-functionnal code type in [execution_ctx_binding_inst].")
           | None -> follow s [])))

(** val entering_func_code :
    runs_type -> state -> execution_ctx -> object_loc -> value -> value list
    -> result **)

let entering_func_code runs0 s c lf vthis args =
  if_some (run_object_method object_code_ s lf) (fun bdo ->
    if_some bdo (fun bd ->
      let_binding (funcbody_is_strict bd) (fun str ->
        let_binding (fun s' vthis' ->
          if_some (run_object_method object_scope_ s' lf) (fun lexo ->
            if_some lexo (fun lex ->
              let_binding (lexical_env_alloc_decl s' lex) (fun p ->
                let (lex', s1) = p in
                let_binding (execution_ctx_intro_same lex' vthis' str)
                  (fun c' ->
                  if_void
                    (execution_ctx_binding_inst runs0 s1 c' Coq_codetype_func
                      (Some lf) (funcbody_prog bd) args) (fun s2 ->
                    run_call_default runs0 s2 c' lf)))))) (fun follow ->
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
                   | Coq_prim_bool b -> if_value (to_object s vthis) follow
                   | Coq_prim_number n -> if_value (to_object s vthis) follow
                   | Coq_prim_string s0 ->
                     if_value (to_object s vthis) follow)
                | Coq_value_object lthis -> follow s vthis)))))

(** val run_object_get_own_prop :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name ->
    full_descriptor specres **)

let run_object_get_own_prop runs0 s c l x =
  if_some (run_object_method object_get_own_prop_ s l) (fun b ->
    let_binding (fun s' ->
      if_some (run_object_method object_properties_ s' l) (fun p ->
        res_spec s'
          (if_some_or_default
            (convert_option_attributes
              (Heap.read_option string_comparable p x))
            Coq_full_descriptor_undef id))) (fun def ->
      match b with
      | Coq_builtin_get_own_prop_default -> def s
      | Coq_builtin_get_own_prop_args_obj ->
        if_spec (def s) (fun s1 d ->
          match d with
          | Coq_full_descriptor_undef ->
            res_spec s1 Coq_full_descriptor_undef
          | Coq_full_descriptor_some a ->
            if_some (run_object_method object_parameter_map_ s1 l)
              (fun lmapo ->
              if_some lmapo (fun lmap ->
                if_spec (runs0.runs_type_object_get_own_prop s1 c lmap x)
                  (fun s2 d0 ->
                  let_binding (fun s' a0 ->
                    res_spec s' (Coq_full_descriptor_some a0)) (fun follow ->
                    match d0 with
                    | Coq_full_descriptor_undef -> follow s2 a
                    | Coq_full_descriptor_some amap ->
                      if_value (run_object_get runs0 s2 c lmap x)
                        (fun s3 v ->
                        match a with
                        | Coq_attributes_data_of ad ->
                          follow s3 (Coq_attributes_data_of
                            (attributes_data_with_value ad v))
                        | Coq_attributes_accessor_of aa ->
                          (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                            s3
                            ("[run_object_get_own_prop]:  received an accessor property descriptor in a point where the specification suppose it never happens.")))))))
      | Coq_builtin_get_own_prop_string ->
        if_spec (def s) (fun s0 d ->
          match d with
          | Coq_full_descriptor_undef ->
            if_spec
              (to_int32 runs0 s0 c (Coq_value_prim (Coq_prim_string x)))
              (fun s1 k ->
              if_string
                (runs0.runs_type_to_string s1 c (Coq_value_prim
                  (Coq_prim_number (abs_float k))))
                (fun s2 s3 ->
                if not_decidable (string_comparable x s3)
                then res_spec s2 Coq_full_descriptor_undef
                else if_string (run_object_prim_value s2 l) (fun s4 str ->
                       if_spec
                         (to_int32 runs0 s4 c (Coq_value_prim
                           (Coq_prim_string x))) (fun s5 k0 ->
                         let_binding (number_of_int (strlength str)) (fun len ->
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
                                  (Coq_attributes_data_of a)))))))
          | Coq_full_descriptor_some a -> res_spec s0 d)))

(** val run_function_has_instance :
    runs_type -> state -> object_loc -> value -> result **)

let run_function_has_instance runs0 s lv _foo_ = match _foo_ with
| Coq_value_prim p -> run_error s Coq_native_error_type
| Coq_value_object lo ->
  if_some (run_object_method object_proto_ s lv) (fun vproto ->
    match vproto with
    | Coq_value_prim p ->
      (match p with
       | Coq_prim_undef ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("Primitive found in the prototype chain in [run_object_has_instance_loop].")
       | Coq_prim_null ->
         res_ter s (res_val (Coq_value_prim (Coq_prim_bool false)))
       | Coq_prim_bool b ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("Primitive found in the prototype chain in [run_object_has_instance_loop].")
       | Coq_prim_number n ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("Primitive found in the prototype chain in [run_object_has_instance_loop].")
       | Coq_prim_string s0 ->
         (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
           s
           ("Primitive found in the prototype chain in [run_object_has_instance_loop]."))
    | Coq_value_object proto ->
      if object_loc_comparable proto lo
      then res_ter s (res_val (Coq_value_prim (Coq_prim_bool true)))
      else runs0.runs_type_function_has_instance s proto (Coq_value_object
             lo))

(** val run_object_has_instance :
    runs_type -> state -> execution_ctx -> builtin_has_instance -> object_loc
    -> value -> result **)

let run_object_has_instance runs0 s c b l v =
  match b with
  | Coq_builtin_has_instance_function ->
    (match v with
     | Coq_value_prim w ->
       result_out (Coq_out_ter (s,
         (res_val (Coq_value_prim (Coq_prim_bool false)))))
     | Coq_value_object lv ->
       if_value
         (run_object_get runs0 s c l
           ("prototype"))
         (fun s1 vproto ->
         match vproto with
         | Coq_value_prim p -> run_error s1 Coq_native_error_type
         | Coq_value_object lproto ->
           runs0.runs_type_function_has_instance s1 lv (Coq_value_object
             lproto)))
  | Coq_builtin_has_instance_after_bind ->
    if_some (run_object_method object_target_function_ s l) (fun ol ->
      if_some ol (fun l0 ->
        if_some (run_object_method object_has_instance_ s l0) (fun ob ->
          match ob with
          | Some b0 -> runs0.runs_type_object_has_instance s c b0 l0 v
          | None -> run_error s Coq_native_error_type)))

(** val from_prop_descriptor :
    runs_type -> state -> execution_ctx -> full_descriptor -> result **)

let from_prop_descriptor runs0 s c _foo_ = match _foo_ with
| Coq_full_descriptor_undef ->
  result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_undef))))
| Coq_full_descriptor_some a ->
  if_object (run_construct_prealloc runs0 s c Coq_prealloc_object [])
    (fun s1 l ->
    let_binding (fun s0 x ->
      let_binding
        (attributes_data_intro_all_true (Coq_value_prim (Coq_prim_bool
          (attributes_enumerable a)))) (fun a1 ->
        if_bool
          (object_define_own_prop runs0 s0 c l
            ("enumerable")
            (descriptor_of_attributes (Coq_attributes_data_of a1))
            throw_false) (fun s0' x0 ->
          let_binding
            (attributes_data_intro_all_true (Coq_value_prim (Coq_prim_bool
              (attributes_configurable a)))) (fun a2 ->
            if_bool
              (object_define_own_prop runs0 s0' c l
                ("configurable")
                (descriptor_of_attributes (Coq_attributes_data_of a2))
                throw_false) (fun s' x1 ->
              res_ter s' (res_val (Coq_value_object l))))))) (fun follow ->
      match a with
      | Coq_attributes_data_of ad ->
        let_binding (attributes_data_intro_all_true ad.attributes_data_value)
          (fun a1 ->
          if_bool
            (object_define_own_prop runs0 s1 c l
              ("value")
              (descriptor_of_attributes (Coq_attributes_data_of a1))
              throw_false) (fun s2 x ->
            let_binding
              (attributes_data_intro_all_true (Coq_value_prim (Coq_prim_bool
                ad.attributes_data_writable))) (fun a2 ->
              if_bool
                (object_define_own_prop runs0 s2 c l
                  ("writable")
                  (descriptor_of_attributes (Coq_attributes_data_of a2))
                  throw_false) follow)))
      | Coq_attributes_accessor_of aa ->
        let_binding
          (attributes_data_intro_all_true aa.attributes_accessor_get)
          (fun a1 ->
          if_bool
            (object_define_own_prop runs0 s1 c l ("get")
              (descriptor_of_attributes (Coq_attributes_data_of a1))
              throw_false) (fun s2 x ->
            let_binding
              (attributes_data_intro_all_true aa.attributes_accessor_set)
              (fun a2 ->
              if_bool
                (object_define_own_prop runs0 s2 c l ("set")
                  (descriptor_of_attributes (Coq_attributes_data_of a2))
                  throw_false) follow)))))

(** val is_lazy_op : binary_op -> bool option **)

let is_lazy_op _foo_ = match _foo_ with
| Coq_binary_op_mult -> None
| Coq_binary_op_div -> None
| Coq_binary_op_mod -> None
| Coq_binary_op_add -> None
| Coq_binary_op_sub -> None
| Coq_binary_op_left_shift -> None
| Coq_binary_op_right_shift -> None
| Coq_binary_op_unsigned_right_shift -> None
| Coq_binary_op_lt -> None
| Coq_binary_op_gt -> None
| Coq_binary_op_le -> None
| Coq_binary_op_ge -> None
| Coq_binary_op_instanceof -> None
| Coq_binary_op_in -> None
| Coq_binary_op_equal -> None
| Coq_binary_op_disequal -> None
| Coq_binary_op_strict_equal -> None
| Coq_binary_op_strict_disequal -> None
| Coq_binary_op_bitwise_and -> None
| Coq_binary_op_bitwise_or -> None
| Coq_binary_op_bitwise_xor -> None
| Coq_binary_op_and -> Some false
| Coq_binary_op_or -> Some true
| Coq_binary_op_coma -> None

(** val get_puremath_op :
    binary_op -> (number -> number -> number) option **)

let get_puremath_op _foo_ = match _foo_ with
| Coq_binary_op_mult -> Some mult
| Coq_binary_op_div -> Some div
| Coq_binary_op_mod -> Some fmod
| Coq_binary_op_add -> None
| Coq_binary_op_sub -> Some sub
| Coq_binary_op_left_shift -> None
| Coq_binary_op_right_shift -> None
| Coq_binary_op_unsigned_right_shift -> None
| Coq_binary_op_lt -> None
| Coq_binary_op_gt -> None
| Coq_binary_op_le -> None
| Coq_binary_op_ge -> None
| Coq_binary_op_instanceof -> None
| Coq_binary_op_in -> None
| Coq_binary_op_equal -> None
| Coq_binary_op_disequal -> None
| Coq_binary_op_strict_equal -> None
| Coq_binary_op_strict_disequal -> None
| Coq_binary_op_bitwise_and -> None
| Coq_binary_op_bitwise_or -> None
| Coq_binary_op_bitwise_xor -> None
| Coq_binary_op_and -> None
| Coq_binary_op_or -> None
| Coq_binary_op_coma -> None

(** val get_inequality_op : binary_op -> (bool * bool) option **)

let get_inequality_op _foo_ = match _foo_ with
| Coq_binary_op_mult -> None
| Coq_binary_op_div -> None
| Coq_binary_op_mod -> None
| Coq_binary_op_add -> None
| Coq_binary_op_sub -> None
| Coq_binary_op_left_shift -> None
| Coq_binary_op_right_shift -> None
| Coq_binary_op_unsigned_right_shift -> None
| Coq_binary_op_lt -> Some (false, false)
| Coq_binary_op_gt -> Some (true, false)
| Coq_binary_op_le -> Some (true, true)
| Coq_binary_op_ge -> Some (false, true)
| Coq_binary_op_instanceof -> None
| Coq_binary_op_in -> None
| Coq_binary_op_equal -> None
| Coq_binary_op_disequal -> None
| Coq_binary_op_strict_equal -> None
| Coq_binary_op_strict_disequal -> None
| Coq_binary_op_bitwise_and -> None
| Coq_binary_op_bitwise_or -> None
| Coq_binary_op_bitwise_xor -> None
| Coq_binary_op_and -> None
| Coq_binary_op_or -> None
| Coq_binary_op_coma -> None

(** val get_shift_op :
    binary_op -> (bool * (float -> float -> float)) option **)

let get_shift_op _foo_ = match _foo_ with
| Coq_binary_op_mult -> None
| Coq_binary_op_div -> None
| Coq_binary_op_mod -> None
| Coq_binary_op_add -> None
| Coq_binary_op_sub -> None
| Coq_binary_op_left_shift -> Some (false, JsNumber.int32_left_shift)
| Coq_binary_op_right_shift -> Some (false, JsNumber.int32_right_shift)
| Coq_binary_op_unsigned_right_shift -> Some (true, JsNumber.uint32_right_shift)
| Coq_binary_op_lt -> None
| Coq_binary_op_gt -> None
| Coq_binary_op_le -> None
| Coq_binary_op_ge -> None
| Coq_binary_op_instanceof -> None
| Coq_binary_op_in -> None
| Coq_binary_op_equal -> None
| Coq_binary_op_disequal -> None
| Coq_binary_op_strict_equal -> None
| Coq_binary_op_strict_disequal -> None
| Coq_binary_op_bitwise_and -> None
| Coq_binary_op_bitwise_or -> None
| Coq_binary_op_bitwise_xor -> None
| Coq_binary_op_and -> None
| Coq_binary_op_or -> None
| Coq_binary_op_coma -> None

(** val get_bitwise_op : binary_op -> (float -> float -> float) option **)

let get_bitwise_op _foo_ = match _foo_ with
| Coq_binary_op_mult -> None
| Coq_binary_op_div -> None
| Coq_binary_op_mod -> None
| Coq_binary_op_add -> None
| Coq_binary_op_sub -> None
| Coq_binary_op_left_shift -> None
| Coq_binary_op_right_shift -> None
| Coq_binary_op_unsigned_right_shift -> None
| Coq_binary_op_lt -> None
| Coq_binary_op_gt -> None
| Coq_binary_op_le -> None
| Coq_binary_op_ge -> None
| Coq_binary_op_instanceof -> None
| Coq_binary_op_in -> None
| Coq_binary_op_equal -> None
| Coq_binary_op_disequal -> None
| Coq_binary_op_strict_equal -> None
| Coq_binary_op_strict_disequal -> None
| Coq_binary_op_bitwise_and -> Some JsNumber.int32_bitwise_and
| Coq_binary_op_bitwise_or -> Some JsNumber.int32_bitwise_or
| Coq_binary_op_bitwise_xor -> Some JsNumber.int32_bitwise_xor
| Coq_binary_op_and -> None
| Coq_binary_op_or -> None
| Coq_binary_op_coma -> None

(** val run_equal :
    runs_type -> state -> execution_ctx -> value -> value -> result **)

let run_equal runs0 s c v1 v2 =
  let conv_number = fun s0 v -> to_number runs0 s0 c v in
  let conv_primitive = fun s0 v -> to_primitive runs0 s0 c v None in
  let_binding (fun s0 v3 v4 k ->
    let ty1 = type_of v3 in
    let ty2 = type_of v4 in
    if type_comparable ty1 ty2
    then result_out (Coq_out_ter (s0,
           (res_val (Coq_value_prim (Coq_prim_bool
             (equality_test_for_same_type ty1 v3 v4))))))
    else k ty1 ty2) (fun checkTypesThen ->
    checkTypesThen s v1 v2 (fun ty1 ty2 ->
      let_binding (fun v3 f v4 ->
        if_value (f s v4) (fun s0 v2' -> runs0.runs_type_equal s0 c v3 v2'))
        (fun dc_conv ->
        let so = fun b ->
          result_out (Coq_out_ter (s,
            (res_val (Coq_value_prim (Coq_prim_bool b)))))
        in
        if and_decidable (type_comparable ty1 Coq_type_null)
             (type_comparable ty2 Coq_type_undef)
        then so true
        else if and_decidable (type_comparable ty1 Coq_type_undef)
                  (type_comparable ty2 Coq_type_null)
             then so true
             else if and_decidable (type_comparable ty1 Coq_type_number)
                       (type_comparable ty2 Coq_type_string)
                  then dc_conv v1 conv_number v2
                  else if and_decidable (type_comparable ty1 Coq_type_string)
                            (type_comparable ty2 Coq_type_number)
                       then dc_conv v2 conv_number v1
                       else if type_comparable ty1 Coq_type_bool
                            then dc_conv v2 conv_number v1
                            else if type_comparable ty2 Coq_type_bool
                                 then dc_conv v1 conv_number v2
                                 else if and_decidable
                                           (or_decidable
                                             (type_comparable ty1
                                               Coq_type_string)
                                             (type_comparable ty1
                                               Coq_type_number))
                                           (type_comparable ty2
                                             Coq_type_object)
                                      then dc_conv v1 conv_primitive v2
                                      else if and_decidable
                                                (type_comparable ty1
                                                  Coq_type_object)
                                                (or_decidable
                                                  (type_comparable ty2
                                                    Coq_type_string)
                                                  (type_comparable ty2
                                                    Coq_type_number))
                                           then dc_conv v2 conv_primitive v1
                                           else so false)))

(** val convert_twice :
    ('a2 resultof -> (state -> 'a1 -> ('a1 * 'a1) specres) -> ('a1 * 'a1)
    specres) -> (state -> value -> 'a2 resultof) -> state -> value -> value
    -> ('a1 * 'a1) specres **)

let convert_twice ifv kC s v1 v2 =
  ifv (kC s v1) (fun s1 vc1 ->
    ifv (kC s1 v2) (fun s2 vc2 -> res_spec s2 (vc1, vc2)))

(** val convert_twice_primitive :
    runs_type -> state -> execution_ctx -> value -> value -> (prim * prim)
    specres **)

let convert_twice_primitive runs0 s c v1 v2 =
  convert_twice if_prim (fun s0 v -> to_primitive runs0 s0 c v None) s v1 v2

(** val convert_twice_number :
    runs_type -> state -> execution_ctx -> value -> value ->
    (number * number) specres **)

let convert_twice_number runs0 s c v1 v2 =
  convert_twice if_number (fun s0 v -> to_number runs0 s0 c v) s v1 v2

(** val convert_twice_string :
    runs_type -> state -> execution_ctx -> value -> value ->
    (string * string) specres **)

let convert_twice_string runs0 s c v1 v2 =
  convert_twice if_string (fun s0 v -> to_string runs0 s0 c v) s v1 v2

(** val issome : 'a1 option -> bool **)

let issome _foo_ = match _foo_ with
| Some t -> true
| None -> false

(** val run_binary_op :
    runs_type -> state -> execution_ctx -> binary_op -> value -> value ->
    result **)

let run_binary_op runs0 s c op v1 v2 =
  if binary_op_comparable op Coq_binary_op_add
  then if_spec (convert_twice_primitive runs0 s c v1 v2) (fun s1 ww ->
         let (w1, w2) = ww in
         if or_decidable
              (type_comparable (type_of (Coq_value_prim w1)) Coq_type_string)
              (type_comparable (type_of (Coq_value_prim w2)) Coq_type_string)
         then if_spec
                (convert_twice_string runs0 s1 c (Coq_value_prim w1)
                  (Coq_value_prim w2)) (fun s2 ss ->
                let (s3, s4) = ss in
                res_out (Coq_out_ter (s2,
                  (res_val (Coq_value_prim (Coq_prim_string (append s3 s4)))))))
         else if_spec
                (convert_twice_number runs0 s1 c (Coq_value_prim w1)
                  (Coq_value_prim w2)) (fun s2 nn ->
                let (n1, n2) = nn in
                res_out (Coq_out_ter (s2,
                  (res_val (Coq_value_prim (Coq_prim_number (add n1 n2))))))))
  else if issome (get_puremath_op op)
       then if_some (get_puremath_op op) (fun mop ->
              if_spec (convert_twice_number runs0 s c v1 v2) (fun s1 nn ->
                let (n1, n2) = nn in
                res_out (Coq_out_ter (s1,
                  (res_val (Coq_value_prim (Coq_prim_number (mop n1 n2))))))))
       else if issome (get_shift_op op)
            then if_some (get_shift_op op) (fun so ->
                   let (b_unsigned, f) = so in
                   if_spec
                     ((if b_unsigned then to_uint32 else to_int32) runs0 s c
                       v1) (fun s1 k1 ->
                     if_spec (to_uint32 runs0 s1 c v2) (fun s2 k2 ->
                       let k2' = JsNumber.modulo_32 k2 in
                       res_ter s2
                         (res_val (Coq_value_prim (Coq_prim_number
                           (of_int (f k1 k2'))))))))
            else if issome (get_bitwise_op op)
                 then if_some (get_bitwise_op op) (fun bo ->
                        if_spec (to_int32 runs0 s c v1) (fun s1 k1 ->
                          if_spec (to_int32 runs0 s1 c v2) (fun s2 k2 ->
                            res_ter s2
                              (res_val (Coq_value_prim (Coq_prim_number
                                (of_int (bo k1 k2))))))))
                 else if issome (get_inequality_op op)
                      then if_some (get_inequality_op op) (fun io ->
                             let (b_swap, b_neg) = io in
                             if_spec
                               (convert_twice_primitive runs0 s c v1 v2)
                               (fun s1 ww ->
                               let (w1, w2) = ww in
                               let_binding
                                 (if b_swap then (w2, w1) else (w1, w2))
                                 (fun p ->
                                 let (wa, wb) = p in
                                 let wr = inequality_test_primitive wa wb in
                                 res_out (Coq_out_ter (s1,
                                   (if prim_comparable wr Coq_prim_undef
                                    then res_val (Coq_value_prim
                                           (Coq_prim_bool false))
                                    else if and_decidable
                                              (bool_comparable b_neg true)
                                              (prim_comparable wr
                                                (Coq_prim_bool true))
                                         then res_val (Coq_value_prim
                                                (Coq_prim_bool false))
                                         else if and_decidable
                                                   (bool_comparable b_neg
                                                     true)
                                                   (prim_comparable wr
                                                     (Coq_prim_bool false))
                                              then res_val (Coq_value_prim
                                                     (Coq_prim_bool true))
                                              else res_val (Coq_value_prim
                                                     wr)))))))
                      else if binary_op_comparable op
                                Coq_binary_op_instanceof
                           then (match v2 with
                                 | Coq_value_prim p ->
                                   run_error s Coq_native_error_type
                                 | Coq_value_object l ->
                                   if_some
                                     (run_object_method object_has_instance_
                                       s l) (fun b ->
                                     option_case (fun x ->
                                       run_error s Coq_native_error_type)
                                       (fun has_instance_id x ->
                                       run_object_has_instance runs0 s c
                                         has_instance_id l v1) b ()))
                           else if binary_op_comparable op Coq_binary_op_in
                                then (match v2 with
                                      | Coq_value_prim p ->
                                        run_error s Coq_native_error_type
                                      | Coq_value_object l ->
                                        if_string (to_string runs0 s c v1)
                                          (fun s2 x ->
                                          object_has_prop runs0 s2 c l x))
                                else if binary_op_comparable op
                                          Coq_binary_op_equal
                                     then runs0.runs_type_equal s c v1 v2
                                     else if binary_op_comparable op
                                               Coq_binary_op_disequal
                                          then if_bool
                                                 (runs0.runs_type_equal s c
                                                   v1 v2) (fun s0 b0 ->
                                                 res_ter s0
                                                   (res_val (Coq_value_prim
                                                     (Coq_prim_bool
                                                     (negb b0)))))
                                          else if binary_op_comparable op
                                                    Coq_binary_op_strict_equal
                                               then result_out (Coq_out_ter
                                                      (s,
                                                      (res_val
                                                        (Coq_value_prim
                                                        (Coq_prim_bool
                                                        (strict_equality_test
                                                          v1 v2))))))
                                               else if binary_op_comparable
                                                         op
                                                         Coq_binary_op_strict_disequal
                                                    then result_out
                                                           (Coq_out_ter (s,
                                                           (res_val
                                                             (Coq_value_prim
                                                             (Coq_prim_bool
                                                             (negb
                                                               (strict_equality_test
                                                                 v1 v2)))))))
                                                    else if binary_op_comparable
                                                              op
                                                              Coq_binary_op_coma
                                                         then result_out
                                                                (Coq_out_ter
                                                                (s,
                                                                (res_val v2)))
                                                         else (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                                                                s
                                                                ("Undealt lazy operator in [run_binary_op].")

(** val run_prepost_op : unary_op -> ((number -> number) * bool) option **)

let run_prepost_op _foo_ = match _foo_ with
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

let run_typeof_value s _foo_ = match _foo_ with
| Coq_value_prim w -> typeof_prim w
| Coq_value_object l ->
  if is_callable_dec s (Coq_value_object l)
  then "function"
  else "object"

(** val run_unary_op :
    runs_type -> state -> execution_ctx -> unary_op -> expr -> result **)

let run_unary_op runs0 s c op e =
  if prepost_unary_op_dec op
  then if_success (runs0.runs_type_expr s c e) (fun s1 rv1 ->
         if_spec (ref_get_value runs0 s1 c rv1) (fun s2 v2 ->
           if_number (to_number runs0 s2 c v2) (fun s3 n1 ->
             if_some (run_prepost_op op) (fun po ->
               let (number_op, is_pre) = po in
               let_binding (number_op n1) (fun n2 ->
                 let_binding (Coq_prim_number (if is_pre then n2 else n1))
                   (fun v ->
                   if_void
                     (ref_put_value runs0 s3 c rv1 (Coq_value_prim
                       (Coq_prim_number n2))) (fun s4 ->
                     result_out (Coq_out_ter (s4,
                       (res_val (Coq_value_prim v)))))))))))
  else (match op with
        | Coq_unary_op_delete ->
          if_success (runs0.runs_type_expr s c e) (fun s0 rv ->
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
                      if_object (to_object s0 v) (fun s1 l ->
                        runs0.runs_type_object_delete s1 c l r.ref_name
                          r.ref_strict)
                    | Coq_ref_base_type_env_loc l ->
                      if r.ref_strict
                      then run_error s0 Coq_native_error_syntax
                      else env_record_delete_binding runs0 s0 c l r.ref_name))
        | Coq_unary_op_void ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_typeof ->
          if_success (runs0.runs_type_expr s c e) (fun s1 rv ->
            match rv with
            | Coq_resvalue_empty ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
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
              else if_spec (ref_get_value runs0 s1 c (Coq_resvalue_ref r))
                     (fun s2 v ->
                     res_ter s2
                       (res_val (Coq_value_prim (Coq_prim_string
                         (run_typeof_value s2 v))))))
        | Coq_unary_op_post_incr ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_post_decr ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_pre_incr ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_pre_decr ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_add ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_neg ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_bitwise_not ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v))))))
        | Coq_unary_op_not ->
          if_spec (run_expr_get_value runs0 s c e) (fun s1 v ->
            match op with
            | Coq_unary_op_delete ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_void ->
              res_ter s1 (res_val (Coq_value_prim Coq_prim_undef))
            | Coq_unary_op_typeof ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_post_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_incr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_pre_decr ->
              (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                s1
                ("Undealt regular operator in [run_unary_op].")
            | Coq_unary_op_add -> to_number runs0 s1 c v
            | Coq_unary_op_neg ->
              if_number (to_number runs0 s1 c v) (fun s2 n ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (JsNumber.neg n)))))
            | Coq_unary_op_bitwise_not ->
              if_spec (to_int32 runs0 s1 c v) (fun s2 k ->
                res_ter s2
                  (res_val (Coq_value_prim (Coq_prim_number
                    (of_int (JsNumber.int32_bitwise_not k))))))
            | Coq_unary_op_not ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_bool
                  (neg (convert_value_to_boolean v)))))))

(** val create_new_function_in :
    runs_type -> state -> execution_ctx -> string list -> funcbody ->
    result **)

let create_new_function_in runs0 s c args bd =
  creating_function_object runs0 s c args bd c.execution_ctx_lexical_env
    c.execution_ctx_strict

(** val init_object :
    runs_type -> state -> execution_ctx -> object_loc -> propdefs -> result **)

let rec init_object runs0 s c l _foo_ = match _foo_ with
| [] -> result_out (Coq_out_ter (s, (res_val (Coq_value_object l))))
| p :: pds' ->
  let (pn, pb) = p in
  let_binding (string_of_propname pn) (fun x ->
    let_binding (fun s1 desc ->
      if_success (object_define_own_prop runs0 s1 c l x desc false)
        (fun s2 rv -> init_object runs0 s2 c l pds')) (fun follows ->
      match pb with
      | Coq_propbody_val e0 ->
        if_spec (run_expr_get_value runs0 s c e0) (fun s1 v0 ->
          let desc = { descriptor_value = (Some v0); descriptor_writable =
            (Some true); descriptor_get = None; descriptor_set = None;
            descriptor_enumerable = (Some true); descriptor_configurable =
            (Some true) }
          in
          follows s1 desc)
      | Coq_propbody_get bd ->
        if_value (create_new_function_in runs0 s c [] bd) (fun s1 v0 ->
          let desc = { descriptor_value = None; descriptor_writable = None;
            descriptor_get = (Some v0); descriptor_set = None;
            descriptor_enumerable = (Some true); descriptor_configurable =
            (Some true) }
          in
          follows s1 desc)
      | Coq_propbody_set (args, bd) ->
        if_value (create_new_function_in runs0 s c args bd) (fun s1 v0 ->
          let desc = { descriptor_value = None; descriptor_writable = None;
            descriptor_get = None; descriptor_set = (Some v0);
            descriptor_enumerable = (Some true); descriptor_configurable =
            (Some true) }
          in
          follows s1 desc)))

(** val run_array_element_list :
    runs_type -> state -> execution_ctx -> object_loc -> expr option list ->
    float -> result **)

let run_array_element_list runs0 s c l oes n =
  match oes with
  | [] -> result_out (Coq_out_ter (s, (res_val (Coq_value_object l))))
  | o :: oes' ->
    (match o with
     | Some e ->
       let_binding (fun s0 ->
         runs0.runs_type_array_element_list s0 c l oes' 0.)
         (fun loop_result ->
         if_spec (run_expr_get_value runs0 s c e) (fun s0 v ->
           if_value
             (run_object_get runs0 s0 c l
               ("length")) (fun s1 vlen ->
             if_spec (to_uint32 runs0 s1 c vlen) (fun s2 ilen ->
               if_string
                 (to_string runs0 s2 c (Coq_value_prim (Coq_prim_number
                   (ilen +. n)))) (fun s3 slen ->
                 let_binding { attributes_data_value = v;
                   attributes_data_writable = true;
                   attributes_data_enumerable = true;
                   attributes_data_configurable = true } (fun desc ->
                   if_bool
                     (object_define_own_prop runs0 s3 c l slen
                       (descriptor_of_attributes (Coq_attributes_data_of
                         desc)) false) (fun s4 x ->
                     if_object (loop_result s4) (fun s5 l0 ->
                       res_ter s5 (res_val (Coq_value_object l0))))))))))
     | None ->
       let_binding (elision_head_count (None :: oes')) (fun firstIndex ->
         runs0.runs_type_array_element_list s c l
           (elision_head_remove (None :: oes')) (number_of_int firstIndex)))

(** val init_array :
    runs_type -> state -> execution_ctx -> object_loc -> expr option list ->
    result **)

let init_array runs0 s c l oes =
  let_binding (elision_tail_remove oes) (fun elementList ->
    let_binding (elision_tail_count oes) (fun elisionLength ->
      if_object (run_array_element_list runs0 s c l elementList 0.)
        (fun s0 l0 ->
        if_value
          (run_object_get runs0 s0 c l0
            ("length")) (fun s1 vlen ->
          if_spec (to_uint32 runs0 s1 c vlen) (fun s2 ilen ->
            if_spec
              (to_uint32 runs0 s2 c (Coq_value_prim (Coq_prim_number
                (ilen +. number_of_int elisionLength))))
              (fun s3 len ->
              if_not_throw
                (object_put runs0 s3 c l0
                  ("length")
                  (Coq_value_prim (Coq_prim_number (of_int len)))
                  throw_false) (fun s4 x ->
                result_out (Coq_out_ter (s4,
                  (res_val (Coq_value_object l0)))))))))))

(** val run_var_decl_item :
    runs_type -> state -> execution_ctx -> prop_name -> expr option -> result **)

let run_var_decl_item runs0 s c x _foo_ = match _foo_ with
| Some e ->
  if_spec (identifier_resolution runs0 s c x) (fun s1 ir ->
    if_spec (run_expr_get_value runs0 s1 c e) (fun s2 v ->
      if_void (ref_put_value runs0 s2 c (Coq_resvalue_ref ir) v) (fun s3 ->
        result_out (Coq_out_ter (s3,
          (res_val (Coq_value_prim (Coq_prim_string x))))))))
| None ->
  result_out (Coq_out_ter (s,
    (res_val (Coq_value_prim (Coq_prim_string x)))))

(** val run_var_decl :
    runs_type -> state -> execution_ctx -> (prop_name * expr option) list ->
    result **)

let rec run_var_decl runs0 s c _foo_ = match _foo_ with
| [] -> result_out (Coq_out_ter (s, res_empty))
| y :: xeos' ->
  let (x, eo) = y in
  if_value (run_var_decl_item runs0 s c x eo) (fun s1 vname ->
    run_var_decl runs0 s1 c xeos')

(** val run_list_expr :
    runs_type -> state -> execution_ctx -> value list -> expr list -> value
    list specres **)

let rec run_list_expr runs0 s1 c vs _foo_ = match _foo_ with
| [] -> res_spec s1 (rev vs)
| e :: es' ->
  if_spec (run_expr_get_value runs0 s1 c e) (fun s2 v ->
    run_list_expr runs0 s2 c (v :: vs) es')

(** val run_block :
    runs_type -> state -> execution_ctx -> stat list -> result **)

let rec run_block runs0 s c _foo_ = match _foo_ with
| [] -> res_ter s (res_normal Coq_resvalue_empty)
| t :: ts_rev' ->
  if_success (run_block runs0 s c ts_rev') (fun s0 rv0 ->
    if_success_state rv0 (runs0.runs_type_stat s0 c t) (fun x x0 ->
      result_out (Coq_out_ter (x, (res_normal x0)))))

(** val run_expr_binary_op :
    runs_type -> state -> execution_ctx -> binary_op -> expr -> expr ->
    result **)

let run_expr_binary_op runs0 s c op e1 e2 =
  match is_lazy_op op with
  | Some b_ret ->
    if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
      let_binding (convert_value_to_boolean v1) (fun b1 ->
        if bool_comparable b1 b_ret
        then res_ter s1 (res_val v1)
        else if_spec (run_expr_get_value runs0 s1 c e2) (fun s2 v ->
               res_ter s2 (res_val v))))
  | None ->
    if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
      if_spec (run_expr_get_value runs0 s1 c e2) (fun s2 v2 ->
        run_binary_op runs0 s2 c op v1 v2))

(** val run_expr_access :
    runs_type -> state -> execution_ctx -> expr -> expr -> result **)

let run_expr_access runs0 s c e1 e2 =
  if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
    if_spec (run_expr_get_value runs0 s1 c e2) (fun s2 v2 ->
      if or_decidable (value_comparable v1 (Coq_value_prim Coq_prim_undef))
           (value_comparable v1 (Coq_value_prim Coq_prim_null))
      then run_error s2 Coq_native_error_type
      else if_string (to_string runs0 s2 c v2) (fun s3 x ->
             res_ter s3
               (res_ref (ref_create_value v1 x c.execution_ctx_strict)))))

(** val run_expr_assign :
    runs_type -> state -> execution_ctx -> binary_op option -> expr -> expr
    -> result **)

let run_expr_assign runs0 s c opo e1 e2 =
  if_success (runs0.runs_type_expr s c e1) (fun s1 rv1 ->
    let_binding (fun s0 rv' ->
      match rv' with
      | Coq_resvalue_empty ->
        (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
          s0
          ("Non-value result in [run_expr_assign].")
      | Coq_resvalue_value v ->
        if_void (ref_put_value runs0 s0 c rv1 v) (fun s' ->
          result_out (Coq_out_ter (s', (res_val v))))
      | Coq_resvalue_ref r ->
        (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
          s0
          ("Non-value result in [run_expr_assign]."))
      (fun follow ->
      match opo with
      | Some op ->
        if_spec (ref_get_value runs0 s1 c rv1) (fun s2 v1 ->
          if_spec (run_expr_get_value runs0 s2 c e2) (fun s3 v2 ->
            if_success (run_binary_op runs0 s3 c op v1 v2) follow))
      | None ->
        if_spec (run_expr_get_value runs0 s1 c e2) (fun x x0 ->
          follow x (Coq_resvalue_value x0))))

(** val run_expr_function :
    runs_type -> state -> execution_ctx -> prop_name option -> string list
    -> funcbody -> result **)

let run_expr_function runs0 s c fo args bd =
  match fo with
  | Some fn ->
    let_binding (lexical_env_alloc_decl s c.execution_ctx_lexical_env)
      (fun p ->
      let (lex', s') = p in
      let follow = fun l ->
        if_some (env_record_binds_pickable_option s' l) (fun e ->
          if_void (env_record_create_immutable_binding s' l fn) (fun s1 ->
            if_object
              (creating_function_object runs0 s1 c args bd lex'
                (funcbody_is_strict bd)) (fun s2 l0 ->
              if_void
                (env_record_initialize_immutable_binding s2 l fn
                  (Coq_value_object l0)) (fun s3 ->
                result_out (Coq_out_ter (s3,
                  (res_val (Coq_value_object l0))))))))
      in
      destr_list lex' (fun x ->
        (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
          s'
          ("Empty lexical environnment allocated in [run_expr_function]."))
        (fun l x -> follow l) ())
  | None ->
    let lex = c.execution_ctx_lexical_env in
    creating_function_object runs0 s c args bd lex (funcbody_is_strict bd)

(** val entering_eval_code :
    runs_type -> state -> execution_ctx -> bool -> funcbody -> (state ->
    execution_ctx -> result) -> result **)

let entering_eval_code runs0 s c direct bd k =
  let_binding
    (coq_or (funcbody_is_strict bd) ( direct && c.execution_ctx_strict))
    (fun str ->
    let_binding (if direct then c else execution_ctx_initial str) (fun c' ->
      let_binding
        (if str
         then lexical_env_alloc_decl s c'.execution_ctx_lexical_env
         else (c'.execution_ctx_lexical_env, s)) (fun p ->
        let (lex, s') = p in
        let_binding (if str then execution_ctx_with_lex_same c' lex else c')
          (fun c1 ->
          let_binding (funcbody_prog bd) (fun p0 ->
            if_void
              (execution_ctx_binding_inst runs0 s' c1 Coq_codetype_eval None
                p0 []) (fun s1 -> k s1 c1))))))

(** val run_eval :
    runs_type -> state -> execution_ctx -> bool -> value list -> result **)

let run_eval runs0 s c is_direct_call vs =
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
       let_binding (coq_and is_direct_call c.execution_ctx_strict)
         (fun str ->
         match parse_pickable s0 str with
         | Some p0 ->
           entering_eval_code runs0 s c is_direct_call (Coq_funcbody_intro
             (p0, s0)) (fun s1 c' ->
             if_ter (runs0.runs_type_prog s1 c' p0) (fun s2 r ->
               match r.res_type with
               | Coq_restype_normal ->
                 if_empty_label s2 r (fun x ->
                   match r.res_value with
                   | Coq_resvalue_empty ->
                     res_ter s2 (res_val (Coq_value_prim Coq_prim_undef))
                   | Coq_resvalue_value v -> res_ter s2 (res_val v)
                   | Coq_resvalue_ref r0 ->
                     (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                       s2
                       ("Reference found in the result of an `eval\' in [run_eval]."))
               | Coq_restype_break ->
                 (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                   s2
                   ("Forbidden result type returned by an `eval\' in [run_eval].")
               | Coq_restype_continue ->
                 (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                   s2
                   ("Forbidden result type returned by an `eval\' in [run_eval].")
               | Coq_restype_return ->
                 (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                   s2
                   ("Forbidden result type returned by an `eval\' in [run_eval].")
               | Coq_restype_throw -> res_ter s2 (res_throw r.res_value)))
         | None -> run_error s Coq_native_error_syntax))
  | Coq_value_object o ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_object o))))

(** val run_expr_call :
    runs_type -> state -> execution_ctx -> expr -> expr list -> result **)

let run_expr_call runs0 s c e1 e2s =
  let_binding (is_syntactic_eval e1) (fun is_eval_direct ->
    if_success (runs0.runs_type_expr s c e1) (fun s1 rv ->
      if_spec (ref_get_value runs0 s1 c rv) (fun s2 f ->
        if_spec (run_list_expr runs0 s2 c [] e2s) (fun s3 vs ->
          match f with
          | Coq_value_prim p -> run_error s3 Coq_native_error_type
          | Coq_value_object l ->
            if is_callable_dec s3 (Coq_value_object l)
            then let_binding (fun vthis ->
                   if object_loc_comparable l (Coq_object_loc_prealloc
                        Coq_prealloc_global_eval)
                   then run_eval runs0 s3 c is_eval_direct vs
                   else runs0.runs_type_call s3 c l vthis vs) (fun follow ->
                   match rv with
                   | Coq_resvalue_empty ->
                     (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                       s3
                       ("[run_expr_call] unable to call an  empty result.")
                   | Coq_resvalue_value v ->
                     follow (Coq_value_prim Coq_prim_undef)
                   | Coq_resvalue_ref r ->
                     (match r.ref_base with
                      | Coq_ref_base_type_value v ->
                        if or_decidable
                             (ref_kind_comparable (ref_kind_of r)
                               Coq_ref_kind_primitive_base)
                             (or_decidable
                               (ref_kind_comparable (ref_kind_of r)
                                 Coq_ref_kind_null)
                               (ref_kind_comparable (ref_kind_of r)
                                 Coq_ref_kind_object))
                        then follow v
                        else (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                               s3
                               ("[run_expr_call] unable to call a non-property function.")
                      | Coq_ref_base_type_env_loc l0 ->
                        if_some (env_record_implicit_this_value s3 l0) follow))
            else run_error s3 Coq_native_error_type))))

(** val run_expr_conditionnal :
    runs_type -> state -> execution_ctx -> expr -> expr -> expr -> result **)

let run_expr_conditionnal runs0 s c e1 e2 e3 =
  if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
    let_binding (convert_value_to_boolean v1) (fun b ->
      let_binding (if b then e2 else e3) (fun e ->
        if_spec (run_expr_get_value runs0 s1 c e) (fun s0 r ->
          res_ter s0 (res_val r)))))

(** val run_expr_new :
    runs_type -> state -> execution_ctx -> expr -> expr list -> result **)

let run_expr_new runs0 s c e1 e2s =
  if_spec (run_expr_get_value runs0 s c e1) (fun s1 v ->
    if_spec (run_list_expr runs0 s1 c [] e2s) (fun s2 args ->
      match v with
      | Coq_value_prim p -> run_error s2 Coq_native_error_type
      | Coq_value_object l ->
        if_some (run_object_method object_construct_ s2 l) (fun coo ->
          match coo with
          | Some co -> run_construct runs0 s2 c co l args
          | None -> run_error s2 Coq_native_error_type)))

(** val run_stat_label :
    runs_type -> state -> execution_ctx -> label -> stat -> result **)

let run_stat_label runs0 s c lab t =
  if_break (runs0.runs_type_stat s c t) (fun s1 r1 ->
    result_out (Coq_out_ter (s1,
      (if label_comparable r1.res_label lab
       then res_normal r1.res_value
       else r1))))

(** val run_stat_with :
    runs_type -> state -> execution_ctx -> expr -> stat -> result **)

let run_stat_with runs0 s c e1 t2 =
  if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
    if_object (to_object s1 v1) (fun s2 l ->
      let_binding c.execution_ctx_lexical_env (fun lex ->
        let_binding (lexical_env_alloc_object s2 lex l provide_this_true)
          (fun p ->
          let (lex', s3) = p in
          let_binding (execution_ctx_with_lex c lex') (fun c' ->
            runs0.runs_type_stat s3 c' t2)))))

(** val run_stat_if :
    runs_type -> state -> execution_ctx -> expr -> stat -> stat option ->
    result **)

let run_stat_if runs0 s c e1 t2 to0 =
  if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
    let_binding (convert_value_to_boolean v1) (fun b ->
      if b
      then runs0.runs_type_stat s1 c t2
      else (match to0 with
            | Some t3 -> runs0.runs_type_stat s1 c t3
            | None ->
              result_out (Coq_out_ter (s1, (res_normal Coq_resvalue_empty))))))

(** val run_stat_while :
    runs_type -> state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

let run_stat_while runs0 s c rv labs e1 t2 =
  if_spec (run_expr_get_value runs0 s c e1) (fun s1 v1 ->
    let_binding (convert_value_to_boolean v1) (fun b ->
      if b
      then if_ter (runs0.runs_type_stat s1 c t2) (fun s2 r ->
             let_binding
               (if not_decidable
                     (resvalue_comparable r.res_value Coq_resvalue_empty)
                then r.res_value
                else rv) (fun rv' ->
               let_binding (fun x ->
                 runs0.runs_type_stat_while s2 c rv' labs e1 t2) (fun loop ->
                 if or_decidable
                      (not_decidable
                        (restype_comparable r.res_type Coq_restype_continue))
                      (not_decidable (bool_decidable (res_label_in r labs)))
                 then if and_decidable
                           (restype_comparable r.res_type Coq_restype_break)
                           (bool_decidable (res_label_in r labs))
                      then res_ter s2 (res_normal rv')
                      else if not_decidable
                                (restype_comparable r.res_type
                                  Coq_restype_normal)
                           then res_ter s2 r
                           else loop ()
                 else loop ())))
      else res_ter s1 (res_normal rv)))

(** val run_stat_switch_end :
    runs_type -> state -> execution_ctx -> resvalue -> switchclause list ->
    result **)

let rec run_stat_switch_end runs0 s c rv _foo_ = match _foo_ with
| [] -> result_out (Coq_out_ter (s, (res_normal rv)))
| y :: scs' ->
  match y with Coq_switchclause_intro (e, ts) ->
  if_success_state rv (run_block runs0 s c (rev ts)) (fun s1 rv1 ->
    run_stat_switch_end runs0 s1 c rv1 scs')

(** val run_stat_switch_no_default :
    runs_type -> state -> execution_ctx -> value -> resvalue -> switchclause
    list -> result **)

let rec run_stat_switch_no_default runs0 s c vi rv _foo_ = match _foo_ with
| [] -> result_out (Coq_out_ter (s, (res_normal rv)))
| y :: scs' ->
  match y with Coq_switchclause_intro (e, ts) ->
  if_spec (run_expr_get_value runs0 s c e) (fun s1 v1 ->
    let_binding (strict_equality_test v1 vi) (fun b ->
      if b
      then if_success (run_block runs0 s1 c (rev ts)) (fun s2 rv2 ->
             run_stat_switch_end runs0 s2 c rv2 scs')
      else run_stat_switch_no_default runs0 s1 c vi rv scs'))

(** val run_stat_switch_with_default_default :
    runs_type -> state -> execution_ctx -> stat list -> switchclause list ->
    result **)

let run_stat_switch_with_default_default runs0 s c ts scs =
  if_success (run_block runs0 s c (rev ts)) (fun s1 rv ->
    run_stat_switch_end runs0 s1 c rv scs)

(** val run_stat_switch_with_default_B :
    runs_type -> state -> execution_ctx -> value -> resvalue -> stat list ->
    switchclause list -> result **)

let rec run_stat_switch_with_default_B runs0 s c vi rv ts0 scs = match scs with
| [] -> run_stat_switch_with_default_default runs0 s c ts0 scs
| y :: scs' ->
  match y with Coq_switchclause_intro (e, ts) ->
  if_spec (run_expr_get_value runs0 s c e) (fun s1 v1 ->
    let_binding (strict_equality_test v1 vi) (fun b ->
      if b
      then if_success (run_block runs0 s1 c (rev ts)) (fun s2 rv2 ->
             run_stat_switch_end runs0 s2 c rv2 scs')
      else run_stat_switch_with_default_B runs0 s1 c vi rv ts0 scs'))

(** val run_stat_switch_with_default_A :
    runs_type -> state -> execution_ctx -> bool -> value -> resvalue ->
    switchclause list -> stat list -> switchclause list -> result **)

let rec run_stat_switch_with_default_A runs0 s c found vi rv scs1 ts0 scs2 =
  match scs1 with
  | [] ->
    if found
    then run_stat_switch_with_default_default runs0 s c ts0 scs2
    else run_stat_switch_with_default_B runs0 s c vi rv ts0 scs2
  | y :: scs' ->
    match y with Coq_switchclause_intro (e, ts) ->
    let_binding (fun s0 ->
      if_success_state rv (run_block runs0 s0 c (rev ts)) (fun s1 rv0 ->
        run_stat_switch_with_default_A runs0 s1 c true vi rv0 scs' ts0 scs2))
      (fun follow ->
      if found
      then follow s
      else if_spec (run_expr_get_value runs0 s c e) (fun s1 v1 ->
             let_binding (strict_equality_test v1 vi) (fun b ->
               if b
               then follow s1
               else run_stat_switch_with_default_A runs0 s1 c false vi rv
                      scs' ts0 scs2)))

(** val run_stat_switch :
    runs_type -> state -> execution_ctx -> label_set -> expr -> switchbody ->
    result **)

let run_stat_switch runs0 s c labs e sb =
  if_spec (run_expr_get_value runs0 s c e) (fun s1 vi ->
    let_binding (fun w ->
      if_success
        (if_break w (fun s2 r ->
          if res_label_in r labs
          then result_out (Coq_out_ter (s2, (res_normal r.res_value)))
          else result_out (Coq_out_ter (s2, r)))) (fun s0 r ->
        res_ter s0 (res_normal r))) (fun follow ->
      match sb with
      | Coq_switchbody_nodefault scs ->
        follow
          (run_stat_switch_no_default runs0 s1 c vi Coq_resvalue_empty scs)
      | Coq_switchbody_withdefault (scs1, ts, scs2) ->
        follow
          (run_stat_switch_with_default_A runs0 s1 c false vi
            Coq_resvalue_empty scs1 ts scs2)))

(** val run_stat_do_while :
    runs_type -> state -> execution_ctx -> resvalue -> label_set -> expr ->
    stat -> result **)

let run_stat_do_while runs0 s c rv labs e1 t2 =
  if_ter (runs0.runs_type_stat s c t2) (fun s1 r ->
    let_binding
      (if resvalue_comparable r.res_value Coq_resvalue_empty
       then rv
       else r.res_value) (fun rv' ->
      let_binding (fun x ->
        if_spec (run_expr_get_value runs0 s1 c e1) (fun s2 v1 ->
          let_binding (convert_value_to_boolean v1) (fun b ->
            if b
            then runs0.runs_type_stat_do_while s2 c rv' labs e1 t2
            else res_ter s2 (res_normal rv')))) (fun loop ->
        if and_decidable (restype_comparable r.res_type Coq_restype_continue)
             (bool_decidable (res_label_in r labs))
        then loop ()
        else if and_decidable
                  (restype_comparable r.res_type Coq_restype_break)
                  (bool_decidable (res_label_in r labs))
             then res_ter s1 (res_normal rv')
             else if not_decidable
                       (restype_comparable r.res_type Coq_restype_normal)
                  then res_ter s1 r
                  else loop ())))

(** val run_stat_try :
    runs_type -> state -> execution_ctx -> stat -> (prop_name * stat) option
    -> stat option -> result **)

let run_stat_try runs0 s c t1 t2o t3o =
  let_binding (fun s1 r ->
    match t3o with
    | Some t3 ->
      if_success (runs0.runs_type_stat s1 c t3) (fun s2 rv' -> res_ter s2 r)
    | None -> res_ter s1 r) (fun finallycont ->
    if_any_or_throw (runs0.runs_type_stat s c t1) finallycont (fun s1 v ->
      match t2o with
      | Some y ->
        let (x, t2) = y in
        let_binding c.execution_ctx_lexical_env (fun lex ->
          let_binding (lexical_env_alloc_decl s1 lex) (fun p ->
            let (lex', s') = p in
            (match lex' with
             | [] ->
               (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                 s'
                 ("Empty lexical environnment in [run_stat_try].")
             | l :: oldlex ->
               if_void
                 (env_record_create_set_mutable_binding runs0 s' c l x None v
                   throw_irrelevant) (fun s2 ->
                 let c' = execution_ctx_with_lex c lex' in
                 if_ter (runs0.runs_type_stat s2 c' t2) finallycont))))
      | None -> finallycont s1 (res_throw (Coq_resvalue_value v))))

(** val run_stat_throw :
    runs_type -> state -> execution_ctx -> expr -> result **)

let run_stat_throw runs0 s c e =
  if_spec (run_expr_get_value runs0 s c e) (fun s1 v1 ->
    res_ter s1 (res_throw (Coq_resvalue_value v1)))

(** val run_stat_return :
    runs_type -> state -> execution_ctx -> expr option -> result **)

let run_stat_return runs0 s c _foo_ = match _foo_ with
| Some e ->
  if_spec (run_expr_get_value runs0 s c e) (fun s1 v1 ->
    res_ter s1 (res_return (Coq_resvalue_value v1)))
| None ->
  result_out (Coq_out_ter (s,
    (res_return (Coq_resvalue_value (Coq_value_prim Coq_prim_undef)))))

(** val run_stat_for_loop :
    runs_type -> state -> execution_ctx -> label_set -> resvalue -> expr
    option -> expr option -> stat -> result **)

let run_stat_for_loop runs0 s c labs rv eo2 eo3 t =
  let_binding (fun s0 ->
    if_ter (runs0.runs_type_stat s0 c t) (fun s1 r ->
      let_binding
        (if not_decidable
              (resvalue_comparable r.res_value Coq_resvalue_empty)
         then r.res_value
         else rv) (fun rv' ->
        let_binding (fun s2 ->
          runs0.runs_type_stat_for_loop s2 c labs rv' eo2 eo3 t) (fun loop ->
          if and_decidable (restype_comparable r.res_type Coq_restype_break)
               (bool_decidable (res_label_in r labs))
          then res_ter s1 (res_normal rv')
          else if or_decidable
                    (restype_comparable r.res_type Coq_restype_normal)
                    (and_decidable
                      (restype_comparable r.res_type Coq_restype_continue)
                      (bool_decidable (res_label_in r labs)))
               then (match eo3 with
                     | Some e3 ->
                       if_spec (run_expr_get_value runs0 s1 c e3)
                         (fun s2 v3 -> loop s2)
                     | None -> loop s1)
               else res_ter s1 r)))) (fun follows ->
    match eo2 with
    | Some e2 ->
      if_spec (run_expr_get_value runs0 s c e2) (fun s0 v2 ->
        let_binding (convert_value_to_boolean v2) (fun b ->
          if b then follows s0 else res_ter s0 (res_normal rv)))
    | None -> follows s)

(** val run_stat_for :
    runs_type -> state -> execution_ctx -> label_set -> expr option -> expr
    option -> expr option -> stat -> result **)

let run_stat_for runs0 s c labs eo1 eo2 eo3 t =
  let follows = fun s0 ->
    runs0.runs_type_stat_for_loop s0 c labs Coq_resvalue_empty eo2 eo3 t
  in
  (match eo1 with
   | Some e1 ->
     if_spec (run_expr_get_value runs0 s c e1) (fun s0 v1 -> follows s0)
   | None -> follows s)

(** val run_stat_for_var :
    runs_type -> state -> execution_ctx -> label_set -> (string * expr
    option) list -> expr option -> expr option -> stat -> result **)

let run_stat_for_var runs0 s c labs ds eo2 eo3 t =
  if_ter (runs0.runs_type_stat s c (Coq_stat_var_decl ds)) (fun s0 r ->
    runs0.runs_type_stat_for_loop s0 c labs Coq_resvalue_empty eo2 eo3 t)

(** val run_expr : runs_type -> state -> execution_ctx -> expr -> result **)

let run_expr runs0 s c _term_ = match _term_ with
| Coq_expr_this ->
  result_out (Coq_out_ter (s, (res_val c.execution_ctx_this_binding)))
| Coq_expr_identifier x ->
  if_spec (identifier_resolution runs0 s c x) (fun s0 r ->
    res_ter s0 (res_ref r))
| Coq_expr_literal i ->
  result_out (Coq_out_ter (s,
    (res_val (Coq_value_prim (convert_literal_to_prim i)))))
| Coq_expr_object pds ->
  if_object (run_construct_prealloc runs0 s c Coq_prealloc_object [])
    (fun s1 l -> init_object runs0 s1 c l pds)
| Coq_expr_array oes ->
  if_object (run_construct_prealloc runs0 s c Coq_prealloc_array [])
    (fun s1 l -> init_array runs0 s1 c l oes)
| Coq_expr_function (fo, args, bd) -> run_expr_function runs0 s c fo args bd
| Coq_expr_access (e1, e2) -> run_expr_access runs0 s c e1 e2
| Coq_expr_member (e1, f) ->
  runs0.runs_type_expr s c (Coq_expr_access (e1, (Coq_expr_literal
    (Coq_literal_string f))))
| Coq_expr_new (e1, e2s) -> run_expr_new runs0 s c e1 e2s
| Coq_expr_call (e1, e2s) -> run_expr_call runs0 s c e1 e2s
| Coq_expr_unary_op (op, e0) -> run_unary_op runs0 s c op e0
| Coq_expr_binary_op (e1, op, e2) -> run_expr_binary_op runs0 s c op e1 e2
| Coq_expr_conditional (e1, e2, e3) ->
  run_expr_conditionnal runs0 s c e1 e2 e3
| Coq_expr_assign (e1, opo, e2) -> run_expr_assign runs0 s c opo e1 e2

(** val run_stat : runs_type -> state -> execution_ctx -> stat -> result **)

let run_stat runs0 s c _term_ = match _term_ with
| Coq_stat_expr e ->
  if_spec (run_expr_get_value runs0 s c e) (fun s0 r ->
    res_ter s0 (res_val r))
| Coq_stat_label (lab, t0) ->
  run_stat_label runs0 s c (Coq_label_string lab) t0
| Coq_stat_block ts -> run_block runs0 s c (rev ts)
| Coq_stat_var_decl xeos -> run_var_decl runs0 s c xeos
| Coq_stat_if (e1, t2, to0) -> run_stat_if runs0 s c e1 t2 to0
| Coq_stat_do_while (ls, t1, e2) ->
  runs0.runs_type_stat_do_while s c Coq_resvalue_empty ls e2 t1
| Coq_stat_while (ls, e1, t2) ->
  runs0.runs_type_stat_while s c Coq_resvalue_empty ls e1 t2
| Coq_stat_with (e1, t2) -> run_stat_with runs0 s c e1 t2
| Coq_stat_throw e -> run_stat_throw runs0 s c e
| Coq_stat_return eo -> run_stat_return runs0 s c eo
| Coq_stat_break so -> result_out (Coq_out_ter (s, (res_break so)))
| Coq_stat_continue so -> result_out (Coq_out_ter (s, (res_continue so)))
| Coq_stat_try (t1, t2o, t3o) -> run_stat_try runs0 s c t1 t2o t3o
| Coq_stat_for (ls, eo1, eo2, eo3, s0) ->
  run_stat_for runs0 s c ls eo1 eo2 eo3 s0
| Coq_stat_for_var (ls, ds, eo2, eo3, s0) ->
  run_stat_for_var runs0 s c ls ds eo2 eo3 s0
| Coq_stat_for_in (ls, e1, e2, s0) ->
  (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
    ("stat_for_in")
| Coq_stat_for_in_var (ls, x, e1o, e2, s0) ->
  (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
    ("stat_for_in_var")
| Coq_stat_debugger -> result_out (Coq_out_ter (s, res_empty))
| Coq_stat_switch (labs, e, sb) -> run_stat_switch runs0 s c labs e sb

(** val run_elements :
    runs_type -> state -> execution_ctx -> elements -> result **)

let rec run_elements runs0 s c _foo_ = match _foo_ with
| [] -> result_out (Coq_out_ter (s, (res_normal Coq_resvalue_empty)))
| el :: els_rev' ->
  if_success (run_elements runs0 s c els_rev') (fun s0 rv0 ->
    match el with
    | Coq_element_stat t ->
      if_ter (runs0.runs_type_stat s0 c t) (fun s1 r1 ->
        let r2 = res_overwrite_value_if_empty rv0 r1 in
        res_out (Coq_out_ter (s1, r2)))
    | Coq_element_func_decl (name, args, bd) -> res_ter s0 (res_normal rv0))

(** val run_prog : runs_type -> state -> execution_ctx -> prog -> result **)

let run_prog runs0 s c _term_ = match _term_ with
| Coq_prog_intro (str, els) -> run_elements runs0 s c (rev els)

(** val push :
    runs_type -> state -> execution_ctx -> object_loc -> value list -> float
    -> result **)

let rec push runs0 s c l args ilen =
  let_binding (of_int ilen) (fun vlen ->
    match args with
    | [] ->
      if_not_throw
        (object_put runs0 s c l ("length")
          (Coq_value_prim (Coq_prim_number vlen)) throw_true) (fun s0 x ->
        result_out (Coq_out_ter (s0,
          (res_val (Coq_value_prim (Coq_prim_number vlen))))))
    | v :: vs ->
      if_string (to_string runs0 s c (Coq_value_prim (Coq_prim_number vlen)))
        (fun s0 slen ->
        if_not_throw (object_put runs0 s0 c l slen v throw_true) (fun s1 x ->
          push runs0 s1 c l vs (ilen +. 1.))))

(** val run_object_is_sealed :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

let rec run_object_is_sealed runs0 s c l _foo_ = match _foo_ with
| [] ->
  if_some (run_object_method object_extensible_ s l) (fun ext ->
    res_ter s (res_val (Coq_value_prim (Coq_prim_bool (neg ext)))))
| x :: xs' ->
  if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s0 d ->
    match d with
    | Coq_full_descriptor_undef ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s0
        ("[run_object_is_sealed]:  Undefined descriptor found in a place where it shouldn\'t.")
    | Coq_full_descriptor_some a ->
      if attributes_configurable a
      then res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool false)))
      else run_object_is_sealed runs0 s0 c l xs')

(** val run_object_seal :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

let rec run_object_seal runs0 s c l _foo_ = match _foo_ with
| [] ->
  if_some (run_object_heap_set_extensible false s l) (fun s0 ->
    res_ter s0 (res_val (Coq_value_object l)))
| x :: xs' ->
  if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s0 d ->
    match d with
    | Coq_full_descriptor_undef ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s0
        ("[run_object_seal]:  Undefined descriptor found in a place where it shouldn\'t.")
    | Coq_full_descriptor_some a ->
      let a' =
        if attributes_configurable a
        then let desc = { descriptor_value = None; descriptor_writable =
               None; descriptor_get = None; descriptor_set = None;
               descriptor_enumerable = None; descriptor_configurable = (Some
               false) }
             in
             attributes_update a desc
        else a
      in
      if_bool
        (object_define_own_prop runs0 s0 c l x (descriptor_of_attributes a')
          true) (fun s1 x0 -> run_object_seal runs0 s1 c l xs'))

(** val run_object_freeze :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

let rec run_object_freeze runs0 s c l _foo_ = match _foo_ with
| [] ->
  if_some (run_object_heap_set_extensible false s l) (fun s0 ->
    res_ter s0 (res_val (Coq_value_object l)))
| x :: xs' ->
  if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s0 d ->
    match d with
    | Coq_full_descriptor_undef ->
      (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
        s0
        ("[run_object_freeze]:  Undefined descriptor found in a place where it shouldn\'t.")
    | Coq_full_descriptor_some a ->
      let a' =
        if and_decidable (attributes_is_data_dec a)
             (bool_decidable (attributes_writable a))
        then let desc = { descriptor_value = None; descriptor_writable =
               (Some false); descriptor_get = None; descriptor_set = None;
               descriptor_enumerable = None; descriptor_configurable = None }
             in
             attributes_update a desc
        else a
      in
      let a'' =
        if attributes_configurable a'
        then let desc = { descriptor_value = None; descriptor_writable =
               None; descriptor_get = None; descriptor_set = None;
               descriptor_enumerable = None; descriptor_configurable = (Some
               false) }
             in
             attributes_update a' desc
        else a'
      in
      if_bool
        (object_define_own_prop runs0 s0 c l x (descriptor_of_attributes a'')
          true) (fun s1 x0 -> run_object_freeze runs0 s1 c l xs'))

(** val run_object_is_frozen :
    runs_type -> state -> execution_ctx -> object_loc -> prop_name list ->
    result **)

let rec run_object_is_frozen runs0 s c l _foo_ = match _foo_ with
| [] ->
  if_some (run_object_method object_extensible_ s l) (fun ext ->
    res_ter s (res_val (Coq_value_prim (Coq_prim_bool (neg ext)))))
| x :: xs' ->
  if_spec (runs0.runs_type_object_get_own_prop s c l x) (fun s0 d ->
    let_binding (fun a ->
      if attributes_configurable a
      then res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool false)))
      else run_object_is_frozen runs0 s0 c l xs') (fun check_configurable ->
      match d with
      | Coq_full_descriptor_undef ->
        (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
          s0
          ("[run_object_is_frozen]:  Undefined descriptor found in a place where it shouldn\'t.")
      | Coq_full_descriptor_some a ->
        (match a with
         | Coq_attributes_data_of ad ->
           if attributes_writable (Coq_attributes_data_of ad)
           then res_ter s0 (res_val (Coq_value_prim (Coq_prim_bool false)))
           else check_configurable (Coq_attributes_data_of ad)
         | Coq_attributes_accessor_of aa ->
           check_configurable (Coq_attributes_accessor_of aa))))

(** val run_get_args_for_apply :
    runs_type -> state -> execution_ctx -> object_loc -> float -> float ->
    value list specres **)

let run_get_args_for_apply runs0 s c l index n =
  if lt_int_decidable index n
  then if_string
         (to_string runs0 s c (Coq_value_prim (Coq_prim_number
           (of_int index)))) (fun s0 sindex ->
         if_value (run_object_get runs0 s0 c l sindex) (fun s1 v ->
           let_binding
             (runs0.runs_type_get_args_for_apply s1 c l (index +. 1.) n)
             (fun tail_args ->
             if_spec tail_args (fun s2 tail -> res_spec s2 (v :: tail)))))
  else res_spec s []

(** val valueToStringForJoin :
    runs_type -> state -> execution_ctx -> object_loc -> float -> string
    specres **)

let valueToStringForJoin runs0 s c l k =
  if_string
    (to_string runs0 s c (Coq_value_prim (Coq_prim_number (of_int k))))
    (fun s0 prop ->
    if_value (run_object_get runs0 s0 c l prop) (fun s1 v ->
      match v with
      | Coq_value_prim p ->
        (match p with
         | Coq_prim_undef -> res_spec s1 ""
         | Coq_prim_null -> res_spec s1 ""
         | Coq_prim_bool b ->
           if_string (to_string runs0 s1 c v) (fun s2 s3 -> res_spec s2 s3)
         | Coq_prim_number n ->
           if_string (to_string runs0 s1 c v) (fun s2 s3 -> res_spec s2 s3)
         | Coq_prim_string s2 ->
           if_string (to_string runs0 s1 c v) (fun s3 s4 -> res_spec s3 s4))
      | Coq_value_object o ->
        if_string (to_string runs0 s1 c v) (fun s2 s3 -> res_spec s2 s3)))

(** val run_array_join_elements :
    runs_type -> state -> execution_ctx -> object_loc -> float -> float ->
    string -> string -> result **)

let run_array_join_elements runs0 s c l k length0 sep sR =
  if lt_int_decidable k length0
  then let_binding (append sR sep) (fun ss ->
         let_binding (valueToStringForJoin runs0 s c l k) (fun sE ->
           if_spec sE (fun s0 element ->
             let_binding (append ss element) (fun sR0 ->
               runs0.runs_type_array_join_elements s0 c l (k +. 1.)
                 length0 sep sR0))))
  else res_ter s (res_val (Coq_value_prim (Coq_prim_string sR)))

(** val run_call_prealloc :
    runs_type -> state -> execution_ctx -> prealloc -> value -> value list ->
    result **)

let run_call_prealloc runs0 s c b vthis args =
  match b with
  | Coq_prealloc_global ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_eval ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_parse_int ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_parse_float ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_is_finite ->
    let_binding (get_arg 0 args) (fun v ->
      if_number (to_number runs0 s c v) (fun s0 n ->
        res_ter s0
          (res_val (Coq_value_prim (Coq_prim_bool
            (neg
              (or_decidable (number_comparable n JsNumber.nan)
                (or_decidable (number_comparable n JsNumber.infinity)
                  (number_comparable n JsNumber.neg_infinity)))))))))
  | Coq_prealloc_global_is_nan ->
    let_binding (get_arg 0 args) (fun v ->
      if_number (to_number runs0 s c v) (fun s0 n ->
        res_ter s0
          (res_val (Coq_value_prim (Coq_prim_bool
            (number_comparable n JsNumber.nan))))))
  | Coq_prealloc_global_decode_uri ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_decode_uri_component ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_encode_uri ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_global_encode_uri_component ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_object ->
    let_binding (get_arg 0 args) (fun value0 ->
      match value0 with
      | Coq_value_prim p ->
        (match p with
         | Coq_prim_undef -> run_construct_prealloc runs0 s c b args
         | Coq_prim_null -> run_construct_prealloc runs0 s c b args
         | Coq_prim_bool b0 -> to_object s value0
         | Coq_prim_number n -> to_object s value0
         | Coq_prim_string s0 -> to_object s value0)
      | Coq_value_object o -> to_object s value0)
  | Coq_prealloc_object_get_proto_of ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (run_object_method object_proto_ s l) (fun proto ->
          res_ter s (res_val proto)))
  | Coq_prealloc_object_get_own_prop_descriptor ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_string (to_string runs0 s c (get_arg 1 args))
          (fun s1 x ->
          if_spec (runs0.runs_type_object_get_own_prop s1 c l x) (fun s2 d ->
            from_prop_descriptor runs0 s2 c d)))
  | Coq_prealloc_object_get_own_prop_name ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_object_create ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_object_define_prop ->
    let_binding (get_arg 0 args) (fun o ->
      let_binding (get_arg 1 args) (fun p ->
        let_binding (get_arg 2 args)
          (fun attr ->
          match o with
          | Coq_value_prim p0 -> run_error s Coq_native_error_type
          | Coq_value_object l ->
            if_string (to_string runs0 s c p) (fun s1 name ->
              if_spec (run_to_descriptor runs0 s1 c attr) (fun s2 desc ->
                if_bool (object_define_own_prop runs0 s2 c l name desc true)
                  (fun s3 x -> res_ter s3 (res_val (Coq_value_object l))))))))
  | Coq_prealloc_object_define_props ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_object_seal ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (object_properties_keys_as_list_pickable_option s l)
          (run_object_seal runs0 s c l))
  | Coq_prealloc_object_freeze ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (object_properties_keys_as_list_pickable_option s l)
          (run_object_freeze runs0 s c l))
  | Coq_prealloc_object_prevent_extensions ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (object_binds_pickable_option s l) (fun o ->
          let o1 = object_with_extension o false in
          let s' = object_write s l o1 in
          res_ter s' (res_val (Coq_value_object l))))
  | Coq_prealloc_object_is_sealed ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (object_properties_keys_as_list_pickable_option s l)
          (run_object_is_sealed runs0 s c l))
  | Coq_prealloc_object_is_frozen ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (object_properties_keys_as_list_pickable_option s l)
          (run_object_is_frozen runs0 s c l))
  | Coq_prealloc_object_is_extensible ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p -> run_error s Coq_native_error_type
      | Coq_value_object l ->
        if_some (run_object_method object_extensible_ s l) (fun r ->
          res_ter s (res_val (Coq_value_prim (Coq_prim_bool r)))))
  | Coq_prealloc_object_keys ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_object_keys_call ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_object_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
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
          if_object (to_object s vthis) (fun s1 l ->
            if_some (run_object_method object_class_ s1 l) (fun s0 ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_string
                  (append
                    ("[object ")
                    (append s0 ("]"))))))))
        | Coq_prim_number n ->
          if_object (to_object s vthis) (fun s1 l ->
            if_some (run_object_method object_class_ s1 l) (fun s0 ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_string
                  (append
                    ("[object ")
                    (append s0 ("]"))))))))
        | Coq_prim_string s0 ->
          if_object (to_object s vthis) (fun s1 l ->
            if_some (run_object_method object_class_ s1 l) (fun s2 ->
              res_ter s1
                (res_val (Coq_value_prim (Coq_prim_string
                  (append
                    ("[object ")
                    (append s2 ("]")))))))))
     | Coq_value_object o ->
       if_object (to_object s vthis) (fun s1 l ->
         if_some (run_object_method object_class_ s1 l) (fun s0 ->
           res_ter s1
             (res_val (Coq_value_prim (Coq_prim_string
               (append
                 ("[object ")
                 (append s0 ("]")))))))))
  | Coq_prealloc_object_proto_value_of -> to_object s vthis
  | Coq_prealloc_object_proto_has_own_prop ->
    let_binding (get_arg 0 args) (fun v ->
      if_string (to_string runs0 s c v) (fun s1 x ->
        if_object (to_object s1 vthis) (fun s2 l ->
          if_spec (runs0.runs_type_object_get_own_prop s2 c l x) (fun s3 d ->
            match d with
            | Coq_full_descriptor_undef ->
              res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool false)))
            | Coq_full_descriptor_some a ->
              res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool true)))))))
  | Coq_prealloc_object_proto_is_prototype_of ->
    let_binding (get_arg 0 args) (fun v ->
      match v with
      | Coq_value_prim p ->
        result_out (Coq_out_ter (s,
          (res_val (Coq_value_prim (Coq_prim_bool false)))))
      | Coq_value_object l ->
        if_object (to_object s vthis) (fun s1 lo ->
          runs0.runs_type_object_proto_is_prototype_of s1 lo l))
  | Coq_prealloc_object_proto_prop_is_enumerable ->
    let_binding (get_arg 0 args) (fun v ->
      if_string (to_string runs0 s c v) (fun s1 x ->
        if_object (to_object s1 vthis) (fun s2 l ->
          if_spec (runs0.runs_type_object_get_own_prop s2 c l x) (fun s3 d ->
            match d with
            | Coq_full_descriptor_undef ->
              res_ter s3 (res_val (Coq_value_prim (Coq_prim_bool false)))
            | Coq_full_descriptor_some a ->
              res_ter s3
                (res_val (Coq_value_prim (Coq_prim_bool
                  (attributes_enumerable a))))))))
  | Coq_prealloc_function ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_function_proto ->
    result_out (Coq_out_ter (s, (res_val (Coq_value_prim Coq_prim_undef))))
  | Coq_prealloc_function_proto_to_string ->
    if is_callable_dec s vthis
    then (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
           ("Function.prototype.toString() is implementation dependent.")
    else run_error s Coq_native_error_type
  | Coq_prealloc_function_proto_apply ->
    let_binding (get_arg 0 args) (fun thisArg ->
      let_binding (get_arg 1 args) (fun argArray ->
        if is_callable_dec s vthis
        then (match vthis with
              | Coq_value_prim p ->
                (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                  s
                  ("Value is callable, but isn\'t an object.")
              | Coq_value_object thisobj ->
                (match argArray with
                 | Coq_value_prim p ->
                   (match p with
                    | Coq_prim_undef ->
                      runs0.runs_type_call s c thisobj thisArg []
                    | Coq_prim_null ->
                      runs0.runs_type_call s c thisobj thisArg []
                    | Coq_prim_bool b0 -> run_error s Coq_native_error_type
                    | Coq_prim_number n -> run_error s Coq_native_error_type
                    | Coq_prim_string s0 -> run_error s Coq_native_error_type)
                 | Coq_value_object array ->
                   if_value
                     (run_object_get runs0 s c array
                       ("length"))
                     (fun s0 v ->
                     if_spec (to_uint32 runs0 s0 c v) (fun s1 ilen ->
                       if_spec
                         (run_get_args_for_apply runs0 s1 c array 0. ilen)
                         (fun s2 arguments ->
                         runs0.runs_type_call s2 c thisobj thisArg arguments)))))
        else run_error s Coq_native_error_type))
  | Coq_prealloc_function_proto_call ->
    if is_callable_dec s vthis
    then (match vthis with
          | Coq_value_prim p ->
            (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
              s
              ("Value is callable, but isn\'t an object.")
          | Coq_value_object thisobj ->
            let (thisArg, a) = get_arg_first_and_rest args in
            runs0.runs_type_call s c thisobj thisArg a)
    else run_error s Coq_native_error_type
  | Coq_prealloc_function_proto_bind ->
    if is_callable_dec s vthis
    then (match vthis with
          | Coq_value_prim p ->
            (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
              s
              ("Value is callable, but isn\'t an object.")
          | Coq_value_object thisobj ->
            let (vthisArg, a) = get_arg_first_and_rest args in
            let_binding
              (object_new (Coq_value_object (Coq_object_loc_prealloc
                Coq_prealloc_object_proto))
                ("Object")) (fun o1 ->
              let_binding (object_with_get o1 Coq_builtin_get_function)
                (fun o2 ->
                let_binding
                  (object_with_details o2 None None None (Some thisobj) (Some
                    vthisArg) (Some a) None) (fun o3 ->
                  let_binding
                    (object_set_class o3
                      ("Function"))
                    (fun o4 ->
                    let_binding
                      (object_set_proto o4 (Coq_value_object
                        (Coq_object_loc_prealloc
                        Coq_prealloc_function_proto))) (fun o5 ->
                      let_binding
                        (object_with_invokation o5 (Some
                          Coq_construct_after_bind) (Some
                          Coq_call_after_bind) (Some
                          Coq_builtin_has_instance_after_bind)) (fun o6 ->
                        let_binding (object_set_extensible o6 true)
                          (fun o7 ->
                          let (l, s') = object_alloc s o7 in
                          let_binding
                            (if_some
                              (run_object_method object_class_ s' thisobj)
                              (fun class0 ->
                              if string_comparable class0
                                   ("Function")
                              then if_number
                                     (run_object_get runs0 s' c thisobj
                                       ("length"))
                                     (fun s'0 n ->
                                     if_spec
                                       (to_int32 runs0 s'0 c (Coq_value_prim
                                         (Coq_prim_number n)))
                                       (fun s'1 ilen ->
                                       if lt_int_decidable ilen
                                            (number_of_int (LibList.length a))
                                       then res_spec s'1 0.
                                       else res_spec s'1
                                              (ilen -.
                                                (number_of_int (LibList.length a)))))
                              else res_spec s' 0.)) (fun vlength ->
                            if_spec vlength (fun s'0 length0 ->
                              let_binding { attributes_data_value =
                                (Coq_value_prim (Coq_prim_number
                                (of_int length0)));
                                attributes_data_writable = false;
                                attributes_data_enumerable = false;
                                attributes_data_configurable = false }
                                (fun a0 ->
                                if_some
                                  (object_heap_map_properties_pickable_option
                                    s'0 l (fun p ->
                                    Heap.write p
                                      ("length")
                                      (Coq_attributes_data_of a0)))
                                  (fun s'1 ->
                                  let_binding (Coq_value_object
                                    (Coq_object_loc_prealloc
                                    Coq_prealloc_throw_type_error))
                                    (fun vthrower ->
                                    let_binding { attributes_accessor_get =
                                      vthrower; attributes_accessor_set =
                                      vthrower;
                                      attributes_accessor_enumerable = false;
                                      attributes_accessor_configurable =
                                      false } (fun a1 ->
                                      if_bool
                                        (object_define_own_prop runs0 s'1 c l
                                          ("caller")
                                          (descriptor_of_attributes
                                            (Coq_attributes_accessor_of a1))
                                          false) (fun s'2 x ->
                                        if_bool
                                          (object_define_own_prop runs0 s'2 c
                                            l
                                            ("arguments")
                                            (descriptor_of_attributes
                                              (Coq_attributes_accessor_of
                                              a1)) false) (fun s'3 x0 ->
                                          res_ter s'3
                                            (res_val (Coq_value_object l))))))))))))))))))
    else run_error s Coq_native_error_type
  | Coq_prealloc_bool ->
    result_out
      (let_binding (get_arg 0 args) (fun v -> Coq_out_ter (s,
        (res_val (Coq_value_prim (Coq_prim_bool
          (convert_value_to_boolean v)))))))
  | Coq_prealloc_bool_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
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
       if_some_or_default (run_object_method object_class_ s l)
         (run_error s Coq_native_error_type) (fun s0 ->
         if string_comparable s0
              ("Boolean")
         then if_some_or_default (run_object_method object_prim_value_ s l)
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
       if_some_or_default (run_object_method object_class_ s l)
         (run_error s Coq_native_error_type) (fun s0 ->
         if string_comparable s0
              ("Boolean")
         then if_some_or_default (run_object_method object_prim_value_ s l)
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
    else let v = get_arg 0 args in to_number runs0 s c v
  | Coq_prealloc_number_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_number_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
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
       if_some_or_default (run_object_method object_class_ s l)
         (run_error s Coq_native_error_type) (fun s0 ->
         if string_comparable s0 ("Number")
         then if_some_or_default (run_object_method object_prim_value_ s l)
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
  | Coq_prealloc_number_proto_to_fixed ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_number_proto_to_exponential ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_number_proto_to_precision ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_array ->
    run_construct_prealloc runs0 s c Coq_prealloc_array args
  | Coq_prealloc_array_is_array ->
    let_binding (get_arg 0 args) (fun arg ->
      match arg with
      | Coq_value_prim p ->
        res_ter s (res_val (Coq_value_prim (Coq_prim_bool false)))
      | Coq_value_object arg0 ->
        if_some (run_object_method object_class_ s arg0) (fun class0 ->
          if string_comparable class0 ("Array")
          then res_ter s (res_val (Coq_value_prim (Coq_prim_bool true)))
          else res_ter s (res_val (Coq_value_prim (Coq_prim_bool false)))))
  | Coq_prealloc_array_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_array_proto_to_string ->
    if_object (to_object s vthis) (fun s0 array ->
      if_value
        (run_object_get runs0 s0 c array ("join"))
        (fun s1 vfunc ->
        if is_callable_dec s1 vfunc
        then (match vfunc with
              | Coq_value_prim p ->
                (fun s message ->
  print_endline (__LOC__ ^ ": Stuck!\nState:  " ^ Prheap.prstate true s
    ^ "\nMessage:\t" ^ Prheap.string_of_char_list message) ;
  Coq_result_impossible)
                  s1
                  ("Value is callable, but isn\'t an object.")
              | Coq_value_object func ->
                runs0.runs_type_call s1 c func (Coq_value_object array) [])
        else runs0.runs_type_call_prealloc s1 c
               Coq_prealloc_object_proto_to_string (Coq_value_object array)
               []))
  | Coq_prealloc_array_proto_join ->
    let_binding (get_arg 0 args) (fun vsep ->
      if_object (to_object s vthis) (fun s0 l ->
        if_value
          (run_object_get runs0 s0 c l
            ("length")) (fun s1 vlen ->
          if_spec (to_uint32 runs0 s1 c vlen) (fun s2 ilen ->
            let_binding
              (if not_decidable
                    (value_comparable vsep (Coq_value_prim Coq_prim_undef))
               then vsep
               else Coq_value_prim (Coq_prim_string (","))) (fun rsep ->
              if_string (to_string runs0 s2 c rsep) (fun s3 sep ->
                if float_eq ilen 0.0
                then res_ter s3
                       (res_val (Coq_value_prim (Coq_prim_string "")))
                else let_binding (valueToStringForJoin runs0 s3 c l 0.)
                       (fun sR ->
                       if_spec sR (fun s4 sR0 ->
                         run_array_join_elements runs0 s4 c l 1. ilen sep sR0))))))))
  | Coq_prealloc_array_proto_pop ->
    if_object (to_object s vthis) (fun s0 l ->
      if_value
        (run_object_get runs0 s0 c l
          ("length")) (fun s1 vlen ->
        if_spec (to_uint32 runs0 s1 c vlen) (fun s2 ilen ->
          if float_eq ilen 0.0
          then if_not_throw
                 (object_put runs0 s2 c l
                   ("length")
                   (Coq_value_prim (Coq_prim_number JsNumber.zero)) throw_true)
                 (fun s3 x ->
                 result_out (Coq_out_ter (s3,
                   (res_val (Coq_value_prim Coq_prim_undef)))))
          else if_string
                 (to_string runs0 s2 c (Coq_value_prim (Coq_prim_number
                   (of_int (ilen -. 1.))))) (fun s3 sindx ->
                 if_value (run_object_get runs0 s3 c l sindx)
                   (fun s4 velem ->
                   if_not_throw
                     (object_delete_default runs0 s4 c l sindx throw_true)
                     (fun s5 x ->
                     if_not_throw
                       (object_put runs0 s5 c l
                         ("length")
                         (Coq_value_prim (Coq_prim_string sindx)) throw_true)
                       (fun s6 x0 ->
                       result_out (Coq_out_ter (s6, (res_val velem))))))))))
  | Coq_prealloc_array_proto_push ->
    if_object (to_object s vthis) (fun s0 l ->
      if_value
        (run_object_get runs0 s0 c l
          ("length")) (fun s1 vlen ->
        if_spec (to_uint32 runs0 s1 c vlen) (fun s2 ilen ->
          push runs0 s2 c l args ilen)))
  | Coq_prealloc_string ->
    if list_eq_nil_decidable args
    then res_ter s (res_val (Coq_value_prim (Coq_prim_string "")))
    else let_binding (get_arg 0 args) (fun value0 ->
           if_string (to_string runs0 s c value0) (fun s0 s1 ->
             res_ter s0 (res_val (Coq_value_prim (Coq_prim_string s1)))))
  | Coq_prealloc_string_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_string_proto_to_string ->
    (match vthis with
     | Coq_value_prim p ->
       if type_comparable (type_of vthis) Coq_type_string
       then res_ter s (res_val vthis)
       else run_error s Coq_native_error_type
     | Coq_value_object l ->
       if_some (run_object_method object_class_ s l) (fun s0 ->
         if string_comparable s0 ("String")
         then run_object_prim_value s l
         else run_error s Coq_native_error_type))
  | Coq_prealloc_string_proto_value_of ->
    (match vthis with
     | Coq_value_prim p ->
       if type_comparable (type_of vthis) Coq_type_string
       then res_ter s (res_val vthis)
       else run_error s Coq_native_error_type
     | Coq_value_object l ->
       if_some (run_object_method object_class_ s l) (fun s0 ->
         if string_comparable s0 ("String")
         then run_object_prim_value s l
         else run_error s Coq_native_error_type))
  | Coq_prealloc_string_proto_char_at ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_string_proto_char_code_at ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_math ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_mathop m ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_date ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_regexp ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_error ->
    let_binding (get_arg 0 args) (fun v ->
      build_error s (Coq_value_object (Coq_object_loc_prealloc
        Coq_prealloc_error_proto)) v)
  | Coq_prealloc_error_proto ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_native_error ne ->
    let_binding (get_arg 0 args) (fun v ->
      build_error s (Coq_value_object (Coq_object_loc_prealloc
        (Coq_prealloc_native_error_proto ne))) v)
  | Coq_prealloc_native_error_proto n ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_error_proto_to_string ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))
  | Coq_prealloc_throw_type_error -> run_error s Coq_native_error_type
  | Coq_prealloc_json ->
    (fun s ->
  print_endline (__LOC__ ^ ": Not implemented because: " ^ Prheap.string_of_char_list s) ;
  Coq_result_not_yet_implemented)
      (append
        ("Call prealloc_")
        (append (string_of_prealloc b)
          (" not yet implemented")))

(** val run_call :
    runs_type -> state -> execution_ctx -> object_loc -> value -> value list
    -> result **)

let run_call runs0 s c l vthis args =
  if_some (run_object_method object_call_ s l) (fun co ->
    if_some co (fun c0 ->
      match c0 with
      | Coq_call_default -> entering_func_code runs0 s c l vthis args
      | Coq_call_after_bind ->
        if_some (run_object_method object_bound_args_ s l) (fun oarg ->
          if_some oarg (fun boundArgs ->
            if_some (run_object_method object_bound_this_ s l) (fun obnd ->
              if_some obnd (fun boundThis ->
                if_some (run_object_method object_target_function_ s l)
                  (fun otrg ->
                  if_some otrg (fun target ->
                    let_binding (LibList.append boundArgs args)
                      (fun arguments ->
                      runs0.runs_type_call s c target boundThis arguments)))))))
      | Coq_call_prealloc b -> run_call_prealloc runs0 s c b vthis args))

(** val run_javascript : runs_type -> prog -> result **)

let run_javascript runs0 p =
  let p' = add_infos_prog strictness_false p in
  let c = execution_ctx_initial (prog_intro_strictness p') in
  if_void
    (execution_ctx_binding_inst runs0 state_initial c Coq_codetype_global
      None p' []) (fun s' -> runs0.runs_type_prog s' c p')

(** val runs : int -> runs_type **)

let rec runs =
  { runs_type_expr = (fun x y z -> run_expr runs x y z);
    runs_type_stat = (fun x y z -> run_stat runs x y z);
    runs_type_prog = (fun x y z -> run_prog runs x y z);
    runs_type_call = (fun x y z r t -> run_call runs x y z r t);
    runs_type_call_prealloc = (fun x y z r t -> run_call_prealloc runs x y z r t);
    runs_type_construct = (fun x y z r t -> run_construct runs x y z r t);
    runs_type_function_has_instance = (fun x y z -> run_function_has_instance runs x y z);
    runs_type_object_has_instance = (fun x y z r t -> run_object_has_instance runs x y z r t);
    runs_type_get_args_for_apply = (fun x y z r t -> run_get_args_for_apply runs x y z r t);
    runs_type_stat_while = (fun x y z r t s -> run_stat_while runs x y z r t s);
    runs_type_stat_do_while = (fun x y z r t s -> run_stat_do_while runs x y z r t s);
    runs_type_stat_for_loop = (fun x y z r t s v -> run_stat_for_loop runs x y z r t s v);
    runs_type_object_delete = (fun x y z r t -> object_delete runs x y z r t);
    runs_type_object_get_own_prop = (fun x y z r -> run_object_get_own_prop runs x y z r);
    runs_type_object_get_prop = (fun x y z r -> run_object_get_prop runs x y z r);
    runs_type_object_get = (fun x y z r -> run_object_get runs x y z r);
    runs_type_object_proto_is_prototype_of = (fun x y z -> object_proto_is_prototype_of runs x y z);
    runs_type_object_put = (fun x y z r s t -> object_put runs x y z r s t);
    runs_type_equal = (fun x y z r -> run_equal runs x y z r);
    runs_type_to_integer = (fun x y z -> to_integer runs x y z);
    runs_type_to_string = (fun x y z -> to_string runs x y z);
    runs_type_array_join_elements = (fun x y z r s t v -> run_array_join_elements runs x y z r s t v);
    runs_type_array_element_list = (fun x y z r s -> run_array_element_list runs x y z r s);
    runs_type_object_define_own_prop_array_loop = (fun x y z r s t u v w -> run_object_define_own_prop_array_loop runs x y z r s t u v w) }
