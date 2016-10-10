
open Datatypes
(*open JsNumber*)
open JsSyntax
open JsSyntaxAux
open LibList
open LibOption
open List0
open Shared

(** val res_overwrite_value_if_empty : resvalue -> res -> res **)

let res_overwrite_value_if_empty rv r =
  if resvalue_compare r.res_value Coq_resvalue_empty
  then res_with_value r rv
  else r

(** val res_label_in : res -> label_set -> bool **)

let res_label_in r labs =
  label_set_mem r.res_label labs

(** val convert_literal_to_prim : literal -> prim **)

let convert_literal_to_prim _foo_ = match _foo_ with
| Coq_literal_null -> Coq_prim_null
| Coq_literal_bool b -> Coq_prim_bool b
| Coq_literal_number n -> Coq_prim_number n
| Coq_literal_string s -> Coq_prim_string s

(** val type_of_prim : prim -> coq_type **)

let type_of_prim _foo_ = match _foo_ with
| Coq_prim_undef -> Coq_type_undef
| Coq_prim_null -> Coq_type_null
| Coq_prim_bool b -> Coq_type_bool
| Coq_prim_number n -> Coq_type_number
| Coq_prim_string s -> Coq_type_string

(** val type_of : value -> coq_type **)

let type_of _foo_ = match _foo_ with
| Coq_value_prim w -> type_of_prim w
| Coq_value_object o -> Coq_type_object

(** val attributes_data_default : attributes_data **)

let attributes_data_default =
  { attributes_data_value = (Coq_value_prim Coq_prim_undef);
    attributes_data_writable = false; attributes_data_enumerable = false;
    attributes_data_configurable = false }

(** val attributes_accessor_default : attributes_accessor **)

let attributes_accessor_default =
  { attributes_accessor_get = (Coq_value_prim Coq_prim_undef);
    attributes_accessor_set = (Coq_value_prim Coq_prim_undef);
    attributes_accessor_enumerable = false;
    attributes_accessor_configurable = false }

(** val attributes_accessor_of_attributes_data :
    attributes_data -> attributes_accessor **)

let attributes_accessor_of_attributes_data ad =
  { attributes_accessor_get =
    attributes_accessor_default.attributes_accessor_get;
    attributes_accessor_set =
    attributes_accessor_default.attributes_accessor_set;
    attributes_accessor_enumerable = ad.attributes_data_enumerable;
    attributes_accessor_configurable = ad.attributes_data_configurable }

(** val attributes_data_of_attributes_accessor :
    attributes_accessor -> attributes_data **)

let attributes_data_of_attributes_accessor aa =
  { attributes_data_value = attributes_data_default.attributes_data_value;
    attributes_data_writable =
    attributes_data_default.attributes_data_writable;
    attributes_data_enumerable = aa.attributes_accessor_enumerable;
    attributes_data_configurable = aa.attributes_accessor_configurable }

(** val attributes_data_update :
    attributes_data -> descriptor -> attributes_data **)

let attributes_data_update ad desc =
  { attributes_data_value =
    (unsome_default ad.attributes_data_value desc.descriptor_value);
    attributes_data_writable =
    (unsome_default ad.attributes_data_writable desc.descriptor_writable);
    attributes_data_enumerable =
    (unsome_default ad.attributes_data_enumerable desc.descriptor_enumerable);
    attributes_data_configurable =
    (unsome_default ad.attributes_data_configurable
      desc.descriptor_configurable) }

(** val attributes_accessor_update :
    attributes_accessor -> descriptor -> attributes_accessor **)

let attributes_accessor_update aa desc =
  { attributes_accessor_get =
    (unsome_default aa.attributes_accessor_get desc.descriptor_get);
    attributes_accessor_set =
    (unsome_default aa.attributes_accessor_set desc.descriptor_set);
    attributes_accessor_enumerable =
    (unsome_default aa.attributes_accessor_enumerable
      desc.descriptor_enumerable); attributes_accessor_configurable =
    (unsome_default aa.attributes_accessor_configurable
      desc.descriptor_configurable) }

(** val attributes_update : attributes -> descriptor -> attributes **)

let attributes_update a desc =
  match a with
  | Coq_attributes_data_of ad ->
    Coq_attributes_data_of (attributes_data_update ad desc)
  | Coq_attributes_accessor_of aa ->
    Coq_attributes_accessor_of (attributes_accessor_update aa desc)

(** val attributes_data_of_descriptor : descriptor -> attributes_data **)

let attributes_data_of_descriptor desc =
  attributes_data_update attributes_data_default desc

(** val attributes_accessor_of_descriptor :
    descriptor -> attributes_accessor **)

let attributes_accessor_of_descriptor desc =
  attributes_accessor_update attributes_accessor_default desc

(** val descriptor_of_attributes : attributes -> descriptor **)

let descriptor_of_attributes _foo_ = match _foo_ with
| Coq_attributes_data_of ad ->
  { descriptor_value = (Some ad.attributes_data_value); descriptor_writable =
    (Some ad.attributes_data_writable); descriptor_get = None;
    descriptor_set = None; descriptor_enumerable = (Some
    ad.attributes_data_enumerable); descriptor_configurable = (Some
    ad.attributes_data_configurable) }
| Coq_attributes_accessor_of aa ->
  { descriptor_value = None; descriptor_writable = None; descriptor_get =
    (Some aa.attributes_accessor_get); descriptor_set = (Some
    aa.attributes_accessor_set); descriptor_enumerable = (Some
    aa.attributes_accessor_enumerable); descriptor_configurable = (Some
    aa.attributes_accessor_configurable) }

(** val attributes_configurable : attributes -> bool **)

let attributes_configurable _foo_ = match _foo_ with
| Coq_attributes_data_of ad -> ad.attributes_data_configurable
| Coq_attributes_accessor_of aa -> aa.attributes_accessor_configurable

(** val attributes_enumerable : attributes -> bool **)

let attributes_enumerable _foo_ = match _foo_ with
| Coq_attributes_data_of ad -> ad.attributes_data_enumerable
| Coq_attributes_accessor_of aa -> aa.attributes_accessor_enumerable

(** val state_with_object_heap :
    state -> (object_loc, coq_object) Heap.heap -> state **)

(* STATEFUL *)
let state_with_object_heap s new_object_heap =
  { s with state_object_heap = new_object_heap } 

(** val state_map_object_heap :
    state -> ((object_loc, coq_object) Heap.heap -> (object_loc, coq_object)
    Heap.heap) -> state **)

(* STATEFUL *)
let state_map_object_heap s f =
  state_with_object_heap s (f s.state_object_heap)

(** val object_write : state -> object_loc -> coq_object -> state **)

(* STATEFUL *)
let object_write s l o =
  state_map_object_heap s (fun h -> HeapObj.write h l o)

(** val object_alloc : state -> coq_object -> object_loc * state **)

(* STATEFUL *)
let object_alloc s o =
  let { state_object_heap = cells; state_env_record_heap = bindings;
    state_fresh_locations = state_fresh_locations0; } = s
  in
  let n = state_fresh_locations0 in
  let alloc = state_fresh_locations0 + 1 in
  let l = Coq_object_loc_normal n in
  (l,
  (object_write { state_object_heap = cells; state_env_record_heap =
    bindings; state_fresh_locations = alloc } l
    o))

(** val object_map_properties :
    coq_object -> (object_properties_type -> object_properties_type) ->
    coq_object **)

let object_map_properties o f =
  object_with_properties o (f o.object_properties_)

(** val object_new : value -> class_name -> coq_object **)

let object_new vproto sclass =
  object_create vproto sclass true Heap.empty

(** val attributes_writable : attributes -> bool **)

let attributes_writable _foo_ = match _foo_ with
| Coq_attributes_data_of ad -> ad.attributes_data_writable
| Coq_attributes_accessor_of aa -> false

(** val attributes_data_intro_constant : value -> attributes_data **)

let attributes_data_intro_constant v =
  { attributes_data_value = v; attributes_data_writable = false;
    attributes_data_enumerable = false; attributes_data_configurable =
    false }

(** val attributes_data_intro_all_true : value -> attributes_data **)

let attributes_data_intro_all_true v =
  { attributes_data_value = v; attributes_data_writable = true;
    attributes_data_enumerable = true; attributes_data_configurable = true }

(** val descriptor_intro_data :
    value -> bool -> bool -> bool -> descriptor **)

let descriptor_intro_data v bw be bc =
  { descriptor_value = (Some v); descriptor_writable = (Some bw);
    descriptor_get = None; descriptor_set = None; descriptor_enumerable =
    (Some be); descriptor_configurable = (Some bc) }

(** val descriptor_intro_empty : descriptor **)

let descriptor_intro_empty =
  { descriptor_value = None; descriptor_writable = None; descriptor_get =
    None; descriptor_set = None; descriptor_enumerable = None;
    descriptor_configurable = None }

type ref_kind =
| Coq_ref_kind_null [@f]  (* Auto Generated Attributes *)
| Coq_ref_kind_undef [@f]  (* Auto Generated Attributes *)
| Coq_ref_kind_primitive_base [@f]  (* Auto Generated Attributes *)
| Coq_ref_kind_object [@f]  (* Auto Generated Attributes *)
| Coq_ref_kind_env_record [@f]  (* Auto Generated Attributes *)

(** val ref_kind_of : ref -> ref_kind **)

let ref_kind_of r =
  match r.ref_base with
  | Coq_ref_base_type_value v ->
    (match v with
     | Coq_value_prim w ->
       (match w with
        | Coq_prim_undef -> Coq_ref_kind_undef
        | Coq_prim_null -> Coq_ref_kind_null
        | Coq_prim_bool b -> Coq_ref_kind_primitive_base
        | Coq_prim_number n -> Coq_ref_kind_primitive_base
        | Coq_prim_string s -> Coq_ref_kind_primitive_base)
     | Coq_value_object o -> Coq_ref_kind_object)
  | Coq_ref_base_type_env_loc l -> Coq_ref_kind_env_record

(** val ref_create_value : value -> prop_name -> bool -> ref **)

let ref_create_value v x strict =
  { ref_base = (Coq_ref_base_type_value v); ref_name = x; ref_strict =
    strict }

(** val ref_create_env_loc : env_loc -> prop_name -> bool -> ref **)

let ref_create_env_loc l x strict =
  { ref_base = (Coq_ref_base_type_env_loc l); ref_name = x; ref_strict =
    strict }

(** val mutability_of_bool : bool -> mutability **)

let mutability_of_bool _foo_ = match _foo_ with
| true -> Coq_mutability_deletable
| false -> Coq_mutability_nondeletable

(** val state_with_env_record_heap :
    state -> (env_loc, env_record) Heap.heap -> state **)

(* STATEFUL *)
let state_with_env_record_heap s new_env_heap =
  let { state_object_heap = object_heap; state_env_record_heap =
    old_env_heap; state_fresh_locations = fresh_locs; } = s
  in
  { state_object_heap = object_heap; state_env_record_heap = new_env_heap;
  state_fresh_locations = fresh_locs }

(** val state_map_env_record_heap :
    state -> ((env_loc, env_record) Heap.heap -> (env_loc, env_record)
    Heap.heap) -> state **)

(* STATEFUL *)
let state_map_env_record_heap s f =
  state_with_env_record_heap s (f s.state_env_record_heap)

(** val env_record_write : state -> env_loc -> env_record -> state **)

(* STATEFUL *)
let env_record_write s l e =
  state_map_env_record_heap s (fun h -> HeapInt.write h l e)

(** val env_record_alloc : state -> env_record -> int * state **)

(* STATEFUL *)
let env_record_alloc s e =
  let { state_object_heap = cells; state_env_record_heap = bindings;
    state_fresh_locations = state_fresh_locations0;  } = s
  in
  let l =  state_fresh_locations0 in
  let alloc = state_fresh_locations0 + 1 in
  let bindings' = HeapInt.write bindings l e in
  (l, { state_object_heap = cells; state_env_record_heap = bindings';
  state_fresh_locations = alloc })

(** val provide_this_true : provide_this_flag **)

let provide_this_true =
  true

(** val provide_this_false : provide_this_flag **)

let provide_this_false =
  false

(** val env_record_object_default : object_loc -> env_record **)

let env_record_object_default l =
  Coq_env_record_object (l, provide_this_false)

(** val decl_env_record_empty : decl_env_record **)

let decl_env_record_empty =
  Heap.empty

(** val decl_env_record_write :
    decl_env_record -> prop_name -> mutability -> value -> decl_env_record **)

let decl_env_record_write ed x mu v =
  HeapStr.write ed x (mu, v)

(** val decl_env_record_rem :
    decl_env_record -> prop_name -> decl_env_record **)

let decl_env_record_rem ed x =
  HeapStr.rem ed x

(** val env_record_write_decl_env :
    state -> env_loc -> prop_name -> mutability -> value -> state **)

(* STATEFUL *)
let env_record_write_decl_env s l x mu v =
  match HeapInt.read s.state_env_record_heap l with
  | Coq_env_record_decl ed ->
    let env' = decl_env_record_write ed x mu v in
    env_record_write s l (Coq_env_record_decl env')
  | Coq_env_record_object (o, p) -> s

(** val lexical_env_alloc :
    state -> int list -> env_record -> int list * state **)

(* STATEFUL *)
let lexical_env_alloc s lex e =
  let (l, s') = env_record_alloc s e in let lex' = l :: lex in (lex', s')

(** val lexical_env_alloc_decl : state -> int list -> int list * state **)

(* STATEFUL *)
let lexical_env_alloc_decl s lex =
  lexical_env_alloc s lex (Coq_env_record_decl decl_env_record_empty)

(** val lexical_env_alloc_object :
    state -> int list -> object_loc -> provide_this_flag -> int list * state **)

(* STATEFUL *)
let lexical_env_alloc_object s lex l pt =
  lexical_env_alloc s lex (Coq_env_record_object (l, pt))

(** val execution_ctx_intro_same :
    lexical_env -> value -> strictness_flag -> execution_ctx **)

let execution_ctx_intro_same x lthis strict =
  { execution_ctx_lexical_env = x; execution_ctx_variable_env = x;
    execution_ctx_this_binding = lthis; execution_ctx_strict = strict }

(** val execution_ctx_with_lex :
    execution_ctx -> lexical_env -> execution_ctx **)

let execution_ctx_with_lex c lex =
  let { execution_ctx_lexical_env = x1; execution_ctx_variable_env = x2;
    execution_ctx_this_binding = x3; execution_ctx_strict = x4 } = c
  in
  { execution_ctx_lexical_env = lex; execution_ctx_variable_env = x2;
  execution_ctx_this_binding = x3; execution_ctx_strict = x4 }

(** val execution_ctx_with_lex_same :
    execution_ctx -> lexical_env -> execution_ctx **)

let execution_ctx_with_lex_same c lex =
  let { execution_ctx_lexical_env = x1; execution_ctx_variable_env = x2;
    execution_ctx_this_binding = x3; execution_ctx_strict = x4 } = c
  in
  { execution_ctx_lexical_env = lex; execution_ctx_variable_env = lex;
  execution_ctx_this_binding = x3; execution_ctx_strict = x4 }

(** val lexical_env_initial : lexical_env **)

let lexical_env_initial =
  env_loc_global_env_record :: []

(** val execution_ctx_initial : strictness_flag -> execution_ctx **)

let execution_ctx_initial str =
  { execution_ctx_lexical_env = lexical_env_initial;
    execution_ctx_variable_env = lexical_env_initial;
    execution_ctx_this_binding = (Coq_value_object (Coq_object_loc_prealloc
    Coq_prealloc_global)); execution_ctx_strict = str }

(** val element_funcdecl : element -> funcdecl list **)

let element_funcdecl _foo_ = match _foo_ with
| Coq_element_stat s -> []
| Coq_element_func_decl (name, args, bd) ->
  { funcdecl_name = name; funcdecl_parameters = args; funcdecl_body =
    bd } :: []

(** val prog_funcdecl : prog -> funcdecl list **)

let prog_funcdecl p =
  concat (LibList.map element_funcdecl (prog_elements p))

(** val stat_vardecl : stat -> string list **)

let rec stat_vardecl _foo_ = match _foo_ with
| Coq_stat_expr e -> []
| Coq_stat_label (s0, s) -> stat_vardecl s
| Coq_stat_block ts -> concat (map stat_vardecl ts)
| Coq_stat_var_decl nes -> LibList.map fst nes
| Coq_stat_if (e, s1, s2o) ->
  append (stat_vardecl s1)
    (unsome_default [] (LibOption.map stat_vardecl s2o))
| Coq_stat_do_while (l, s, e) -> stat_vardecl s
| Coq_stat_while (l, e, s) -> stat_vardecl s
| Coq_stat_with (e, s) -> stat_vardecl s
| Coq_stat_throw e -> []
| Coq_stat_return o -> []
| Coq_stat_break l -> []
| Coq_stat_continue l -> []
| Coq_stat_try (s, sco, sfo) ->
  append (stat_vardecl s)
    (append
      (unsome_default []
        (LibOption.map (fun sc -> stat_vardecl (snd sc)) sco))
      (unsome_default [] (LibOption.map stat_vardecl sfo)))
| Coq_stat_for (l, o, o0, o1, s) -> stat_vardecl s
| Coq_stat_for_var (l, nes, o, o0, s) ->
  append (LibList.map fst nes) (stat_vardecl s)
| Coq_stat_for_in (l, e, e0, s) -> stat_vardecl s
| Coq_stat_for_in_var (l, str, o, e, s) -> str :: (stat_vardecl s)
| Coq_stat_debugger -> []
| Coq_stat_switch (l, e, sb) -> switchbody_vardecl sb

(** val switchbody_vardecl : switchbody -> string list **)

and switchbody_vardecl _foo_ = match _foo_ with
| Coq_switchbody_nodefault scl -> concat (map switchclause_vardecl scl)
| Coq_switchbody_withdefault (scl1, sl, scl2) ->
  append (concat (map switchclause_vardecl scl1))
    (append (concat (map stat_vardecl sl))
      (concat (map switchclause_vardecl scl2)))

(** val switchclause_vardecl : switchclause -> string list **)

and switchclause_vardecl _foo_ = match _foo_ with
| Coq_switchclause_intro (e, sl) -> concat (map stat_vardecl sl)

(** val element_vardecl : element -> string list **)

let element_vardecl _foo_ = match _foo_ with
| Coq_element_stat t -> stat_vardecl t
| Coq_element_func_decl (name, args, bd) -> []

(** val prog_vardecl : prog -> string list **)

let prog_vardecl p =
  concat (LibList.map element_vardecl (prog_elements p))

type preftype =
| Coq_preftype_number [@f]  (* Auto Generated Attributes *)
| Coq_preftype_string [@f]  (* Auto Generated Attributes *)

(** val method_of_preftype : preftype -> string **)

let method_of_preftype _foo_ = match _foo_ with
| Coq_preftype_number -> "valueOf"
| Coq_preftype_string ->
  "toString"

(** val other_preftypes : preftype -> preftype **)

let other_preftypes _foo_ = match _foo_ with
| Coq_preftype_number -> Coq_preftype_string
| Coq_preftype_string -> Coq_preftype_number

(** val throw_true : strictness_flag **)

let throw_true =
  true

(** val throw_false : strictness_flag **)

let throw_false =
  false

(** val throw_irrelevant : strictness_flag **)

let throw_irrelevant =
  false

(** val add_one : number -> number **)

let add_one n =
  n +. JsNumber.one

(** val sub_one : number -> number **)

let sub_one n =
  n -. JsNumber.one

(** val is_syntactic_eval : expr -> bool **)

let is_syntactic_eval _foo_ = match _foo_ with
| Coq_expr_this -> false
| Coq_expr_identifier s -> string_eq s ("eval")
| Coq_expr_literal l ->
  (match l with
   | Coq_literal_null -> false
   | Coq_literal_bool b -> false
   | Coq_literal_number n -> false
   | Coq_literal_string s ->
     string_eq s ("eval"))
| Coq_expr_object l -> false
| Coq_expr_array l -> false
| Coq_expr_function (o, l, f) -> false
| Coq_expr_access (e0, e1) -> false
| Coq_expr_member (e0, s) -> false
| Coq_expr_new (e0, l) -> false
| Coq_expr_call (e0, l) -> false
| Coq_expr_unary_op (u, e0) -> false
| Coq_expr_binary_op (e0, b, e1) -> false
| Coq_expr_conditional (e0, e1, e2) -> false
| Coq_expr_assign (e0, o, e1) -> false

(** val elision_head_count : 'a1 option list -> int **)

let rec elision_head_count _foo_ = match _foo_ with
| [] -> 0
| o :: ol' ->
  (match o with
   | Some t -> 0
   | None -> 1 + (elision_head_count ol'))

(** val elision_head_remove : 'a1 option list -> 'a1 option list **)

let rec elision_head_remove ol = match ol with
| [] -> ol
| o :: ol' ->
  (match o with
   | Some t -> ol
   | None -> elision_head_remove ol')

(** val elision_tail_count : 'a1 option list -> int **)

let elision_tail_count ol =
  elision_head_count (rev ol)

(** val elision_tail_remove : 'a1 option list -> 'a1 option list **)

let elision_tail_remove ol =
  rev (elision_head_remove (rev ol))

(** val parse_pickable : string -> bool -> prog coq_Pickable_option **)

let parse_pickable = (fun s strict ->
  Translate_syntax.parse_esprima strict s
    (*Translate_syntax.parse_esprima strict s*)
    (* with
      (* | Translate_syntax.CoqSyntaxDoesNotSupport _ -> assert false (* Temporary *) *)
      | Parser.ParserFailure _ [@f]  (* Auto Generated Attributes *)
      | Parser.InvalidArgument ->
        prerr_string ("Warning:  Parser error on eval.  Input string:  \"" ^ str ^ "\"\n");
        None
    *)
  )

