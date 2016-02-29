open JsNumber
open JsSyntax
open LibList
open LibNat
open LibReflect
open LibString

(** val object_create :
    value -> class_name -> bool -> object_properties_type -> coq_object **)

let object_create vproto sclass bextens p =
  { object_proto_ = vproto; object_class_ = sclass; object_extensible_ =
    bextens; object_prim_value_ = None; object_properties_ = p; object_get_ =
    Coq_builtin_get_default; object_get_own_prop_ =
    Coq_builtin_get_own_prop_default; object_get_prop_ =
    Coq_builtin_get_prop_default; object_put_ = Coq_builtin_put_default;
    object_can_put_ = Coq_builtin_can_put_default; object_has_prop_ =
    Coq_builtin_has_prop_default; object_delete_ =
    Coq_builtin_delete_default; object_default_value_ =
    Coq_builtin_default_value_default; object_define_own_prop_ =
    Coq_builtin_define_own_prop_default; object_construct_ = None;
    object_call_ = None; object_has_instance_ = None; object_scope_ = None;
    object_formal_parameters_ = None; object_code_ = None;
    object_target_function_ = None; object_bound_this_ = None;
    object_bound_args_ = None; object_parameter_map_ = None }

(** val object_set_proto : coq_object -> value -> coq_object **)

let object_set_proto o v =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = v; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_set_class : coq_object -> class_name -> coq_object **)

let object_set_class o s =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = s; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_set_extensible : coq_object -> bool -> coq_object **)

let object_set_extensible o b =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = b;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_primitive_value : coq_object -> value -> coq_object **)

let object_with_primitive_value o v =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = (Some v); object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_extension : coq_object -> bool -> coq_object **)

let object_with_extension o b =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = b;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_properties :
    coq_object -> object_properties_type -> coq_object **)

let object_with_properties o properties =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = properties; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_get : coq_object -> builtin_get -> coq_object **)

let object_with_get o g =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = g;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_get_own_property :
    coq_object -> builtin_get_own_prop -> coq_object **)

let object_with_get_own_property o gop =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = gop; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_invokation :
    coq_object -> construct option -> call option -> builtin_has_instance
    option -> coq_object **)

let object_with_invokation o constr call0 has_instance =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = constr; object_call_ = call0; object_has_instance_ =
  has_instance; object_scope_ = x18; object_formal_parameters_ = x19;
  object_code_ = x20; object_target_function_ = x21; object_bound_this_ =
  x22; object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_scope :
    coq_object -> lexical_env option -> coq_object **)

let object_with_scope o scope =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = scope; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_formal_params :
    coq_object -> string list option -> coq_object **)

let object_with_formal_params o params =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = params; object_code_ =
  x20; object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_with_details :
    coq_object -> lexical_env option -> string list option -> funcbody
    option -> object_loc option -> value option -> value list option ->
    object_loc option -> coq_object **)

let object_with_details o scope params code target boundthis boundargs paramsmap =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = x14;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = scope; object_formal_parameters_ = params; object_code_ =
  code; object_target_function_ = target; object_bound_this_ = boundthis;
  object_bound_args_ = boundargs; object_parameter_map_ = paramsmap }

(** val object_for_array :
    coq_object -> builtin_define_own_prop -> coq_object **)

let object_for_array o defineownproperty =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
  object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
  object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
  object_default_value_ = x13; object_define_own_prop_ = defineownproperty;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = x24 }

(** val object_for_args_object :
    coq_object -> object_loc -> builtin_get -> builtin_get_own_prop ->
    builtin_define_own_prop -> builtin_delete -> coq_object **)

let object_for_args_object o paramsmap get getownproperty defineownproperty delete =
  let { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
    object_prim_value_ = x4; object_properties_ = x5; object_get_ = x6;
    object_get_own_prop_ = x7; object_get_prop_ = x8; object_put_ = x9;
    object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = x12;
    object_default_value_ = x13; object_define_own_prop_ = x14;
    object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
    object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
    object_target_function_ = x21; object_bound_this_ = x22;
    object_bound_args_ = x23; object_parameter_map_ = x24 } = o
  in
  { object_proto_ = x1; object_class_ = x2; object_extensible_ = x3;
  object_prim_value_ = x4; object_properties_ = x5; object_get_ = get;
  object_get_own_prop_ = getownproperty; object_get_prop_ = x8; object_put_ =
  x9; object_can_put_ = x10; object_has_prop_ = x11; object_delete_ = delete;
  object_default_value_ = x13; object_define_own_prop_ = defineownproperty;
  object_construct_ = x15; object_call_ = x16; object_has_instance_ = x17;
  object_scope_ = x18; object_formal_parameters_ = x19; object_code_ = x20;
  object_target_function_ = x21; object_bound_this_ = x22;
  object_bound_args_ = x23; object_parameter_map_ = (Some paramsmap) }

(** val mathop_compare : mathop -> mathop -> bool **)

let mathop_compare m1 m2 =
  match m1 with
  | Coq_mathop_abs ->
    match m2 with
    | Coq_mathop_abs -> true

(** val mathop_comparable : mathop coq_Comparable **)

let mathop_comparable x y =
  mathop_compare x y

(** val native_error_compare : native_error -> native_error -> bool **)

let native_error_compare ne1 ne2 = ne1 === ne2

(** val native_error_comparable : native_error coq_Comparable **)

let native_error_comparable x y =
  native_error_compare x y

(** val prealloc_compare : prealloc -> prealloc -> bool **)

let prealloc_compare bl1 bl2 = bl1 === bl2

(** val prealloc_comparable : prealloc coq_Comparable **)

let prealloc_comparable x y =
  prealloc_compare x y

(** val object_loc_compare : object_loc -> object_loc -> bool **)

let object_loc_compare l1 l2 =
  match l1 with
  | Coq_object_loc_normal ln1 ->
    (match l2 with
     | Coq_object_loc_normal ln2 -> nat_comparable ln1 ln2
     | Coq_object_loc_prealloc p -> false)
  | Coq_object_loc_prealloc bl1 ->
    (match l2 with
     | Coq_object_loc_normal n -> false
     | Coq_object_loc_prealloc bl2 -> prealloc_comparable bl1 bl2)

(** val object_loc_comparable : object_loc coq_Comparable **)

let object_loc_comparable x y =
  object_loc_compare x y

(** val prim_compare : prim -> prim -> bool **)

let prim_compare w1 w2 =
  match w1 with
  | Coq_prim_undef ->
    (match w2 with
     | Coq_prim_undef -> true
     | Coq_prim_null -> false
     | Coq_prim_bool b -> false
     | Coq_prim_number n -> false
     | Coq_prim_string s -> false)
  | Coq_prim_null ->
    (match w2 with
     | Coq_prim_undef -> false
     | Coq_prim_null -> true
     | Coq_prim_bool b -> false
     | Coq_prim_number n -> false
     | Coq_prim_string s -> false)
  | Coq_prim_bool b1 ->
    (match w2 with
     | Coq_prim_undef -> false
     | Coq_prim_null -> false
     | Coq_prim_bool b2 -> bool_comparable b1 b2
     | Coq_prim_number n -> false
     | Coq_prim_string s -> false)
  | Coq_prim_number n1 ->
    (match w2 with
     | Coq_prim_undef -> false
     | Coq_prim_null -> false
     | Coq_prim_bool b -> false
     | Coq_prim_number n2 -> number_comparable n1 n2
     | Coq_prim_string s -> false)
  | Coq_prim_string s1 ->
    (match w2 with
     | Coq_prim_undef -> false
     | Coq_prim_null -> false
     | Coq_prim_bool b -> false
     | Coq_prim_number n -> false
     | Coq_prim_string s2 -> string_comparable s1 s2)

(** val prim_comparable : prim coq_Comparable **)

let prim_comparable x y =
  prim_compare x y

(** val value_compare : value -> value -> bool **)

let value_compare v1 v2 =
  match v1 with
  | Coq_value_prim w1 ->
    (match v2 with
     | Coq_value_prim w2 -> prim_comparable w1 w2
     | Coq_value_object o -> false)
  | Coq_value_object l1 ->
    (match v2 with
     | Coq_value_prim p -> false
     | Coq_value_object l2 -> object_loc_comparable l1 l2)

(** val value_comparable : value coq_Comparable **)

let value_comparable x y =
  value_compare x y

(** val mutability_compare : mutability -> mutability -> bool **)

let mutability_compare m1 m2 =
  match m1 with
  | Coq_mutability_uninitialized_immutable ->
    (match m2 with
     | Coq_mutability_uninitialized_immutable -> true
     | Coq_mutability_immutable -> false
     | Coq_mutability_nondeletable -> false
     | Coq_mutability_deletable -> false)
  | Coq_mutability_immutable ->
    (match m2 with
     | Coq_mutability_uninitialized_immutable -> false
     | Coq_mutability_immutable -> true
     | Coq_mutability_nondeletable -> false
     | Coq_mutability_deletable -> false)
  | Coq_mutability_nondeletable ->
    (match m2 with
     | Coq_mutability_uninitialized_immutable -> false
     | Coq_mutability_immutable -> false
     | Coq_mutability_nondeletable -> true
     | Coq_mutability_deletable -> false)
  | Coq_mutability_deletable ->
    (match m2 with
     | Coq_mutability_uninitialized_immutable -> false
     | Coq_mutability_immutable -> false
     | Coq_mutability_nondeletable -> false
     | Coq_mutability_deletable -> true)

(** val mutability_comparable : mutability coq_Comparable **)

let mutability_comparable x y =
  mutability_compare x y

(** val ref_base_type_compare : ref_base_type -> ref_base_type -> bool **)

let ref_base_type_compare rb1 rb2 =
  match rb1 with
  | Coq_ref_base_type_value v1 ->
    (match rb2 with
     | Coq_ref_base_type_value v2 -> value_comparable v1 v2
     | Coq_ref_base_type_env_loc e -> false)
  | Coq_ref_base_type_env_loc l1 ->
    (match rb2 with
     | Coq_ref_base_type_value v -> false
     | Coq_ref_base_type_env_loc l2 -> nat_comparable l1 l2)

(** val ref_base_type_comparable : ref_base_type coq_Comparable **)

let ref_base_type_comparable x y =
  ref_base_type_compare x y

(** val ref_compare : ref -> ref -> bool **)

let ref_compare r1 r2 =
  and_decidable (ref_base_type_comparable r1.ref_base r2.ref_base)
    (and_decidable (string_comparable r1.ref_name r2.ref_name)
      (bool_comparable r1.ref_strict r2.ref_strict))

(** val ref_comparable : ref coq_Comparable **)

let ref_comparable x y =
  ref_compare x y

(** val type_compare : coq_type -> coq_type -> bool **)

let type_compare t1 t2 =
  match t1 with
  | Coq_type_undef ->
    (match t2 with
     | Coq_type_undef -> true
     | Coq_type_null -> false
     | Coq_type_bool -> false
     | Coq_type_number -> false
     | Coq_type_string -> false
     | Coq_type_object -> false)
  | Coq_type_null ->
    (match t2 with
     | Coq_type_undef -> false
     | Coq_type_null -> true
     | Coq_type_bool -> false
     | Coq_type_number -> false
     | Coq_type_string -> false
     | Coq_type_object -> false)
  | Coq_type_bool ->
    (match t2 with
     | Coq_type_undef -> false
     | Coq_type_null -> false
     | Coq_type_bool -> true
     | Coq_type_number -> false
     | Coq_type_string -> false
     | Coq_type_object -> false)
  | Coq_type_number ->
    (match t2 with
     | Coq_type_undef -> false
     | Coq_type_null -> false
     | Coq_type_bool -> false
     | Coq_type_number -> true
     | Coq_type_string -> false
     | Coq_type_object -> false)
  | Coq_type_string ->
    (match t2 with
     | Coq_type_undef -> false
     | Coq_type_null -> false
     | Coq_type_bool -> false
     | Coq_type_number -> false
     | Coq_type_string -> true
     | Coq_type_object -> false)
  | Coq_type_object ->
    (match t2 with
     | Coq_type_undef -> false
     | Coq_type_null -> false
     | Coq_type_bool -> false
     | Coq_type_number -> false
     | Coq_type_string -> false
     | Coq_type_object -> true)

(** val type_comparable : coq_type coq_Comparable **)

let type_comparable x y =
  type_compare x y

(** val res_with_value : res -> resvalue -> res **)

let res_with_value r rv =
  let { res_type = rt; res_value = old_rv; res_label = labopt } = r in
  { res_type = rt; res_value = rv; res_label = labopt }

(** val resvalue_compare : resvalue -> resvalue -> bool **)

let resvalue_compare rv1 rv2 =
  match rv1 with
  | Coq_resvalue_empty ->
    (match rv2 with
     | Coq_resvalue_empty -> true
     | Coq_resvalue_value v -> false
     | Coq_resvalue_ref r -> false)
  | Coq_resvalue_value v1 ->
    (match rv2 with
     | Coq_resvalue_empty -> false
     | Coq_resvalue_value v2 -> value_comparable v1 v2
     | Coq_resvalue_ref r -> false)
  | Coq_resvalue_ref r1 ->
    (match rv2 with
     | Coq_resvalue_empty -> false
     | Coq_resvalue_value v -> false
     | Coq_resvalue_ref r2 -> ref_comparable r1 r2)

(** val resvalue_comparable : resvalue coq_Comparable **)

let resvalue_comparable x y =
  resvalue_compare x y

(** val binary_op_compare : binary_op -> binary_op -> bool **)

let rec binary_op_compare op1 op2 =
  match op1 with
  | Coq_binary_op_mult ->
    (match op2 with
     | Coq_binary_op_mult -> true
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_div ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> true
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_mod ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> true
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_add ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> true
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_sub ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> true
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_left_shift ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> true
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_right_shift ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> true
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_unsigned_right_shift ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> true
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_lt ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> true
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_gt ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> true
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_le ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> true
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_ge ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> true
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_instanceof ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> true
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_in ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> true
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_equal ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> true
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_disequal ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> true
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_strict_equal ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> true
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_strict_disequal ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> true
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_bitwise_and ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> true
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_bitwise_or ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> true
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_bitwise_xor ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> true
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_and ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> true
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_or ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> true
     | Coq_binary_op_coma -> false)
  | Coq_binary_op_coma ->
    (match op2 with
     | Coq_binary_op_mult -> false
     | Coq_binary_op_div -> false
     | Coq_binary_op_mod -> false
     | Coq_binary_op_add -> false
     | Coq_binary_op_sub -> false
     | Coq_binary_op_left_shift -> false
     | Coq_binary_op_right_shift -> false
     | Coq_binary_op_unsigned_right_shift -> false
     | Coq_binary_op_lt -> false
     | Coq_binary_op_gt -> false
     | Coq_binary_op_le -> false
     | Coq_binary_op_ge -> false
     | Coq_binary_op_instanceof -> false
     | Coq_binary_op_in -> false
     | Coq_binary_op_equal -> false
     | Coq_binary_op_disequal -> false
     | Coq_binary_op_strict_equal -> false
     | Coq_binary_op_strict_disequal -> false
     | Coq_binary_op_bitwise_and -> false
     | Coq_binary_op_bitwise_or -> false
     | Coq_binary_op_bitwise_xor -> false
     | Coq_binary_op_and -> false
     | Coq_binary_op_or -> false
     | Coq_binary_op_coma -> true)

(** val binary_op_comparable : binary_op coq_Comparable **)

let binary_op_comparable x y =
  binary_op_compare x y

(** val prog_intro_strictness : prog -> strictness_flag **)

let prog_intro_strictness p = match p with
| Coq_prog_intro (str, els) -> str

(** val prog_elements : prog -> element list **)

let prog_elements p = match p with
| Coq_prog_intro (str, els) -> els

(** val funcbody_prog : funcbody -> prog **)

let funcbody_prog fb = match fb with
| Coq_funcbody_intro (p, s) -> p

(** val funcbody_is_strict : funcbody -> strictness_flag **)

let funcbody_is_strict fb = match fb with
| Coq_funcbody_intro (p, s) ->
  match p with
  | Coq_prog_intro (b_strict, l) -> b_strict

(** val restype_compare : restype -> restype -> bool **)

let restype_compare rt1 rt2 =
  match rt1 with
  | Coq_restype_normal ->
    (match rt2 with
     | Coq_restype_normal -> true
     | Coq_restype_break -> false
     | Coq_restype_continue -> false
     | Coq_restype_return -> false
     | Coq_restype_throw -> false)
  | Coq_restype_break ->
    (match rt2 with
     | Coq_restype_normal -> false
     | Coq_restype_break -> true
     | Coq_restype_continue -> false
     | Coq_restype_return -> false
     | Coq_restype_throw -> false)
  | Coq_restype_continue ->
    (match rt2 with
     | Coq_restype_normal -> false
     | Coq_restype_break -> false
     | Coq_restype_continue -> true
     | Coq_restype_return -> false
     | Coq_restype_throw -> false)
  | Coq_restype_return ->
    (match rt2 with
     | Coq_restype_normal -> false
     | Coq_restype_break -> false
     | Coq_restype_continue -> false
     | Coq_restype_return -> true
     | Coq_restype_throw -> false)
  | Coq_restype_throw ->
    (match rt2 with
     | Coq_restype_normal -> false
     | Coq_restype_break -> false
     | Coq_restype_continue -> false
     | Coq_restype_return -> false
     | Coq_restype_throw -> true)

(** val restype_comparable : restype coq_Comparable **)

let restype_comparable x y =
  restype_compare x y

(** val label_compare : label -> label -> bool **)

let label_compare lab1 lab2 =
  match lab1 with
  | Coq_label_empty ->
    (match lab2 with
     | Coq_label_empty -> true
     | Coq_label_string s -> false)
  | Coq_label_string s1 ->
    (match lab2 with
     | Coq_label_empty -> false
     | Coq_label_string s2 -> string_comparable s1 s2)

(** val label_comparable : label coq_Comparable **)

let label_comparable x y =
  label_compare x y

(** val label_set_empty : label_set **)

let label_set_empty =
  []

(** val label_set_add : label -> label list -> label list **)

let label_set_add lab labs =
  lab :: labs

(** val label_set_add_empty : label list -> label list **)

let label_set_add_empty labs =
  label_set_add Coq_label_empty labs

(** val label_set_mem : label -> label list -> bool **)

let label_set_mem lab labs =
  coq_Mem_decidable label_comparable lab labs

(** val attributes_data_with_value :
    attributes_data -> value -> attributes_data **)

let attributes_data_with_value ad v' =
  let { attributes_data_value = v; attributes_data_writable = bw;
    attributes_data_enumerable = be; attributes_data_configurable = bc } = ad
  in
  { attributes_data_value = v'; attributes_data_writable = bw;
  attributes_data_enumerable = be; attributes_data_configurable = bc }

(** val descriptor_with_value : descriptor -> value option -> descriptor **)

let descriptor_with_value desc v' =
  let { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
    descriptor_set = vs; descriptor_enumerable = be;
    descriptor_configurable = bc } = desc
  in
  { descriptor_value = v'; descriptor_writable = bw; descriptor_get = vg;
  descriptor_set = vs; descriptor_enumerable = be; descriptor_configurable =
  bc }

(** val descriptor_with_writable :
    descriptor -> bool option -> descriptor **)

let descriptor_with_writable desc bw' =
  let { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
    descriptor_set = vs; descriptor_enumerable = be;
    descriptor_configurable = bc } = desc
  in
  { descriptor_value = v; descriptor_writable = bw'; descriptor_get = vg;
  descriptor_set = vs; descriptor_enumerable = be; descriptor_configurable =
  bc }

(** val descriptor_with_get : descriptor -> value option -> descriptor **)

let descriptor_with_get desc vg' =
  let { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
    descriptor_set = vs; descriptor_enumerable = be;
    descriptor_configurable = bc } = desc
  in
  { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg';
  descriptor_set = vs; descriptor_enumerable = be; descriptor_configurable =
  bc }

(** val descriptor_with_set : descriptor -> value option -> descriptor **)

let descriptor_with_set desc vs' =
  let { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
    descriptor_set = vs; descriptor_enumerable = be;
    descriptor_configurable = bc } = desc
  in
  { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
  descriptor_set = vs'; descriptor_enumerable = be; descriptor_configurable =
  bc }

(** val descriptor_with_enumerable :
    descriptor -> bool option -> descriptor **)

let descriptor_with_enumerable desc be' =
  let { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
    descriptor_set = vs; descriptor_enumerable = be;
    descriptor_configurable = bc } = desc
  in
  { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
  descriptor_set = vs; descriptor_enumerable = be'; descriptor_configurable =
  bc }

(** val descriptor_with_configurable :
    descriptor -> bool option -> descriptor **)

let descriptor_with_configurable desc bc' =
  let { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
    descriptor_set = vs; descriptor_enumerable = be;
    descriptor_configurable = bc } = desc
  in
  { descriptor_value = v; descriptor_writable = bw; descriptor_get = vg;
  descriptor_set = vs; descriptor_enumerable = be; descriptor_configurable =
  bc' }

(** val codetype_compare : codetype -> codetype -> bool **)

let codetype_compare ct1 ct2 =
  match ct1 with
  | Coq_codetype_func ->
    (match ct2 with
     | Coq_codetype_func -> true
     | Coq_codetype_global -> false
     | Coq_codetype_eval -> false)
  | Coq_codetype_global ->
    (match ct2 with
     | Coq_codetype_func -> false
     | Coq_codetype_global -> true
     | Coq_codetype_eval -> false)
  | Coq_codetype_eval ->
    (match ct2 with
     | Coq_codetype_func -> false
     | Coq_codetype_global -> false
     | Coq_codetype_eval -> true)

(** val codetype_comparable : codetype coq_Comparable **)

let codetype_comparable x y =
  codetype_compare x y

