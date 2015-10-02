open Asttypes
open Attributes
open Log
open Misc
open Mytools
open Parse_type
open Print_type
open Types
open Typedtree

module L = Logged (Token_generator) (struct let size = 256 end)

(* TODO: Field annotations for builtin type constructors *)

(**
 * Useful functions (Warning: shadows `show_list' from Mytools)
 *)

let show_list_f f sep l = l
  |> List.map f
  |> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) ""

let show_list sep l =
  List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) "" l

let is_sbool x = List.mem x ["true" ; "false"]

(* Given an expression, check whether it is a primitive type or a constructed type *)
let exp_type_is_constant exp =
  List.exists (Ctype.matches exp.exp_env exp.exp_type)
  [Predef.type_bool; Predef.type_int; Predef.type_char; Predef.type_string; Predef.type_float]

let rec zip l1 l2 = match l1, l2 with
  | [], x :: xs | x :: xs, [] -> failwith "zip: list must have the same length."
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: zip xs ys

let unzip l =
  let rec aux acc1 acc2 = function
  | [] -> List.rev acc1, List.rev acc2
  | (x, y) :: xs -> aux (x :: acc1) (y :: acc2) xs
in aux [] [] l

let string_fold_lefti f acc s =
  let len = String.length s in
  let rec aux f acc i =
    if i = len then acc
    else aux f (f acc i s.[i]) (succ i)
  in aux f acc 0

let string_fold_righti f s acc =
 let len = String.length s in
  let rec aux f i acc =
    if i = len then acc
    else f i s.[i] (aux f (succ i) acc)
  in aux f 0 acc

let is_infix f args = match args with
  | _ :: [] | [] -> false
  | x :: xs ->
     let open Location in
     let f_loc = (f.exp_loc.loc_start, f.exp_loc.loc_end) in
     let args_loc = (x.exp_loc.loc_start, x.exp_loc.loc_end) in
     if fst args_loc < fst f_loc then true else false

let map_cstr_fields ?loc f (cstr : constructor_description) elements =
  let fields = extract_cstr_attrs cstr in
  try List.map2 f fields elements
  with Invalid_argument _ -> error ?loc ("Insufficient fieldnames for arguments to " ^ cstr.cstr_name)

(**
 * Before-hand definitions of Pretty-Printer-Format for converting ocaml
 * to ECMAScript, therefore all of them are in a single place.
 *)

let ppf_lambda_wrap s =
  Printf.sprintf "(function () {@;<1 2>@[<v 0>%s@]@,}())@," s

let ppf_branch case binders expr =
  Printf.sprintf "%s: @[<v 0>%s%s@]@,"
                 case binders expr

let rec ppf_branches branches expr =
  match branches with
  | (case, binders) :: [] ->
    let expr = Printf.sprintf "@,return %s;" expr in
    ppf_branch case binders expr
  | (case, binders) :: l' -> (ppf_branch case binders "") ^ (ppf_branches l' expr)
  | [] -> ""

let ppf_let_in decl exp =
  let s =
    Printf.sprintf "%s@,return %s;"
                   decl exp
  in ppf_lambda_wrap s

let ppf_function args body=
  Printf.sprintf "function (%s) {@;<1 2>@[<v 0>return %s;@]@,}"
                 args body

let ppf_apply f args =
  Printf.sprintf "%s(%s)"
                 f args

let ppf_apply_infix f arg1 arg2 =
  Printf.sprintf "%s %s %s"
                 arg1 f arg2

let ppf_match value cases const =
  let cons_fld = if const then "" else ".type" in
  let s = Printf.sprintf "switch (%s%s) {@,@[<v 0>%s@]@,}"
    value cons_fld cases
  in ppf_lambda_wrap s

let ppf_match_case c =
  Printf.sprintf "case %s" c

let ppf_match_binders binders =
  if binders = [] then "" else
  let binds = show_list ", " binders in
  Printf.sprintf "@[<v 0>var %s;@]" binds

(* obj is passed as the object variable binding, if we need to deconstruct it *)
let ppf_match_binder var ?obj fld = match obj with
  | None     -> Printf.sprintf "%s = %s" var fld
  | Some obj -> Printf.sprintf "%s = %s.%s" var obj fld

let ppf_array values =
  Printf.sprintf "[%s]"
                 values

let ppf_tuple = ppf_array

let ppf_ifthen cond iftrue =
  Printf.sprintf "(function () {@;<1 2>@[<v 2>@,if (%s) {@,return  %s;@,}@]@,})()"
                 cond iftrue

let ppf_ifthenelse cond iftrue iffalse =
  Printf.sprintf "(function () {@;<1 2>@[<v 2>@,if (%s) {@,return  %s;@,} else {@,return  %s;@,}@]@,})()"
                 cond iftrue iffalse

let ppf_sequence exp1 exp2 =
  Printf.sprintf "%s,@,%s"
                 exp1 exp2

let ppf_while cd body =
  let s =
    Printf.sprintf "@[<v 2>while(%s) {@;<1 2>%s@,@]}"
                   cd body
  in ppf_lambda_wrap s

let ppf_for id start ed flag body =
  let fl_to_string = function
    | Upto   -> "++"
    | Downto -> "--" in
  let fl_to_symbl = function
    | Upto   -> "<="
    | Downto -> ">=" in
  let s =
    Printf.sprintf "@[<v 2>for (%s = %s ; %s %s %s ; %s%s) {@,%s@]@,}"
                   id start id (fl_to_symbl flag) ed (fl_to_string flag) id body
  in ppf_lambda_wrap s

(*let ppf_single_cstr tag =
  Printf.sprintf "%s"
    tag
*)
let ppf_cstr tag value =
  Printf.sprintf "%s: %s" tag value

let ppf_single_cstrs typ =
   Printf.sprintf "@[<v 2>{type: \"%s\"}@]" typ

let ppf_multiple_cstrs typ rest =
  Printf.sprintf "@[<v 2>{type: \"%s\", %s}@]"
    typ rest

let ppf_record llde =
  let rec aux acc = function
    | []               -> Printf.sprintf "@[<v 2>{@;<1 2>%s@]@,}" acc
    | (lbl, exp) :: [] -> aux (acc ^ Printf.sprintf "%s: %s" lbl exp) []
    | (lbl, exp) :: xs -> aux (acc ^ Printf.sprintf "%s: %s,@," lbl exp) xs
  in aux "" llde

let ppf_decl id expr = Printf.sprintf "@[<v 0>%s: %s,@,@]" id expr

let ppf_pat_array id_list array_expr =
  Printf.sprintf "var __%s = %s;@," "array" array_expr ^
    List.fold_left2 (fun acc (name, exp_type) y -> acc ^ Printf.sprintf "@[<v 0>var %s = __%s[%d];@,@]" name "array" y)
                    "" id_list @@ range 0 (List.length id_list - 1)

let ppf_field_access expr field =
  Printf.sprintf "%s.%s" expr field

(* ' is not permitted in JS identifier names, and $ is not permitted in OCaml ones *)
let ppf_ident_name =
  String.map (function '\'' -> '$' | c -> c)

let ppf_ident i =
  i |> Ident.name |> ppf_ident_name

let ppf_path =
  Path.name

let ppf_module content =
  Printf.sprintf "{@,%s@,}" content

let ppf_module_wrap name content =
  let modu = ppf_module content in
  Printf.sprintf "var %s = %s;" name modu

(**
 * Main part
 *)

let rec js_of_structure s =
  show_list_f (fun strct -> js_of_structure_item strct) "@,@," s.str_items

and js_of_submodule m =
  let loc = m.mod_loc in
  match m.mod_desc with
  | Tmod_structure  s -> ppf_module (js_of_structure s)
  | Tmod_functor (id, _, mtyp, mexp) -> ppf_function (ppf_ident id) (js_of_submodule mexp)
  | Tmod_apply   (m1, m2, _)         -> ppf_apply (js_of_submodule m1) (js_of_submodule m2)
  | Tmod_ident (p,_) -> ppf_path p
  | Tmod_constraint _ -> out_of_scope loc "module constraint"
  | Tmod_unpack     _ -> out_of_scope loc "module unpack"

and show_value_binding vb =
  js_of_let_pattern vb.vb_pat vb.vb_expr

and js_of_structure_item s =
  let loc = s.str_loc in
  match s.str_desc with
  | Tstr_eval (e, _)     -> Printf.sprintf "%s" @@ js_of_expression e
  | Tstr_value (_, vb_l) -> String.concat "@,@," @@ List.map (fun vb -> show_value_binding vb) @@ vb_l
  | Tstr_type       _  -> "" (* Types have no representation in JS, but the OCaml type checker uses them *)
  | Tstr_open       _  -> "" (* Handle modules by use of multiple compilation/linking *)
  | Tstr_modtype    _  -> ""
  | Tstr_module     b  -> ppf_decl (ppf_ident b.mb_id) (js_of_submodule b.mb_expr)
  | Tstr_primitive  _  -> out_of_scope loc "primitive functions"
  | Tstr_typext     _  -> out_of_scope loc "type extensions"
  | Tstr_exception  _  -> out_of_scope loc "exceptions"
  | Tstr_recmodule  _  -> out_of_scope loc "recursive modules"
  | Tstr_class      _  -> out_of_scope loc "objects"
  | Tstr_class_type _  -> out_of_scope loc "class types"
  | Tstr_include    _  -> out_of_scope loc "includes"
  | Tstr_attribute  _  -> out_of_scope loc "attributes"

and js_of_branch b obj =
  let patterns = js_of_pattern b.c_lhs obj in
  let se = js_of_expression b.c_rhs in
  (*if binders = "" then *)ppf_branches patterns se
  (* FIXME: Logging
   else
    let typ = match List.rev (Str.split (Str.regexp " ") spat) with
      | [] -> assert false
      | x :: xs -> String.sub x 0 (String.length x)
    in L.log_line (ppf_branch spat binders se) (L.Add (binders, typ))
  *)

and js_of_expression e =
  let loc = e.exp_loc in
  match e.exp_desc with
  | Texp_ident (_, ident,  _)         -> js_of_longident ident
  | Texp_constant c                   -> js_of_constant c
  | Texp_let   (_, vb_l, e)           ->
    let sd = String.concat lin1 @@ List.map (fun vb -> show_value_binding vb) @@ vb_l in
    let se = js_of_expression e
    in ppf_let_in sd se
  | Texp_function (_, c :: [], Total) ->
    let rec explore pats e = match e.exp_desc with
      | Texp_function (_, c :: [], Total) ->
        let p, e = c.c_lhs, c.c_rhs
        in explore (p :: pats) e
      | _                                 ->
        String.concat ", " @@ List.map ident_of_pat @@ List.rev @@ pats, js_of_expression e in
    let args, body = explore [c.c_lhs] c.c_rhs
    in ppf_function args body
  | Texp_apply (f, exp_l)                 ->
     let sl' = exp_l
               |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope loc "optional apply arguments" | Some ei -> ei) in
     let sl = exp_l
              |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope loc "optional apply arguments" | Some ei -> js_of_expression ei) in
    let se = js_of_expression f in
    if is_infix f sl' && List.length exp_l = 2
    then ppf_apply_infix se (List.hd sl) (List.hd (List.tl sl))
    else ppf_apply se (String.concat ", " sl)

  | Texp_match (exp, l, [], Total) ->
     let se = js_of_expression exp in
     let sb = String.concat "@," (List.map (fun x -> js_of_branch x se) l) in
     let const = exp_type_is_constant exp in
     ppf_match se sb const

  | Texp_tuple (tl) -> ppf_tuple @@ show_list_f (fun exp -> js_of_expression exp) ", " tl

  | Texp_construct (_, cd, el) ->
    let name = cd.cstr_name in
    if el = [] then (* Constructor has no parameters *)
      if is_sbool name then name (* Special case true/false to their JS natives *)
      else ppf_single_cstrs name
    else (* Constructor has parameters *)
      let expr_strs = List.map (fun exp -> js_of_expression exp) el in
      let expanded_constructors = map_cstr_fields ~loc ppf_cstr cd expr_strs in
      ppf_multiple_cstrs name (show_list ", " expanded_constructors)

  | Texp_array      (exp_l)           -> ppf_array @@ show_list_f (fun exp -> js_of_expression exp) ", " exp_l
  | Texp_ifthenelse (e1, e2, None)    -> ppf_ifthen (js_of_expression e1) (js_of_expression e2)
  | Texp_ifthenelse (e1, e2, Some e3) -> ppf_ifthenelse (js_of_expression e1) (js_of_expression e2) (js_of_expression e3)
  | Texp_sequence   (e1, e2)          -> ppf_sequence (js_of_expression e1) (js_of_expression e2)
  | Texp_while      (cd, body)        -> ppf_while (js_of_expression cd) (js_of_expression body)
  | Texp_for        (id, _, st, ed, fl, body) -> ppf_for (ppf_ident id) (js_of_expression st) (js_of_expression ed) fl (js_of_expression body)
  | Texp_record     (llde,_)          -> ppf_record (List.map (fun (_, lbl, exp) -> (lbl.lbl_name, js_of_expression exp)) llde)
  | Texp_field      (exp, _, lbl)     ->
    ppf_field_access (js_of_expression exp) lbl.lbl_name

  | Texp_match      (_,_,_, Partial)  -> out_of_scope loc "partial matching"
  | Texp_match      (_,_,_,_)         -> out_of_scope loc "matching with exception branches"
  | Texp_try        (_,_)             -> out_of_scope loc "exceptions"
  | Texp_function   (_,_,_)           -> out_of_scope loc "powered-up functions"
  | Texp_variant    (_,_)             -> out_of_scope loc "polymorphic variant"
  | Texp_setfield   (_,_,_,_)         -> out_of_scope loc "setting field"
  | Texp_send       (_,_,_)           -> out_of_scope loc "objects"
  | Texp_new        (_,_,_)           -> out_of_scope loc "objects"
  | Texp_instvar    (_,_,_)           -> out_of_scope loc "objects"
  | Texp_setinstvar (_,_,_,_)         -> out_of_scope loc "objects"
  | Texp_override   (_,_)             -> out_of_scope loc "objects"
  | Texp_letmodule  (_,_,_,_)         -> out_of_scope loc "local modules"
  | Texp_assert      _                -> out_of_scope loc "assert"
  | Texp_lazy        _                -> out_of_scope loc "lazy expressions"
  | Texp_object     (_,_)             -> out_of_scope loc "objects"
  | Texp_pack        _                -> out_of_scope loc "packing"

and js_of_constant = function
  | Const_int       n     -> string_of_int n
  | Const_char      c     -> String.make 1 c
  | Const_string   (s, _) -> "\"" ^ s ^ "\""
  | Const_float     f     -> f
  | Const_int32     n     -> Int32.to_string n
  | Const_int64     n     -> Int64.to_string n
  | Const_nativeint n     -> Nativeint.to_string n

and js_of_longident loc =
  let res = String.concat "." @@ Longident.flatten loc.txt in
  if res = "()" then "undefined" else ppf_ident_name res

and ident_of_pat pat = match pat.pat_desc with
  | Tpat_var (id, _) -> ppf_ident id
  | Tpat_any         -> ""
  | _ -> error ~loc:pat.pat_loc "functions can't deconstruct values"

and js_of_let_pattern pat expr =
  let sexpr = js_of_expression expr in
  match pat.pat_desc with
  | Tpat_var (id, _) -> ppf_decl (ppf_ident id) sexpr
  | Tpat_tuple (pat_l)
  | Tpat_array (pat_l) ->
     let l = List.map
               (function pat ->
                         match pat.pat_desc with
                         | Tpat_var (id, _) -> (ppf_ident id, string_of_type_exp pat.pat_type)
                         | _ -> out_of_scope pat.pat_loc "pattern-matching in arrays"
               ) pat_l in
     ppf_pat_array l sexpr
  | Tpat_record (flds, _) -> ppf_match_binders (List.map
      (fun (_,lbl,pat) -> pat_bind sexpr lbl.lbl_name pat)
    flds)
  | _ -> error ~loc:pat.pat_loc "let can't deconstruct values"

and pat_bind obj field var = (match var.pat_desc with
  | Tpat_var (id, _) -> ppf_match_binder (ppf_ident id) ~obj field
  | Tpat_any         -> ""
  | _                -> out_of_scope var.pat_loc "Nested pattern matching")

and js_of_pattern pat obj =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | Tpat_any                     -> ["default", ""]
  | Tpat_constant   c            -> [ppf_match_case (js_of_constant c), ""]
  | Tpat_var       (id, _)       -> ["default", (ppf_match_binders [ppf_match_binder (ppf_ident id) ""])]
  | Tpat_construct (_, cd, el) ->
     let c = cd.cstr_name in
     let spat = if is_sbool c then ppf_match_case c else ppf_match_case ("\"" ^ c ^ "\"") in
     let binders = ppf_match_binders (map_cstr_fields ~loc (pat_bind obj) cd el) in
     [spat, binders]
  | Tpat_or (p1,p2,_) -> (js_of_pattern p1 obj) @ (js_of_pattern p2 obj)
  | Tpat_tuple el -> unsupported ~loc "tuple matching"
  | Tpat_array el -> unsupported ~loc "array-match"
  | Tpat_record (_,_) -> unsupported ~loc "record"
  | Tpat_alias (_,_,_) -> out_of_scope loc "alias-pattern"
  | Tpat_variant (_,_,_) -> out_of_scope loc "polymorphic variants in pattern matching"
  | Tpat_lazy _ -> out_of_scope loc "lazy-pattern"

let to_javascript module_name typedtree =
  let content = js_of_structure typedtree in
  let pre_res = ppf_module_wrap module_name content in
  (L.logged_output pre_res, L.unlogged_output pre_res, pre_res)

