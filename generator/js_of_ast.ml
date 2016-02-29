open Params
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

let string_of_longident i =
  String.concat "." @@ Longident.flatten @@ i


(****************************************************************)
(* STRING UTILITIES *)

(**
 * Useful functions (Warning: shadows `show_list' from Mytools)
 *)

let show_list_f f sep l = l
  |> List.map f
  |> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) ""

let show_list sep l =
  List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) "" l

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


(****************************************************************)
(* RECOGNIZING EXPRESSIONS *)

let is_sbool x = List.mem x ["true" ; "false"]

let is_unit x = x = "()"

let unit_repr = "{}"

(* Given an expression, check whether it is a primitive type or a constructed type *)
let exp_type_is_constant exp =
  List.exists (Ctype.matches exp.exp_env exp.exp_type)
  [Predef.type_bool; Predef.type_int; Predef.type_char; Predef.type_string; Predef.type_float]

let is_infix f args = match args with
  | _ :: [] | [] -> false
  | x :: xs ->
     let open Location in
     let f_loc = (f.exp_loc.loc_start, f.exp_loc.loc_end) in
     let args_loc = (x.exp_loc.loc_start, x.exp_loc.loc_end) in
     if fst args_loc < fst f_loc then true else false

exception Map_fields_elements_mismatch_number_args

(* here, bind is the function to be applied to a field and an element,
   and it returns an option, with None when the entry should be ignored,
   and with a result otherwise, to be added to the list of results *)

let map_filter_fields_elements bind fields elements =
  let rec aux = function
    | [], [] -> []
    | f :: fs, e :: es ->
      let res = aux (fs,es) in
      begin match bind f e with
        | None -> res
        | Some p -> p :: res  (* p is a pair identifier, code to be bound *)
      end
    | _ -> raise Map_fields_elements_mismatch_number_args
  in aux (fields, elements)

let map_cstr_fields ?loc bind (cstr : constructor_description) elements =
  let fields = extract_cstr_attrs cstr in
  try map_filter_fields_elements bind fields elements 
  with   Map_fields_elements_mismatch_number_args -> 
     error ?loc ("Insufficient fieldnames for arguments to " ^ cstr.cstr_name)
  
(****************************************************************)
(* PPF HELPERS *)

(**
 * Before-hand definitions of Pretty-Printer-Format for converting ocaml
 * to ECMAScript, therefore all of them are in a single place.
 *)

let ppf_lambda_wrap s =
  Printf.sprintf "(function () {@;<1 2>@[<v 0>%s@]@,}())@," s

let ppf_branch case binders expr =
  Printf.sprintf "%s: @[<v 0>%s@,return %s;@]"
                 case binders expr

let ppf_let_in decl exp =
  let s =
    Printf.sprintf "%s@,return %s;"
                   decl exp
  in ppf_lambda_wrap s

let ppf_function args body=
  (L.log_line (Printf.sprintf "function (%s) {" args) [L.Enter; (L.CreateCtx args)]) ^ (Printf.sprintf "@;<1 2>@[<v 0>return %s;@]@,}" body)

let ppf_apply f args =
  Printf.sprintf "%s(%s)"
                 f args

let ppf_apply_infix f arg1 arg2 =
  Printf.sprintf "%s %s %s"
                 arg1 f arg2

let ppf_match value cases const =
  let cons_fld = if const then "" else ".tag" in
  let cases = 
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_unlogged -> cases
    | Mode_line_token
    | Mode_logged -> cases ^ "@,default: throw \"No matching case for switch\";"
    in
  let s = Printf.sprintf "switch (%s%s) {@;<1 2>@[<v 0>%s@]@,}@,"
    value cons_fld cases
  in s

let ppf_match_case c =
  Printf.sprintf "case %s" c

let ppf_match_binders binders =
  if binders = [] then "" else
  let binds = show_list ", " (List.map (fun (id,se) -> Printf.sprintf "%s = %s" id se) binders) in
  Printf.sprintf "var %s;@," binds

let ppf_array values =
  Printf.sprintf "[%s]"
                 values

let ppf_tuple = ppf_array

let ppf_ifthen cond iftrue =
  Printf.sprintf "(function () {@;<1 2>@[<v 2>@,if (%s) {@,return  %s;@,}@]@,})()"
                 cond iftrue

let ppf_ifthenelse cond iftrue iffalse =
  Printf.sprintf "@[<v 2>@,if (%s) {@, %s @,} else {@, %s @,} @]@,"
                 cond iftrue iffalse

let ppf_sequence exp1 exp2 =
  Printf.sprintf "%s;@,%s"
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
  Some (Printf.sprintf "%s: %s" tag value)

(* deprecated:
  let expanded_constructors = map_cstr_fields (*~loc*) ppf_cstr cd args in
*)

let ppf_cstrs styp cstr_name rest =
  let comma = if rest = "" then "" else "," in
  let styp_full =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_unlogged -> ""
    | Mode_line_token
    | Mode_logged -> Printf.sprintf "type: \"%s\", " styp
    in
  Printf.sprintf "@[<v 2>{%stag: \"%s\"%s %s}@]"
    styp_full cstr_name comma rest

let ppf_cstrs_fct cstr_fullname args =
   ppf_apply cstr_fullname (show_list ", " args)

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




(****************************************************************)
(* FRESH ID NAMES *)

let id_fresh =
  let r = ref 0 in
  fun prefix -> (incr r; prefix ^ string_of_int !r)


(****************************************************************)
(* FRESH TOKEN NAMES *)

let token_basename_ref = ref "no_token_basename_registered"

let token_register_basename basename =
  token_basename_ref := basename

let token_fresh =
  let r = ref 0 in
  fun () -> (incr r; 
    let token_start = Printf.sprintf "#<%d#" !r in
    let token_stop = Printf.sprintf "#%d>#" !r in
    let token_lineof = Printf.sprintf "lineof(\"%s.js\", %d)" !token_basename_ref !r in  
    (token_start, token_stop, token_lineof))


(****************************************************************)
(* CONTEXTS *)

(** Fresh name generator for contexts *)

let ctx_fresh =
  let r = ref 0 in
  fun () -> (incr r; "ctx_" ^ string_of_int !r)

let ctx_initial =
  "ctx_empty"


(****************************************************************)
(* LOGGED CONSTRUCTORS *)

let generate_logged_case spat binders ctx newctx sbody need_break =
  (* Note: binders is a list of pairs of id *)
  (* Note: if binders = [], then newctx = ctx *)
  let (token_start, token_stop, token_lineof) = token_fresh() in
  let (shead, sintro) =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_line_token -> 
      (token_start, token_stop)
    | Mode_logged ->
      let ids = List.map fst binders in
      let mk_binding x =
        Printf.sprintf "{key: \"%s\", val: %s}" x x
      in
      let bindings =
        Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding ids))
      in 
      let spreintro =
        if binders = [] then ""
        else Printf.sprintf "var %s = ctx_push(%s, %s);@," newctx ctx bindings
      in
      let sintro = Printf.sprintf "%slog_event(%s, %s, \"case\");@,"
        spreintro token_lineof newctx in
      ("", sintro)
    | Mode_unlogged -> ("", "")
    in
  let sbinders = ppf_match_binders binders in
  (Printf.sprintf "@[<v 0>%s%s:@;<1 2>@[<v 0>%s%s%s%s@]@]"
     shead spat sbinders sintro sbody
     (if need_break then "@,break;" else ""))


(* generate_logged_case implement using
[insertCaseCode(caseBody,bindings,ctx,newctx,sbody)]
£4424;caseBody;codeOf(bindings);sbody;break
case(caseBody); codeOf(bindings); newctx=ctx_push(ctx,bindings); logEvent(LINEOF(432423), "case", newctx);sbody;break

with help of

  if binders = [] then L.log_line (ppf_branch spat binders se) [(L.Exit)]
  else
    let typ = match List.rev (Str.split (Str.regexp " ") spat) with
      | [] -> assert false
      | x :: xs -> String.sub x 0 (String.length x)
    in L.log_line (ppf_branch spat binders se) [(L.Exit); (L.ReturnStrip); (L.Add (binders, typ))]

*)

(* LATER: optimize return when it's a value *)

let generate_logged_return ctx sbody = 
  let (token_start, token_stop, token_lineof) = token_fresh() in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_line_token ->
     Printf.sprintf "%sreturn %s; %s" token_start sbody token_stop
  | Mode_logged ->
    let id = id_fresh "_return_" in
    Printf.sprintf "var %s = %s;@,log_event(%s, ctx_push(%s, {\"return_value\", %s}), \"return\");@,return %s; "
      id sbody token_lineof ctx id id
  | Mode_unlogged -> 
     Printf.sprintf "return %s; " sbody
     (* Printf.sprintf "@[<v 0>return %s;@]" sbody *)
(*
----
  [insertReturnCode(e,ctx)]

TOKEN(432423);return e

var t=e; logEvent(LINEOF(432423), ctx_push(ctx, {"return",t}), "return"); return t
----
*)



let generate_logged_let ids ctx newctx sdecl sbody =
  let (token_start, token_stop, token_lineof) = token_fresh() in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_line_token ->
     Printf.sprintf "%s%s%s@,%s" token_start sdecl token_stop sbody  
  | Mode_logged ->
    let mk_binding x =
      Printf.sprintf "{key: \"%s\", val: %s}" x x
    in
    let bindings =
      Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding ids))
    in 
    Printf.sprintf "%s@,var %s = ctx_push(%s, %s);@,log_event(%s, %s, \"let\");@,%s@,"
      sdecl newctx ctx bindings token_lineof newctx sbody
  | Mode_unlogged -> 
     Printf.sprintf "%s@,%s" sdecl sbody

(*

----
  [insertLetCode(x,e,ctx,newctx,sbody)]

TOKEN(432423);var x = e;sbody

var x=e; var newctx=ctx_push(ctx,x,e); logEvent(LINEOF(432423), "let", ctx);sbody
----
*)

(* LATER: factoriser les bindings *)

let generate_logged_enter arg_ids ctx newctx sbody = 
  let (token_start, token_stop, token_lineof) = token_fresh() in
  let (shead1, shead2, sintro) =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_line_token -> (token_start, token_stop, "")
    | Mode_logged ->
      let mk_binding x =
        Printf.sprintf "{key: \"%s\", val: %s}" x x
      in
      let bindings =
        Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding arg_ids))
      in 
      let sintro = Printf.sprintf "var %s = ctx_push(%s, %s);@,log_event(%s, %s, \"enter\");@,"
        newctx ctx bindings token_lineof newctx in
      ("", "", sintro)
    | Mode_unlogged -> ("", "", "")
  in
  let args = String.concat ", " arg_ids in
  Printf.sprintf "%sfunction (%s)%s {@;<1 2>@[<v 0>%s%s@]@,}" shead1 args shead2 sintro sbody

(*

----
function(x,y) {
  [isnertEnterCode(bindings,ctx,newctx)]fdqfdsf
  }

TOKEN(432423);sbody

var newctx = ctx_push(bindings);
logEvent(LINEOF(432423), newctx, "enter");sbody
----

may reuse 
    ppf_function args body

*)



(****************************************************************)
(* DESTINATIONS *)

(** Destination-style translation of expressions *)

type dest = 
  | Dest_ignore
  | Dest_return
  | Dest_assign of string
  | Dest_inline

let apply_dest ctx dest sbody =
  match dest with
  | Dest_ignore -> sbody
  | Dest_return -> generate_logged_return ctx sbody
  | Dest_assign id -> Printf.sprintf "var %s = %s;" id sbody
  | Dest_inline -> sbody

(* LATER: pull out the "var" out of switch *)

exception Not_good_for_dest_inline

let reject_inline dest =
  if dest = Dest_inline then raise Not_good_for_dest_inline


(****************************************************************)
(* TRANSLATION *)

let rec js_of_structure s =
  show_list_f (fun strct -> js_of_structure_item strct) "@,@," s.str_items

and js_of_submodule m =
  let loc = m.mod_loc in
  match m.mod_desc with
  | Tmod_structure  s -> ppf_module (js_of_structure s)
  | Tmod_functor (id, _, mtyp, mexp) -> ppf_function (ppf_ident id) (js_of_submodule mexp)
  | Tmod_apply (m1, m2, _) -> ppf_apply (js_of_submodule m1) (js_of_submodule m2)
  | Tmod_ident (p,_) -> ppf_path p
  | Tmod_constraint _ -> out_of_scope loc "module constraint"
  | Tmod_unpack     _ -> out_of_scope loc "module unpack"

and show_value_binding ctx vb = (* dest is Ignore *)
  js_of_let_pattern ctx vb.vb_pat vb.vb_expr

and js_of_structure_item s =
  let loc = s.str_loc in
  match s.str_desc with
  | Tstr_eval (e, _)     -> Printf.sprintf "%s" @@ js_of_expression ctx_initial Dest_ignore e
  | Tstr_value (_, vb_l) -> String.concat "@,@," @@ List.map (fun vb -> 
     (* let (id, sdecl) = show_value_binding ctx_initial vb in *)
     Printf.sprintf "@\n@\n%s: %s," (ident_of_pat vb.vb_pat) (js_of_expression_inline_or_wrap ctx_initial vb.vb_expr))
     @@ vb_l
  | Tstr_type decls -> 
     (* function id( f1, f2) { return { typ: t, tag: x, "f1": f1, "f2": f2 } } *)
     String.concat "@,@," @@ (List.map (fun decl -> 
        match decl.typ_type.type_kind with
        | Type_variant cstr_decls ->
           let styp = decl.typ_name.txt in
           String.concat "@,@," @@ (List.map (fun (cd:Types.constructor_declaration) -> 
              let cstr_name = cd.Types.cd_id.Ident.name in
              let fields = extract_cstr_attrs_basic cstr_name cd.cd_attributes in
              let sargs = show_list ", " fields in
              let sbindings = map_filter_fields_elements ppf_cstr fields fields in
              let rest = show_list ", " sbindings in
              let sobj = ppf_cstrs styp cstr_name rest in 
              Printf.sprintf "function %s(%s) { return %s; }" cstr_name sargs sobj))
          @@ cstr_decls
        | _ -> ""))
     @@ decls
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

and js_of_branch ctx dest b eobj =
  let spat, binders = js_of_pattern b.c_lhs eobj in
  let newctx = if binders = [] then ctx else ctx_fresh() in
  let sbody = js_of_expression newctx dest b.c_rhs in
  let need_break = (dest <> Dest_return) in
  generate_logged_case spat binders ctx newctx sbody need_break 
     
and js_of_expression_inline_or_wrap ctx e = 
  try 
    js_of_expression ctx Dest_inline e
  with Not_good_for_dest_inline ->
    js_of_expression_wrapped ctx e

and js_of_expression_wrapped ctx e = (* dest = Dest_return *)
  ppf_lambda_wrap (js_of_expression ctx Dest_return e)

and js_of_expression_naming_argument_if_non_variable ctx obj name_prefix = 
  match obj.exp_desc with
  | Texp_ident (_, ident,  _) -> 
      "", (js_of_longident ident)
  | _ ->  (* generate  var id = sexp;  *)
      let id = id_fresh "_switch_arg_" in
      let sintro = js_of_expression ctx (Dest_assign id) obj in
      (sintro ^ "@,"), id

and js_of_expression ctx dest e =
  let inline_of_wrap = js_of_expression_inline_or_wrap ctx in (* shorthand *)
  let loc = e.exp_loc in
  match e.exp_desc with

  | Texp_ident (_, ident,  _) -> 
      let sexp = js_of_longident ident in
      apply_dest ctx dest sexp

  | Texp_constant c -> 
      let sexp = js_of_constant c in
      apply_dest ctx dest sexp

  | Texp_let   (_, vb_l, e) ->
    reject_inline dest;
    let (ids, sdecl) = begin match vb_l with  
      | [ { vb_pat = { pat_desc = Tpat_tuple el }; vb_expr = obj } ] -> (* binding tuples *)
         let (sintro, seobj) = js_of_expression_naming_argument_if_non_variable ctx obj "_switch_arg_" in     
         let bind i var = 
            match var.pat_desc with
            | Tpat_var (id, _) -> 
                let sid = ppf_ident id in
                (sid, Printf.sprintf "%s[%d]" seobj i)
            | Tpat_any -> out_of_scope var.pat_loc "Underscore pattern in let tuple"
            | _ -> out_of_scope var.pat_loc "Nested pattern matching"
            in
          let binders = List.mapi bind el in
          let ids = List.map fst binders in
          let sdecl = ppf_match_binders binders in
          (ids, sdecl)
      | _ -> (* other cases *)
        let (ids,sdecls) = List.split (List.map (fun vb -> show_value_binding ctx vb) @@ vb_l) in
        let sdecl = String.concat lin1 @@ sdecls in
        (ids, sdecl)
      end in
    let newctx = ctx_fresh() in
    let sbody = js_of_expression newctx dest e in
    let sexp = generate_logged_let ids ctx newctx sdecl sbody in
    sexp

  | Texp_function (_, c :: [], Total) ->
    let rec explore pats e = match e.exp_desc with
      | Texp_function (_, c :: [], Total) ->
        let (p, e) = (c.c_lhs, c.c_rhs) in 
        explore (p :: pats) e
      | _ ->
        List.map ident_of_pat @@ List.rev @@ pats, e 
      in
    let arg_ids, body = explore [c.c_lhs] c.c_rhs in
    let newctx = ctx_fresh() in
    let sbody = js_of_expression newctx Dest_return body in
    let sexp = generate_logged_enter arg_ids ctx newctx sbody in
    apply_dest ctx dest sexp

  | Texp_apply (f, exp_l) ->
     let sl' = exp_l  (* only used to know if infix *)
               |> List.map (fun (_, eo, _) -> match eo with 
                                              | None -> out_of_scope loc "optional apply arguments"
                                              | Some ei -> ei) in
     let sl_clean = exp_l
              |> List.map (fun (_, eo, _) -> match eo with 
                                             | None -> out_of_scope loc "optional apply arguments" 
                                             | Some ei -> ei) in
     let sl = sl_clean |> List.map (fun ei -> inline_of_wrap ei) in
     let se = inline_of_wrap f in
     let sexp = 
        if is_infix f sl' && List.length exp_l = 2
           then ppf_apply_infix se (List.hd sl) (List.hd (List.tl sl))
           else ppf_apply se (String.concat ", " sl)
        in
     apply_dest ctx dest sexp

  | Texp_match (obj, l, [], Total) ->
     reject_inline dest;
     let (sintro, seobj) = js_of_expression_naming_argument_if_non_variable ctx obj "_switch_arg_" in     
     let sb = String.concat "@," (List.map (fun b -> js_of_branch ctx dest b seobj) l) in
     let const = exp_type_is_constant obj in
     let sexp = sintro ^ (ppf_match seobj sb const) in
     sexp

  | Texp_tuple (tl) -> 
     let sexp = ppf_tuple @@ show_list_f (fun exp -> inline_of_wrap exp) ", " tl in
     apply_dest ctx dest sexp

  | Texp_construct (p, cd, el) ->
    let cstr_fullname = string_of_longident p.txt in
    let cstr_name = cd.cstr_name in
    (*let styp = string_of_type_exp e.exp_type in*)
    let sexp =
      if is_sbool cstr_name then cstr_name else
      if is_unit cstr_name then unit_repr else
        begin
          let expr_strs = List.map (fun exp -> inline_of_wrap exp) el in
          ppf_cstrs_fct cstr_fullname expr_strs
        end in
    apply_dest ctx dest sexp

  | Texp_array      (exp_l)           -> ppf_array @@ show_list_f (fun exp -> inline_of_wrap exp) ", " exp_l
  | Texp_ifthenelse (e1, e2, None)    -> out_of_scope loc "if without else"
    (* ppf_ifthen (js_of_expression e1) (js_of_expression e2) *)
  | Texp_ifthenelse (e1, e2, Some e3) ->
     reject_inline dest;
     ppf_ifthenelse (inline_of_wrap e1) (js_of_expression ctx dest e2) (js_of_expression ctx dest e3)
  | Texp_sequence (e1, e2) -> 
     ppf_sequence (inline_of_wrap e1) (js_of_expression ctx dest e2)
  | Texp_while      (cd, body)        -> out_of_scope loc "while"
    (* ppf_while (js_of_expression cd) (js_of_expression body) *)
  | Texp_for        (id, _, st, ed, fl, body) -> out_of_scope loc "for"
    (* ppf_for (ppf_ident id) (js_of_expression st) (js_of_expression ed) fl (js_of_expression body) *)
  | Texp_record     (llde,_)          -> ppf_record (List.map (fun (_, lbl, exp) -> (lbl.lbl_name, inline_of_wrap exp)) llde)
  | Texp_field      (exp, _, lbl)     -> ppf_field_access (inline_of_wrap exp) lbl.lbl_name
  | Texp_assert      e                -> 
      let sexp = inline_of_wrap e in
      Printf.sprintf "throw %s;" sexp

  | Texp_function (label, cases, Total) when label = "" -> 
      let mk_pat pat_des =
        { pat_desc = pat_des;
          pat_loc = e.exp_loc;
          pat_extra = [];
          pat_type = e.exp_type;
          pat_env = e.exp_env;
          pat_attributes = [];
         } in
      let mk_exp exp_desc =
         { exp_desc = exp_desc;  
           exp_loc = e.exp_loc;
           exp_extra = [];
           exp_type = e.exp_type;
           exp_env = e.exp_env;
           exp_attributes = [];
         } in
      let name = "_fun_arg_" in
      let arg = Ident.create name in
      let thearg_lident = { txt = Longident.Lident name; loc = Location.none } in
      let thearg = mk_exp (Texp_ident (Path.Pident arg, thearg_lident, Obj.magic ())) in
      let thecase = {  
           c_lhs = mk_pat (Tpat_var (arg, Location.mknoloc name));
           c_guard = None;
           c_rhs = mk_exp (Texp_match (thearg, cases, [], Total));
          } in
      let exp = mk_exp (Texp_function (label, [thecase], Total)) in
      js_of_expression ctx dest exp

  | Texp_match      (_,_,_, Partial)  -> out_of_scope loc "partial matching"
  | Texp_match      (_,_,_,_)         -> out_of_scope loc "matching with exception branches"
  | Texp_try        (_,_)             -> out_of_scope loc "exceptions"
  | Texp_function (_, _, _) -> out_of_scope loc "use of labels"

  | Texp_variant    (_,_)             -> out_of_scope loc "polymorphic variant"
  | Texp_setfield   (_,_,_,_)         -> out_of_scope loc "setting field"
  | Texp_send       (_,_,_)           -> out_of_scope loc "objects"
  | Texp_new        (_,_,_)           -> out_of_scope loc "objects"
  | Texp_instvar    (_,_,_)           -> out_of_scope loc "objects"
  | Texp_setinstvar (_,_,_,_)         -> out_of_scope loc "objects"
  | Texp_override   (_,_)             -> out_of_scope loc "objects"
  | Texp_letmodule  (_,_,_,_)         -> out_of_scope loc "local modules"
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
  match String.concat "." @@ Longident.flatten loc.txt with
  | "()"  -> unit_repr
  | "+."  -> "+"
  | "*."  -> "*"
  | "-."  -> "-"
  | "~-." -> "-"
  | "/."  -> "/"
  | "="   -> "=="
  | res   -> ppf_ident_name res

and ident_of_pat pat = match pat.pat_desc with
  | Tpat_var (id, _) -> ppf_ident id
  | Tpat_any         -> id_fresh "_pat_any_"
  | _ -> error ~loc:pat.pat_loc "functions can't deconstruct values"

(* returns the name bound and the code that assigns a value to this name *)
and js_of_let_pattern ctx pat expr =
  let id = 
    match pat.pat_desc with
    | Tpat_var (id, _) -> ppf_ident id
    | Tpat_any -> Printf.printf "warning: unsupported let-any\n"; ""
    | Tpat_alias _ -> Printf.printf "warning: unsupported let-alias\n"; ""
    | Tpat_constant _ -> Printf.printf "warning: unsupported let-constant\n"; ""
    | Tpat_tuple _ -> Printf.printf "warning: unsupported let-tuple\n"; ""
    | Tpat_construct _ -> Printf.printf "warning: unsupported let-construct\n"; ""
    | Tpat_variant _ -> Printf.printf "warning: unsupported let-variant\n"; ""
    | Tpat_record _ -> Printf.printf "warning: unsupported let-record\n"; ""
    | Tpat_array _ -> Printf.printf "warning: unsupported let-array\n"; ""
    | Tpat_or _ -> Printf.printf "warning: unsupported let-or\n"; ""
    | Tpat_lazy _ -> Printf.printf "warning: unsupported let-lazy\n"; ""
      (*  error ~loc:pat.pat_loc "let can't deconstruct values"  *)
    in
  (id, js_of_expression ctx (Dest_assign id) expr)

  (* LATER: for   let (x,y) = e,  encode as  translate(e,assign z); x = z[0]; y=z[1] 
    | Tpat_tuple (pat_l)
    | Tpat_array (pat_l) ->
       let l = List.map
                 (function pat ->
                           match pat.pat_desc with
                           | Tpat_var (id, _) -> (ppf_ident id, string_of_type_exp pat.pat_type)
                           | _ -> out_of_scope pat.pat_loc "nested pattern-matching in tuples or arrays"
                 ) pat_l in
       ppf_pat_array l sexpr
       *)

(* [js_of_pattern] translates a pattern to a "case" statement of a switch,
   and a list of assignements of variables (pairs of identifier and body).
   Nested patterns are not supported.
   It returns a pair: spat (the "case" instruction), binders (the assignements) *)
and js_of_pattern pat obj = 
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | Tpat_any -> 
     "default", []
  | Tpat_constant c -> 
     ppf_match_case (js_of_constant c), []
  | Tpat_construct (_, cd, el) ->
     let c = cd.cstr_name in
     let spat = if is_sbool c then ppf_match_case c else ppf_match_case ("\"" ^ c ^ "\"") in
     let bind field var = 
        match var.pat_desc with
        | Tpat_var (id, _) -> 
            Some (ppf_ident id, Printf.sprintf "%s.%s" obj field)
        | Tpat_any -> None
        | _ -> out_of_scope var.pat_loc "Nested pattern matching"
        in
      let binders = map_cstr_fields ~loc bind cd el in
      spat, binders
  | Tpat_var (id, _) -> unsupported ~loc "Tpat_var"
  | Tpat_tuple el -> unsupported ~loc "tuple matching, if not in a simple let-binding"
  | Tpat_array el -> unsupported ~loc "array-match"
  | Tpat_record (_,_) -> unsupported ~loc "record"
  | Tpat_or (_,_,_) -> error ~loc "or pattern not implemented yet"
  | Tpat_alias (_,_,_) -> out_of_scope loc "alias-pattern"
  | Tpat_variant (_,_,_) -> out_of_scope loc "polymorphic variants in pattern matching"
  | Tpat_lazy _ -> out_of_scope loc "lazy-pattern"

let to_javascript basename module_name typedtree =
  token_register_basename basename;
  let content = js_of_structure typedtree in
  let pre_res = ppf_module_wrap module_name content in
  let str_ppf = Format.str_formatter in
  Format.fprintf str_ppf (Scanf.format_from_string pre_res "");
  Format.flush_str_formatter ()


(****************************************************************)
(* COMMENTS *)

(*
ctx_empty
ctx_push(ctx, bindings)   where bindings = [ { key: "ls", val: ls}, { key:"xs", val:xs } ]

push("ls", ls, push("v", v, push("y", y, ctx314)); 

example:  
  ctx321 = ctx_push(ctx320, bindings); log(|line|, ctx321, "ctx_push")


  enter  (or call)   => arguments of the call + name of new ctx
  return (was exit)  => return value
  let (on the "in")  => new binding + name of new ctx
  case               => bound variables + name of new ctx







  type token_info = ctx_operation * current ctx

  
  if  ==> viewed as match with case true/false.


ctx_empty is passed on each structure_item
on each ctx extension, we need a fresh name (enter, let, match_branch)
(for return values, do the extension on the fly)

   
   return f(x);
translates as
   var v213 = f(x);
   log(|line|, ctx_push(ctx320, {key: "return", val: v213}), "return")



  match v with | None -> x | Some y -> y
translates as
  function() { 


  
----------------------
  let f ... =
    match ...

=> 
  switch
    case:
      return;

----------------------
  let f ... =
    match .. -> 
      match ...

=>
  return

----------------------
  let x = match ... in ...
=> 
  switch ...
    case:
      x = ..; break;
    case:
      x = ..; break;


----------------------
  let x = 
    match .. ->
      match .. ->
=> 
  would not work without wrapping

----------------------

  f (match ...)
=> 
  requires A-normalization

*)
