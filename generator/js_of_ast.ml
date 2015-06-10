open Misc
open Asttypes
open Types
open Typedtree
open Mytools
open Longident
open Format
open Print_type
open Location
open Lexing

let unsupported s =
    failwith ("unsupported language construction: " ^ s ^ ".")

and out_of_scope s =
    failwith (s ^ " are and will not be supported.")

and error s =
    failwith ("error: " ^ s ^ ".")


let show_list_f f sep l = l
 |> List.map f
 |> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) ""

let show_list sep l =
 List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) "" l

let js_of_constant = function
  | Const_int n -> string_of_int n
  | Const_char c -> String.make 1 c
  | Const_string (s, _) -> "\"" ^ s ^ "\""
  | Const_float f -> f
  | Const_int32 n -> Int32.to_string n
  | Const_int64 n -> Int64.to_string n
  | Const_nativeint n -> Nativeint.to_string n

let js_of_longident loc =
  let res = String.concat "." @@ Longident.flatten loc.txt in
  if res = "()" then "" else res

let js_of_let_pattern pat = match pat.pat_desc with
  | Tpat_var (id, _) -> (Ident.name id, string_of_type_exp pat.pat_type)
  | _ -> error "let can't deconstruct values"


let ident_of_pat pat = match pat.pat_desc with
  | Tpat_var (id, _) -> Ident.name id
  | _ -> error "functions can't deconstruct values"

let rec js_of_pattern pat = match pat.pat_desc with
  | Tpat_any -> "default"
  | Tpat_constant c -> js_of_constant c
  | Tpat_var (id, _) -> "case " ^ Ident.name id
  | Tpat_alias (_,_,_) -> out_of_scope "alias-pattern"
  | Tpat_tuple (_) -> out_of_scope "tuple matching"
  | Tpat_construct (loc, cd, el) ->
      let c = js_of_longident loc in
        if el = [] then c
        else if List.length el = 1 then (c ^ " " ^ js_of_pattern (List.hd el))
        else Format.sprintf "%s (%s)" c @@ show_list_f js_of_pattern ", " el
  | Tpat_variant (label, None, _) -> "\"" ^ label ^ "\""
  | Tpat_array (_) -> out_of_scope "array-match"
  | Tpat_record (_,_) -> out_of_scope "record"
  | Tpat_or (_,_,_) -> failwith "not implemented yet"
  | Tpat_lazy (_) -> out_of_scope "lazy-pattern"

and js_of_expression (e:expression) =
  let js_of_branch b =
    let spat = js_of_pattern b.c_lhs in
    let se = js_of_expression b.c_rhs in
    Format.sprintf "%s : %s; break;" spat se in
  match e.exp_desc with
  | Texp_ident (_, loc, _) -> js_of_longident loc
  | Texp_constant c -> js_of_constant c
  | Texp_let (_, vb_l, e) ->
      let show_val vb =
        let id, id_type = js_of_let_pattern vb.vb_pat in
        let expr = js_of_expression vb.vb_expr in
        Format.sprintf "var %s = {tag: \"%s\", val: %s};" id id_type expr in
      let sd = String.concat "\n" @@ List.map show_val @@ vb_l in
      let se = js_of_expression e in
      Format.sprintf
        "(function () {
            %s

            return %s;
        })()" sd se
  | Texp_function (_, c :: [], Total) ->
      let rec explore pats e = match e.exp_desc with
        | Texp_function (_, c :: [], Total) ->
            let p = c.c_lhs
            and e = c.c_rhs in
            explore (p :: pats) e
        | _ -> String.concat ", " @@ List.map ident_of_pat @@ List.rev @@ pats,
               js_of_expression e in
      let names, body = explore [c.c_lhs] c.c_rhs in
      Format.sprintf
        "function (%s) {
            return %s;
        }
        " names body
  | Texp_function (_, _, Partial) -> out_of_scope "partial functions"
  | Texp_apply (f, exp_l) ->
     let sl = exp_l
          |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope "optional apply arguments" | Some ei -> js_of_expression ei)
          |> String.concat ", " in
     let se = js_of_expression f in
     Format.sprintf "%s.val(%s);" se sl
  | Texp_match (exp, l, [], Total) ->
     let se = js_of_expression exp in
     let sb = List.fold_left (fun acc x -> acc ^ js_of_branch x ^ ";") "" l in
    Format.sprintf "switch (%s.tag) {
        %s;
     }" se sb
  | Texp_match (_, _, _, _) -> out_of_scope "exception branches or partial matching"
  | Texp_try (_, _) -> out_of_scope "exceptions"
  | Texp_tuple (_) -> failwith "not implemented yet"
  | Texp_construct (loc, cd, el) ->
      let c = js_of_longident loc in
      if el = [] then c
      else if List.length el = 1 then (c ^ " " ^ js_of_expression (List.hd el))
      else Format.sprintf "%s (%s)" c @@ show_list_f js_of_expression ", " el
  | Texp_variant (_,_) -> ""(* Nothing to do *)
  | Texp_record (_, _) -> failwith "rnot implemented yet"
  | Texp_field (_,_,_) -> failwith "fnot implemented yet"
  | Texp_setfield (_,_,_,_) -> failwith "not implemented yet"
  | Texp_array (_) -> out_of_scope "arrays"
  | Texp_ifthenelse (_, _, _) -> failwith "not implemented yet"
  | Texp_sequence (_, _) -> unsupported "sequences"
  | Texp_while (_, _) -> unsupported "while loops"
  | Texp_for (_,_,_,_,_,_) -> unsupported "for loops"
  | Texp_send (_, _, _) -> out_of_scope "objects"
  | Texp_new (_, _, _) -> out_of_scope "objects"
  | Texp_instvar (_,_,_) -> out_of_scope "objects"
  | Texp_setinstvar (_,_,_,_) -> out_of_scope "objects"
  | Texp_override (_,_) -> out_of_scope "objects"
  | Texp_letmodule (_,_,_,_) -> out_of_scope "local modules"
  | Texp_assert (_) -> out_of_scope "assert"
  | Texp_lazy (_) -> out_of_scope "lazy expressions"
  | Texp_object (_, _) -> out_of_scope "objects"
  | Texp_pack (_) -> out_of_scope "packing"
let rec js_of_structure s = show_list_f js_of_structure_item "\n\n" s.str_items
and js_of_structure_item s = match s.str_desc with
  | Tstr_eval (e, _) -> Format.sprintf "%s" @@ js_of_expression e
  | Tstr_value (_, vb_l)  ->
    let show_val vb =
        let id, id_type = js_of_let_pattern vb.vb_pat in
        let expr = js_of_expression vb.vb_expr in
        Format.sprintf "var %s = {tag: \"%s\", val: %s};" id id_type expr in
    let s = List.map show_val vb_l in
    show_list "\n\n" s
  | Tstr_type (_)         -> "" (* Nothing to do; tag rules *)
  | Tstr_primitive (_)    -> out_of_scope "primitive functions"
  | Tstr_typext (_)       -> out_of_scope "type extensions"
  | Tstr_exception (_)    -> out_of_scope "exceptions"
  | Tstr_module (_)       -> out_of_scope "modules"
  | Tstr_recmodule (_)    -> out_of_scope "recursive modules"
  | Tstr_modtype (_)      -> out_of_scope "module type"
  | Tstr_open (_)         -> out_of_scope "open statements"
  | Tstr_class (_)        -> out_of_scope "objects"
  | Tstr_class_type (_)   -> out_of_scope "class types"
  | Tstr_include (_)      -> out_of_scope "includes"
  | Tstr_attribute (_)    -> out_of_scope "attributes"
