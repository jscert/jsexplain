open Misc
open Asttypes
open Types
open Typedtree
open Longident
open Format
open Print_type
open Location
open Lexing
open Mytools
open Attributes

let hashtlb_size = 256
let type_tbl = Hashtbl.create hashtlb_size

(**
 * Debug-purpose functions
 *)
  
let print_tbl () =
  let rec print_str_list = function
    | [] -> ""
    | x :: [] -> (Format.sprintf {|"%s"|} x)
    | x :: xs -> (Format.sprintf {|"%s", |} x) ^ print_str_list xs
  in Hashtbl.iter (fun cstr elems -> Printf.printf ({|%s -> [%s]|} ^^ "\n") cstr (print_str_list elems)) type_tbl; ()

(**
 * Useful functions (shadow show_list from Mytools)
 *)
    
let show_list_f f sep l = l
  |> List.map f
  |> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) ""

let show_list sep l =
  List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) "" l

let is_sbool x = List.mem x ["true" ; "false"] 
    
(**
 * Before-hand definitions of Pretty-Printer-Format for converting ocaml
 * to ECMAScript, therefore all of them are in a single place.
 *)
    
let ppf_branch case binders expr =
  Format.sprintf "@[<v 2>%s: @[<v 4>%s@,return %s;@]@,@]"
    case binders expr

let ppf_let_in decl exp =
  Format.sprintf "@[<v 0>(function () {@,@[<v 4>@,%s@,@,return %s;@,@]@,})()@]"
    decl exp

let ppf_function args body=
  Format.sprintf "@[function (%s) {@,@[<v 4>@,return %s;@,@]@,}@]"
    args body

let ppf_apply f args =
  Format.sprintf "@[<v 0>%s(%s)@]"
    f args

let ppf_match value cases =
  Format.sprintf "@[<v 0>(function () {@,@[<v 4>@,switch (%s.type) {@,@[<v 4>@,%s@,@]@,}@]@,})()@]"
    value cases

let ppf_array values =
  Format.sprintf "[%s]"
    values

let ppf_tuple = ppf_array
    
let ppf_ifthen cond iftrue =
  Format.sprintf "@[<v 0>(function () {@,@[<v 4>@,if (%s) {@,@[<v 4>@,return  %s;@]@,}@]@,})()@]"
    cond iftrue

let ppf_ifthenelse cond iftrue iffalse =
  Format.sprintf "@[<v 0>(function () {@,@[<v 4>@,if (%s) {@,@[<v 4>@,return  %s;@]@,} else {@,@[<v 4>@,return  %s;@]@,}@]@]@,})()@]"
    cond iftrue iffalse

let ppf_sequence exp1 exp2 =
  Format.sprintf "@[<v 0>return %s,@,%s@]"
    exp1 exp2

let ppf_while cd body =
  Format.sprintf "@[<v 0> function () {@,@[<v 3>@,while(%s) {@,@[<v 4>@,%s@]@]@,@]}@,)()@]"
    cd body
    
let ppf_for id start ed flag body =
  let fl_to_string = function
    | Upto   -> "++"
    | Downto -> "--" in
  let fl_to_symbl = function
    | Upto   -> "<="
    | Downto -> ">="
  in Format.sprintf "@[<v 0>(function () {@,@[<v 3>@,for (%s = %s ; %s %s %s ; %s%s) {@,@[@,%s @]@,} @,@]})() @]"
  id start id (fl_to_symbl flag) ed (fl_to_string flag)  id body

let ppf_single_cstr tag =
  Format.sprintf "%s"
    tag

let ppf_cstr tag value =
  Format.sprintf "%s: %s"
    tag value

let ppf_single_cstrs typ =
   Format.sprintf "{type: \"%s\"}"
     typ
  
    
let ppf_multiple_cstrs typ rest =
  Format.sprintf "{type: \"%s\", %s}"
    typ rest
    
(**
 * Main part
 *)
    
let rec show_value_binding vb =
  js_of_let_pattern vb.vb_pat vb.vb_expr
    
and js_of_structure s =
  show_list_f js_of_structure_item lin2 s.str_items
    
and js_of_structure_item s = match s.str_desc with
  | Tstr_eval (e, _)     -> Format.sprintf "%s" @@ js_of_expression e
  | Tstr_value (_, vb_l) -> String.concat lin2 @@ List.map show_value_binding @@ vb_l
  | Tstr_type tl ->
   let explore_type = function
      | [] -> ()
      | x :: xs ->
        (match x.typ_kind with
        | Ttype_variant cdl ->
          let cl = List.map (fun cstr -> extract_cstr_attrs cstr) cdl in
          List.iter (fun (name, cstrs_name) -> Hashtbl.add type_tbl name cstrs_name) cl
        | _ -> unsupported "open types, record and abstract type"
        )
   in explore_type tl; ""
  | Tstr_primitive  _ -> out_of_scope "primitive functions"
  | Tstr_typext     _ -> out_of_scope "type extensions"
  | Tstr_exception  _ -> out_of_scope "exceptions"
  | Tstr_module     _ -> out_of_scope "modules"
  | Tstr_recmodule  _ -> out_of_scope "recursive modules"
  | Tstr_modtype    _ -> out_of_scope "module type"
  | Tstr_open       _ -> out_of_scope "open statements"
  | Tstr_class      _ -> out_of_scope "objects"
  | Tstr_class_type _ -> out_of_scope "class types"
  | Tstr_include    _ -> out_of_scope "includes"
  | Tstr_attribute  _ -> out_of_scope "attributes"

and js_of_branch b obj =
  let spat, binders = js_of_pattern b.c_lhs obj in
  let se = js_of_expression b.c_rhs in
  ppf_branch spat binders se
    
and js_of_expression e = match e.exp_desc with
  | Texp_ident (_, loc,  _)           -> js_of_longident loc
  | Texp_constant c                   -> js_of_constant c
  | Texp_let   (_, vb_l, e)           ->
    let sd = String.concat lin1 @@ List.map show_value_binding @@ vb_l in
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
    let sl = exp_l
          |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope "optional apply arguments" | Some ei -> js_of_expression ei)
          |> String.concat ", " in
     let se = js_of_expression f in
     ppf_apply se sl
  | Texp_match (exp, l, [], Total) ->
     let se = js_of_expression exp in
     let sb = List.fold_left (fun acc x -> acc ^ js_of_branch x se) "" l in
     ppf_match se sb
  | Texp_tuple (tl)               -> ppf_tuple @@ show_list_f js_of_expression ", " tl
  | Texp_construct (loc, cd, el) ->
        let value = js_of_longident loc in
        if el = [] then
          if is_sbool value
          then value
          else ppf_single_cstr value
        else
          let rec expand_constructor_list fields exprs = match fields, exprs with
            | [], [] -> []
            | [], x :: xs | x :: xs , [] -> failwith "argument lists should have the same length."
            | x :: xs, y :: ys -> (if y = "" then ppf_single_cstr x else ppf_cstr x y) :: expand_constructor_list xs ys in
          let names = Hashtbl.find type_tbl value
             in ppf_multiple_cstrs value (show_list ", " (expand_constructor_list names (List.map js_of_expression el)))
  | Texp_array      (exp_l)           -> ppf_array @@ show_list_f js_of_expression ", " exp_l
  | Texp_ifthenelse (e1, e2, None)    -> ppf_ifthen (js_of_expression e1) (js_of_expression e2)
  | Texp_ifthenelse (e1, e2, Some e3) -> ppf_ifthenelse (js_of_expression e1) (js_of_expression e2) (js_of_expression e3)
  | Texp_sequence   (e1, e2)          -> ppf_sequence (js_of_expression e1) (js_of_expression e2)
  | Texp_while      (cd, body)        -> ppf_while (js_of_expression cd) (js_of_expression body)
  | Texp_for        (id, _, st, ed, fl, body) -> ppf_for (Ident.name id) (js_of_expression st) (js_of_expression ed) fl (js_of_expression body)
  | Texp_match      (_,_,_, Partial)  -> out_of_scope "partial matching"
  | Texp_match      (_,_,_,_)         -> out_of_scope "matching with exception branches"
  | Texp_try        (_,_)             -> out_of_scope "exceptions"
  | Texp_function   (_,_,_)           -> out_of_scope "powered-up functions"
  | Texp_variant    (_,_)             -> out_of_scope "polymorphic variant"
  | Texp_record     (_, _)            -> out_of_scope "records"
  | Texp_field      (_,_,_)           -> out_of_scope "accessing field"
  | Texp_setfield   (_,_,_,_)         -> out_of_scope "setting field"
  | Texp_send       (_,_,_)           -> out_of_scope "objects"
  | Texp_new        (_,_,_)           -> out_of_scope "objects"
  | Texp_instvar    (_,_,_)           -> out_of_scope "objects"
  | Texp_setinstvar (_,_,_,_)         -> out_of_scope "objects"
  | Texp_override   (_,_)             -> out_of_scope "objects"
  | Texp_letmodule  (_,_,_,_)         -> out_of_scope "local modules"
  | Texp_assert      _                -> out_of_scope "assert"
  | Texp_lazy        _                -> out_of_scope "lazy expressions"
  | Texp_object     (_,_)             -> out_of_scope "objects"
  | Texp_pack        _                -> out_of_scope "packing"
    
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
  if res = "()" then "undefined" else res

and ident_of_pat pat = match pat.pat_desc with
  | Tpat_var (id, _) -> Ident.name id
  | _ -> error "functions can't deconstruct values"
    
and js_of_let_pattern pat expr = 
  let expr_type pat expr = match expr.exp_desc with
    | Texp_construct (loc, cd, el) ->
       let value = js_of_longident loc in
       if el = [] then
         if is_sbool value then value else ppf_single_cstr value
       else
         let rec expand_constructor_list fields exprs = match fields, exprs with
           | [], [] -> []
           | [], x :: xs | x :: xs , [] -> failwith "argument lists should have the same length."
           | x :: xs, y :: ys ->  ppf_cstr  x y :: expand_constructor_list xs ys in
         let names = Hashtbl.find type_tbl value
         in ppf_multiple_cstrs value (show_list ", " (expand_constructor_list names (List.map js_of_expression el)))
    | _ -> string_of_type_exp pat.pat_type in
  let sexpr = js_of_expression expr in
  match pat.pat_desc with
  | Tpat_var (id, _) ->
     Format.sprintf "@[<v 0>var %s = %s;@,@]" (Ident.name id) sexpr
  | Tpat_tuple (pat_l)
  | Tpat_array (pat_l) ->
     let l = List.map (function pat -> match pat.pat_desc with
                                       | Tpat_var (id, _) -> (Ident.name id, string_of_type_exp pat.pat_type)
                                       | _ -> out_of_scope "pattern-matching in arrays") pat_l in
     Format.sprintf "@[<v 0>var __%s = %s;@,@]" "array" sexpr ^
       List.fold_left2 (fun acc (name, exp_type) y -> acc ^ Format.sprintf "@[<v 0>var %s = __%s[%d];@,@]" name "array" y)
                       "" l @@ range 0 (List.length l - 1)
  | _ -> error "let can't deconstruct values"

and js_of_pattern pat obj = match pat.pat_desc with
  | Tpat_any -> "default", ""
  | Tpat_constant c -> js_of_constant c, ""
  | Tpat_var (id, _) -> Ident.name id, ""
  | Tpat_alias (_,_,_) -> out_of_scope "alias-pattern"
  | Tpat_tuple (_) -> out_of_scope "tuple matching"
  | Tpat_construct (loc, cd, el) ->
     let c = js_of_longident loc in
     let spat = Format.sprintf "%s" ("case \"" ^ c ^ "\"") in
     let params = Hashtbl.find type_tbl c in
     let binders =
       if List.length el = 0 then Format.sprintf ""
       else Format.sprintf "%s@," ("var " ^ show_list ", " (List.map2 (fun x y -> x ^ " = " ^ obj ^ "." ^ y) (List.map (fun x -> fst (js_of_pattern x obj)) el) params) ^ ";") in
     spat, binders
  | Tpat_variant (_,_,_) -> out_of_scope "polymorphic variants in pattern matching"
  | Tpat_array (_) -> out_of_scope "array-match"
  | Tpat_record (_,_) -> out_of_scope "record"
  | Tpat_or (_,_,_) -> failwith "not implemented yet"
  | Tpat_lazy (_) -> out_of_scope "lazy-pattern"
