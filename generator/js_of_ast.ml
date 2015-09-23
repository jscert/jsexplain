open Asttypes
open Attributes
open Env
open Format
open Lexing
open Log
open Longident
open Misc
open Mytools
open Parse_type
open Print_type
open Types
open Typedtree
  
let hashtbl_size = 256

let record_tbl   = Hashtbl.create hashtbl_size
let module_list  = ref []
let module_code  = ref []
let module_created = ref []
module L = Logged (Token_generator) (struct let size = 256 end)

(**
 * Debug-purpose functions
 *)

let env_diff_names env1 env2 =
  List.map Ident.unique_name (Env.diff env1 env2)

(**
 *  Functions to work with environment
 **)

let rec list_of_ident_from_summary = function
  | Env_empty -> []
  | Env_value (sum, id, vd) -> id :: list_of_ident_from_summary sum
  | Env_type (sum,_,_)
  | Env_extension (sum,_,_)
  | Env_module (sum,_,_)
  | Env_modtype (sum,_,_)
  | Env_class (sum,_,_)
  | Env_cltype (sum,_,_)
  | Env_open (sum,_)
  | Env_functor_arg (sum,_) -> list_of_ident_from_summary sum

let print_name_list l =
  let rec aux = function
    | [] -> ""
    | x :: [] -> x
    | x :: xs -> x ^ ", " ^ aux xs
  in "[ " ^ aux l ^ " ]"

let print_env env =
  let idents = env
               |> Env.summary
               |> list_of_ident_from_summary
               |> List.map Ident.name in
  Printf.printf "env: %s\n" (print_name_list idents)
                            
(**
 * Useful functions (Warning: shadows `show_list' from Mytools)
 *)
    
let show_list_f f sep l = l
  |> List.map f
  |> List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) ""

let show_list sep l =
  List.fold_left (fun acc x -> acc ^ (if acc = "" then "" else sep) ^ x) "" l

let is_sbool x = List.mem x ["true" ; "false"] 

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
  Printf.sprintf "function (%s) {@;<1 2>@[<v 0>return %s;@]@,}"
                 args body

let ppf_apply f args =
  Printf.sprintf "%s(%s)"
                 f args

let ppf_apply_infix f arg1 arg2 =
  Printf.sprintf "%s %s %s"
                 arg1 f arg2
    
let ppf_match value cases =
  let s =
    Printf.sprintf "switch (%s.type) {@,@[<v 0>%s@]@,}"
                   value cases
  in ppf_lambda_wrap s

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
  Printf.sprintf "%s: %s"
    tag value

let ppf_single_cstrs typ =
   Printf.sprintf "@[<v 2>{type: \"%s\"}@]"
     typ
      
let ppf_multiple_cstrs typ rest =
  Printf.sprintf "@[<v 2>{type: \"%s\", %s}@]"
    typ rest

let ppf_record llde =
  let rec aux acc = function
    | []               -> Printf.sprintf "@[<v 2>{@;<1 2>%s@]@,}" acc
    | (lbl, exp) :: [] -> aux (acc ^ Printf.sprintf "%s: %s" lbl exp) []
    | (lbl, exp) :: xs -> aux (acc ^ Printf.sprintf "%s: %s,@," lbl exp) xs
  in aux "" llde

let ppf_decl ?(mod_gen=[]) id expr =
  let assign_op, decl_kw, end_mark = if mod_gen = [] then " = ", "var ", ";" else ": ", "", "," in 
  Printf.sprintf "@[<v 0>%s%s%s%s%s@,@]" 
    decl_kw id assign_op expr end_mark

let ppf_pat_array id_list array_expr =
  Printf.sprintf "var __%s = %s;@," "array" array_expr ^
    List.fold_left2 (fun acc (name, exp_type) y -> acc ^ Printf.sprintf "@[<v 0>var %s = __%s[%d];@,@]" name "array" y)
                    "" id_list @@ range 0 (List.length id_list - 1)

(**
 * Module managment part
 *)

(** Return tuple of module name and path to module **)
let find_module_path mod_list =
  let open Config in
  let check_path name = find_in_path_uncap !load_path (name ^ ".ml") in
  try
    module_list := [];
    zip mod_list (List.map check_path mod_list)
  with Not_found -> failwith "Unbound module"

(** Return bool of whether a module has bee ncreated already **)
and not_already_created mod_name =
  not @@ List.exists ((=) mod_name) !module_created

(**
 * Main part
 *)

let rec js_of_structure ?(mod_gen=[]) old_env s =
  let new_env = s.str_final_env in
  show_list_f (fun strct -> js_of_structure_item ~mod_gen new_env strct) "@,@," s.str_items

and parse_modules ?(mod_gen=[]) = function
  | [] -> []
  | (name, path) :: xs ->
   let ppf = Format.std_formatter in
   let (opt, inputfile) = process_implementation_file ppf path in
   let ((parsetree1 : Parsetree.structure), typedtree1) =
      match opt with
      | None -> failwith ("Could not read and typecheck " ^ inputfile)
      | Some (parsetree1, (typedtree1, _)) -> parsetree1, typedtree1
      in
   let pre = js_of_structure ~mod_gen:(name :: mod_gen) Env.empty typedtree1 in
   Printf.sprintf "%s = {\n%s\n}" name pre :: parse_modules ~mod_gen xs

and show_value_binding ?(mod_gen=[]) vb =
  js_of_let_pattern ~mod_gen vb.vb_pat vb.vb_expr

and js_of_structure_item ?(mod_gen=[]) old_env s =
  let new_env = s.str_env in
  let loc = s.str_loc in
  match s.str_desc with
  | Tstr_eval (e, _)     -> Printf.sprintf "%s" @@ js_of_expression ~mod_gen new_env e
  | Tstr_value (_, vb_l) -> String.concat "@,@," @@ List.map (fun vb -> show_value_binding ~mod_gen vb) @@ vb_l
  | Tstr_type tl ->
   let create_type x =
      (match x.typ_kind with
        | Ttype_variant cdl -> () (* Do nothing, now using typedtree defs *)
        | Ttype_record ldl ->
          (* Beware silent shadowing for record labels *)
          List.iter (fun lbl -> Hashtbl.replace record_tbl (Ident.name lbl.ld_id) (Ident.name x.typ_id)) ldl
        | Ttype_abstract -> ()
        | _ -> unsupported "open types, record and abstract type")
   in List.iter create_type tl; ""
  | Tstr_open       od -> 
    let name = (fun od -> if od.open_override = Fresh then js_of_longident od.open_txt else "") od in
    if (name <> "" && not_already_created name) then
      module_list := name :: !module_list;

      (* Disable writing of .cmi files for modules we're opening to avoid automatically over-writing existing signature
       * with an inconsistent one *)
      let old_dont_write_files = !Clflags.dont_write_files in
      Clflags.dont_write_files := true;

      let new_mod = parse_modules ~mod_gen @@ find_module_path @@ !module_list in

      Clflags.dont_write_files := old_dont_write_files;

      module_created := name :: !module_created;
      module_code := new_mod @ !module_code;
    "" 
  | Tstr_primitive  _  -> out_of_scope loc "primitive functions"
  | Tstr_typext     _  -> out_of_scope loc "type extensions"
  | Tstr_exception  _  -> out_of_scope loc "exceptions"
  | Tstr_module     _  -> out_of_scope loc "modules"
  | Tstr_recmodule  _  -> out_of_scope loc "recursive modules"
  | Tstr_modtype    _  -> out_of_scope loc "module type"
  | Tstr_class      _  -> out_of_scope loc "objects"
  | Tstr_class_type _  -> out_of_scope loc "class types"
  | Tstr_include    _  -> out_of_scope loc "includes"
  | Tstr_attribute  attrs -> out_of_scope loc "attributes"

and js_of_branch ?(mod_gen=[]) old_env b obj =
  let spat, binders = js_of_pattern ~mod_gen b.c_lhs obj in
  let se = js_of_expression ~mod_gen old_env b.c_rhs in
  if binders = "" then ppf_branch spat binders se
  else
    let typ = match List.rev (Str.split (Str.regexp " ") spat) with
      | [] -> assert false
      | x :: xs -> String.sub x 0 (String.length x)
    in L.log_line (ppf_branch spat binders se) (L.Add (binders, typ))
    
and js_of_expression ?(mod_gen=[]) old_env e =
  let locn = e.exp_loc in
  let new_env = e.exp_env in
  match e.exp_desc with
  | Texp_ident (_, loc,  _)           -> js_of_longident loc
  | Texp_constant c                   -> js_of_constant c
  | Texp_let   (_, vb_l, e)           ->
    let sd = String.concat lin1 @@ List.map (fun vb -> show_value_binding ~mod_gen vb) @@ vb_l in
    let se = js_of_expression ~mod_gen new_env e
    in ppf_let_in sd se
  | Texp_function (_, c :: [], Total) ->
    let rec explore pats e = match e.exp_desc with
      | Texp_function (_, c :: [], Total) ->
        let p, e = c.c_lhs, c.c_rhs
        in explore (p :: pats) e
      | _                                 ->
        String.concat ", " @@ List.map ident_of_pat @@ List.rev @@ pats, js_of_expression ~mod_gen new_env e in
    let args, body = explore [c.c_lhs] c.c_rhs
    in ppf_function args body
  | Texp_apply (f, exp_l)                 ->
     let sl' = exp_l
               |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope locn "optional apply arguments" | Some ei -> ei) in
     let sl = exp_l
              |> List.map (fun (_, eo, _) -> match eo with None -> out_of_scope locn "optional apply arguments" | Some ei -> js_of_expression ~mod_gen new_env ei) in
    let se = js_of_expression ~mod_gen new_env f in
    if is_infix f sl' && List.length exp_l = 2
    then ppf_apply_infix se (List.hd sl) (List.hd (List.tl sl))
    else ppf_apply se (String.concat ", " sl)

  | Texp_match (exp, l, [], Total) ->
     let se = js_of_expression ~mod_gen new_env exp in
     let sb = String.concat "@," (List.map (fun x -> js_of_branch ~mod_gen old_env x se) l) in
     ppf_match se sb

  | Texp_tuple (tl) -> ppf_tuple @@ show_list_f (fun exp -> js_of_expression ~mod_gen new_env exp) ", " tl

  | Texp_construct (loc, cd, el) ->
    let name = js_of_longident loc in
    if el = [] then (* Constructor has no parameters *)
      if is_sbool name then name (* Special case true/false to their JS natives *)
      else ppf_single_cstrs name
    else (* Constructor has parameters *)
      let fields = extract_attrs cd.cstr_attributes in
      let expr_strs = List.map (fun exp -> js_of_expression ~mod_gen new_env exp) el in
      let expand_constructor_list = List.map2 ppf_cstr in
      let expanded_constructors = expand_constructor_list fields expr_strs in
      ppf_multiple_cstrs name (show_list ", " expanded_constructors)

  | Texp_array      (exp_l)           -> ppf_array @@ show_list_f (fun exp -> js_of_expression ~mod_gen new_env exp) ", " exp_l
  | Texp_ifthenelse (e1, e2, None)    -> ppf_ifthen (js_of_expression ~mod_gen new_env e1) (js_of_expression ~mod_gen new_env e2)
  | Texp_ifthenelse (e1, e2, Some e3) -> ppf_ifthenelse (js_of_expression ~mod_gen new_env e1) (js_of_expression ~mod_gen new_env e2) (js_of_expression ~mod_gen new_env e3)
  | Texp_sequence   (e1, e2)          -> ppf_sequence (js_of_expression ~mod_gen new_env e1) (js_of_expression ~mod_gen new_env e2)
  | Texp_while      (cd, body)        -> ppf_while (js_of_expression ~mod_gen new_env cd) (js_of_expression ~mod_gen new_env body)
  | Texp_for        (id, _, st, ed, fl, body) -> ppf_for (Ident.name id) (js_of_expression ~mod_gen new_env st) (js_of_expression ~mod_gen new_env ed) fl (js_of_expression ~mod_gen new_env body)
  | Texp_record     (llde,_)          -> ppf_record (List.map (fun (_, lbl, exp) -> (lbl.lbl_name, js_of_expression ~mod_gen new_env exp)) llde)
  | Texp_match      (_,_,_, Partial)  -> out_of_scope locn "partial matching"
  | Texp_match      (_,_,_,_)         -> out_of_scope locn "matching with exception branches"
  | Texp_try        (_,_)             -> out_of_scope locn "exceptions"
  | Texp_function   (_,_,_)           -> out_of_scope locn "powered-up functions"
  | Texp_variant    (_,_)             -> out_of_scope locn "polymorphic variant"
  | Texp_field      (_,_,_)           -> out_of_scope locn "accessing field"
  | Texp_setfield   (_,_,_,_)         -> out_of_scope locn "setting field"
  | Texp_send       (_,_,_)           -> out_of_scope locn "objects"
  | Texp_new        (_,_,_)           -> out_of_scope locn "objects"
  | Texp_instvar    (_,_,_)           -> out_of_scope locn "objects"
  | Texp_setinstvar (_,_,_,_)         -> out_of_scope locn "objects"
  | Texp_override   (_,_)             -> out_of_scope locn "objects"
  | Texp_letmodule  (_,_,_,_)         -> out_of_scope locn "local modules"
  | Texp_assert      _                -> out_of_scope locn "assert"
  | Texp_lazy        _                -> out_of_scope locn "lazy expressions"
  | Texp_object     (_,_)             -> out_of_scope locn "objects"
  | Texp_pack        _                -> out_of_scope locn "packing"
    
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
  | _ -> error ~loc:pat.pat_loc "functions can't deconstruct values"
    
and js_of_let_pattern ?(mod_gen=[]) pat expr =
  let new_env = pat.pat_env in
  let sexpr = js_of_expression ~mod_gen new_env expr in
  match pat.pat_desc with
  | Tpat_var (id, _) -> ppf_decl ~mod_gen (Ident.name id) sexpr
  | Tpat_tuple (pat_l)
  | Tpat_array (pat_l) ->
     let l = List.map
               (function pat ->
                         match pat.pat_desc with
                         | Tpat_var (id, _) -> (Ident.name id, string_of_type_exp pat.pat_type)
                         | _ -> out_of_scope pat.pat_loc "pattern-matching in arrays"
               ) pat_l in
     ppf_pat_array l sexpr
  | _ -> error ~loc:pat.pat_loc "let can't deconstruct values"

and js_of_pattern ?(mod_gen=[]) pat obj =
  let locn = pat.pat_loc in
  match pat.pat_desc with
  | Tpat_any                     -> "default", ""
  | Tpat_constant   c            -> js_of_constant c, ""
  | Tpat_var       (id, _)       -> Ident.name id, ""
  | Tpat_construct (loc, cd, el) ->
     let c = js_of_longident loc in
     let spat = Printf.sprintf "%s" ("case \"" ^ c ^ "\"") in
     let params = extract_attrs cd.cstr_attributes in
     let binders =
       if List.length el = 0 then ""
       else Printf.sprintf "@[<v 0>%s@]"
          ("var " ^ show_list ", " (List.map2 (fun x y -> x ^ " = " ^ obj ^ "." ^ y) (List.map (fun x -> fst (js_of_pattern ~mod_gen x obj)) el) params) ^ ";") in
     spat, binders
  | Tpat_tuple el -> unsupported ~loc:locn "tuple matching"
  | Tpat_array el -> unsupported ~loc:locn "array-match"
  | Tpat_record (_,_) -> unsupported ~loc:locn "record"
  | Tpat_or (_,_,_) -> error ~loc:locn "not implemented yet"
  | Tpat_alias (_,_,_) -> out_of_scope locn "alias-pattern"
  | Tpat_variant (_,_,_) -> out_of_scope locn "polymorphic variants in pattern matching"
  | Tpat_lazy _ -> out_of_scope locn "lazy-pattern"

let to_javascript typedtree =
  let pre_res = js_of_structure Env.empty typedtree in
  let mod_code = String.concat "\n\n" (List.map L.strip_log_info !module_code) in
  let logged, unlogged, pre = L.logged_output (mod_code ^ "\n" ^ pre_res),
                              L.unlogged_output (mod_code ^ "\n" ^ pre_res),
                              (mod_code ^ "\n" ^ pre_res) in
  (logged, unlogged, pre)

