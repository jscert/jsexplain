(*
   Copyright 2017 Inria and Imperial College London

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Params
open Asttypes
open Attributes
open Log
open Misc
open Mytools
open Types
open Typedtree
open Monadic_binder_list

module L = Logged (Token_generator) (struct let size = 256 end)

let string_of_longident i =
  String.concat "." @@ Longident.flatten @@ i


(****************************************************************)
(* RENAMING OF CONSTRUCTOR NAMES *)

let rename_constructor s =
  if is_mode_not_pseudo() then s else begin
    let n = String.length s in
    if (n > 4 && s.[0] = 'C' && s.[1] = 'o' && s.[2] = 'q' && s.[3] = '_') then begin
      let r = String.sub s 4 (n-4) in
      r.[0] <- Char.uppercase_ascii r.[0];
      r
    end else s
  end


(****************************************************************)
(* SHADOWING CHECKER *)

module ShadowMapM = Map406.Make(String)
type shadow_map = int ShadowMapM.t
let increment_sm sm id =
  ShadowMapM.update id (option_app (Some 0) (fun i -> Some (i+1))) sm

(* Checks if ident is defined in env, and thus would shadow if redefined. *)
let ident_is_shadowing env str_ident =
  try ignore (Env.lookup_value (Longident.Lident str_ident) env);
    true
  with Not_found -> false

(* If the identifier is shadowing another, then update shadow map *)
let update_shadow_map sm env id =
  let str_id = Ident.name id in
  if ident_is_shadowing env str_id then increment_sm sm str_id else sm

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
     let open Lexing in
     if f.exp_loc.loc_ghost then false else
     if x.exp_loc.loc_ghost then false else
       x.exp_loc.loc_start.pos_lnum < f.exp_loc.loc_start.pos_lnum ||
       (x.exp_loc.loc_start.pos_lnum = f.exp_loc.loc_start.pos_lnum &&
        x.exp_loc.loc_start.pos_cnum < f.exp_loc.loc_start.pos_cnum)

let map_cstr_fields ?loc (sm : shadow_map) bind cstr elements =
  let fields = extract_cstr_attrs cstr in
  try map_opt_state2 bind sm fields elements
  with Invalid_argument _ ->
     error ?loc ("Insufficient fieldnames for arguments to " ^ cstr.cstr_name)

(** Decomposition of functions *)

let function_get_args_and_body e =
  let rec aux pats body = 
    match body.exp_desc with
    | Texp_function (_, c :: [], Total) ->
      let (p, body2) = (c.c_lhs, c.c_rhs) in 
      aux (p :: pats) body2
    | _ ->
       List.rev pats, body 
    in
  aux [] e


(****************************************************************)
exception Not_a_Tconstr

(* Extract type name from Tconstr type expressions *)
let get_type_name typ =
  match (Ctype.repr typ).desc with
  | Tconstr(path, _, _) -> Path.name path
  | _ -> raise Not_a_Tconstr

let test_type_name names typ =
  try List.mem (get_type_name typ) names
  with Not_a_Tconstr -> false

(* === comparison *)

let is_triple_equal_type = test_type_name ["int"; "bool"; "string"; "float"; "JsNumber.number"]

(****************************************************************)
(* PSEUDO-CODE mode *)

(* Auxiliary function *)

let is_ident e =
  match e.exp_desc with
  | Texp_ident (path, ident,  _) -> true
  | _ -> false

(* Hide all function arguments of type execution_ctx or state 
   (for function definitions) *)

let is_hidden_type = test_type_name ["JsSyntax.execution_ctx"; "JsSyntax.state"]

(* Hide all functions arguments of type execution_ctx or state 
   (for function applications) *)

let is_hidden_arg e =
  is_hidden_type e.exp_type && is_ident e

(* List of coercion functions *)

let coercion_functions = 
  [ 
    "JsSyntax.res_normal"; 
    "JsSyntax.res_ref"; 
    "JsSyntax.res_val"; 
    "JsInterpreterMonads.res_spec"; 
    "JsInterpreterMonads.res_out"; 
    "JsInterpreterMonads.res_ter"; 
    "JsInterpreterMonads.result_out";
    "Stdlib.number_of_int";
    (* "JsIntepreterMonads.res_void"; --no arg *)
  ]

(* Do not generate events for particular functions *)

let is_function_without_event f =
  match f.exp_desc with
  | Texp_ident (path, ident,  _) ->
      let x = Path.name path in 
      let m = Path.head path in
      let h = Ident.name m in
         List.mem h [ "JsSyntax"; "JsSyntaxAux" ]
      || List.mem x coercion_functions
  | _ -> false

(* Do not display particular functions *)

let is_coercion_function f =
  match f.exp_desc with
  | Texp_ident (path, ident,  _) -> 
      let x = Path.name path in
      List.mem x coercion_functions
  | _ -> false

let is_coercion_constructor lident =
    let x = string_of_longident lident in
    (*  Printf.printf "%s\n" x; *)
    let b = List.mem x [ (* todo: where is JsSyntax? *)
      "Coq_out_ter"; 
      "Coq_prim_bool"; 
      "Coq_prim_number"; 
      "Coq_prim_string"; 
      "Coq_value_prim"; 
      "Coq_object_loc_prealloc"; 
      "Coq_value_object"; 
      "Coq_attributes_data_of"; 
      "Coq_attributes_accessor_of"; 
      "Coq_full_descriptor_some"; 
      "Coq_env_record_decl"; 
      "Coq_resvalue_value"; 
      "Coq_resvalue_ref"; 
      "Coq_resvalue_ref"; 
      ] in 
    (* if (is_mode_pseudo()) then Printf.printf "%s %s\n" x (if b then " [yes]" else ""); *)
    b

(****************************************************************)
(* PPF HELPERS *)

(**
 * Before-hand definitions of Pretty-Printer-Format for converting ocaml
 * to ECMAScript, therefore all of them are in a single place.
 *)

let ppf_lambda_wrap s =
  Printf.sprintf "(function () {@;<1 2>@[<v 0>%s@]@,}())@," s

let ppf_function args body=
  (L.log_line (Printf.sprintf "function (%s) {" args) [L.Enter; (L.CreateCtx args)]) ^ (Printf.sprintf "@;<1 2>return@[<hov 2>@ (%s);@]@,}" body)

let ppf_apply f args =
  Printf.sprintf "@[<hov 2>%s(@,%s)@]"
                 f args

let ppf_apply_infix f arg1 arg2 =
  Printf.sprintf "@[<hov 0>(%s@ %s %s)@]"
                 arg1 f arg2
  (* todo: only put parentheses if needed *)

let ppf_match_case c =
  Printf.sprintf "case %s" c

(* FIXME: shadows now a sm, always should introduce a var *)
let ppf_match_binders binders =
  if binders = [] then "" else
    let binds = show_list "@," (List.map
      (fun (id,se) -> Printf.sprintf "var %s = %s;" id se) binders) in
  Printf.sprintf "@[<hov 2>%s@]" binds

let ppf_let_tuple ids sbody =
  assert (ids <> []);
  Printf.sprintf "@[<hov 2>var (%s) = %s;@]" (show_list ",@ " ids) sbody

let ppf_let_record ids sbody =
  Printf.sprintf "@[<hov 2>var {%s} = %s;@]" (show_list ",@ " ids) sbody

let ppf_array values =
  Printf.sprintf "[%s]"
                 values

let ppf_tuple = ppf_array

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

let ppf_cstrs styp cstr_name rest =
  let comma = if rest = "" then "" else "," in
  let styp_full =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_unlogged _ | Mode_pseudo _ -> ""
    | Mode_logged -> Printf.sprintf "type: \"%s\", " styp
    in
  Printf.sprintf "{@[<v 2>%stag: \"%s\"%s %s@]}" (* TODO: cleanup *)
    styp_full cstr_name comma rest

let ppf_cstrs_fct cstr_fullname args =
  if is_mode_pseudo() && args = [] 
    then cstr_fullname 
    else ppf_apply cstr_fullname (show_list ",@ " args)

let ppf_record llde =
  let rec aux acc = function
    | []               -> Printf.sprintf "{@;<1 2>@[<v 0>%s@]@,}" (*"@[<v 2>{@;<1 2>%s@]@,}"*) (* TODO: cleanup *) acc
    | (lbl, exp) :: [] -> aux (acc ^ Printf.sprintf "%s: %s" lbl exp) []
    | (lbl, exp) :: xs -> aux (acc ^ Printf.sprintf "%s: %s,@," lbl exp) xs
  in aux "" llde

let ppf_record_with seinit assign_exp =
  ppf_apply "Object.assign" (show_list ",@ " [ "{}"; seinit; assign_exp ])

let ppf_decl id expr = Printf.sprintf "@[<v 0>%s: %s,@,@]" id expr

let ppf_pat_array id_list array_expr =
  Printf.sprintf "var __%s = %s;@," "array" array_expr ^
    List.fold_left2 (fun acc (name, exp_type) y -> acc ^ Printf.sprintf "@[<v 0>var %s = __%s[%d];@,@]" name "array" y)
                    "" id_list @@ range 0 (List.length id_list - 1)

let ppf_field_access expr field =
  Printf.sprintf "%s.%s" expr field

(****************************************************)
(* Identifier Rewriting *)
(* List of JavaScript keywords that cannot be used as identifiers *)
let js_keywords =
  ["await"; "break"; "case"; "catch"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "else";
  "export"; "extends"; "finally"; "for"; "function"; "if"; "import"; "in"; "instanceof"; "new"; "return"; "super";
  "switch"; "this"; "throw"; "try"; "typeof"; "var"; "void"; "while"; "with"; "yield"; "enum"]

(** Conversion between integers and unicode \mathbb strings *)
(* 0-9 as unicode \mathbb{d} multibyte character strings *)
let ustr_bb_digits = Array.init 10 (fun i -> Printf.sprintf "\xf0\x9d\x9F%c" (char_of_int (0x98 + i)))

(** Converts an integer into an array of decimal digits *)
let int_to_array = function
| 0 -> [0]
| i -> let rec f i acc = if i = 0 then acc else f (i/10) (i mod 10 :: acc) in f i []

(** Converts an integer i into a unicode string representation of \mathbb{i} *)
let int_to_bb_ustr i = String.concat "" (List.map (fun d -> ustr_bb_digits.(d)) (int_to_array i))

(* On with the variable name mangling *)

let ppf_ident_name x sm =
  let x' =
    if List.mem x js_keywords then
      (* Variable name clashes with JS keyword: prefix with a \mathbb{V} character (\u1d54d) *)
      "\xf0\x9d\x95\x8d" ^ x
    else
      (* Variable name contains ' (not supported by JS): replace with unicode prime symbol (\u02b9) *)
      Str.global_replace (Str.regexp "'") "\xca\xb9" x
  in (* Append digits to handle non-shadowed ML variables that become shadowed in JS scopes *)
  option_app x' (fun i -> x' ^ (int_to_bb_ustr i)) (ShadowMapM.find_opt x sm)

(** Returns the JS version of the Ident name *)
 let ppf_ident id sm =
  ppf_ident_name (Ident.name id) sm

let ppf_path =
  Path.name

let ppf_module content =
  Printf.sprintf "{@,%s@,}" content

let ppf_module_wrap name content names_bound =
  let bindings = show_list ", " (List.map (fun id -> Printf.sprintf "@;<0 2>%s: %s" id id) names_bound) in
  Printf.sprintf "@[<v 0>var %s = (function() {@,%s@,@,return {%s};@,})();@,@]" name content bindings



(****************************************************************)
(* FRESH ID NAMES *)

let id_fresh =
  let r = ref 0 in
  fun prefix -> (incr r; prefix ^ string_of_int !r)


(****************************************************************)
(* TOKEN TO LOC BINDINGS FOR THE ML SOURCE FILES *)

(* Keeps track of the location associated with each token,
   maps int to (pos*pos).  *)

type pos = { pos_line: int; pos_col: int }
let token_locs = Hashtbl.create 50 

let pos_of_lexing_pos lexing_pos =
  let (file, line, char) = Location.get_pos_info lexing_pos in
  { pos_line = line; pos_col = char } 

let pos_pair_of_loc loc =
  (pos_of_lexing_pos loc.Location.loc_start,
   pos_of_lexing_pos loc.Location.loc_end)


(****************************************************************)
(* FRESH TOKEN NAMES *)

let token_basename_ref = ref "no_token_basename_registered"

let token_register_basename basename =
  token_basename_ref := basename

(* returns a string of the form: ["filename.js", 3425],
   where 3425 describes the token. *)

let token_fresh =
  let r = ref 0 in
  fun mode loc -> (
    incr r; 
    Hashtbl.add token_locs (!r) (pos_pair_of_loc loc);
    (* if mode = Mode_unlogged TokenFalse then ("", "", "") else begin end*)
    let token_start = Printf.sprintf "@{<%d>" !r in
    let token_stop = "@}" in
    let token_loc = Printf.sprintf "\"%s.js\", %d" !token_basename_ref !r in 
    (token_start, token_stop, token_loc)
    )



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




(*--------- if ---------*)

let ppf_ifthenelse arg iftrue iffalse =
  Printf.sprintf "@[<v 0>if (%s) {@;<1 2>@[<v 0>%s@]@,} else {@;<1 2>@[<hv 0>%s@]@,}@]"
                 arg iftrue iffalse

let generate_logged_if loc ctx sintro sarg siftrue siffalse =
  (* sintro is not empty only in the logged case,
     it describes the binding of the value describing the argument of the if *)
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     let sarg_with_token = Printf.sprintf "%s%s%s" token_start sarg token_stop in
     ppf_ifthenelse sarg_with_token siftrue siffalse
  | Mode_logged ->
     let sevent = Printf.sprintf "%slog_event(%s, %s, \"if\");@,"
        sintro token_loc ctx in
     let sbody = ppf_ifthenelse sarg siftrue siffalse in
     sevent ^ sbody


  (* TODO: extend the ctx with if_arg *)

(*--------- match ---------*)

let generate_logged_case loc spat binders ctx newctx sbody need_break =
  (* Note: if binders = [], then newctx = ctx *)
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  let sbinders_common () = 
    Printf.sprintf "%s%s" (if binders = [] then "" else "@;<1 2>") (ppf_match_binders binders) in
  let (shead, spat, sbinders, sintro) =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_pseudo _ ->
        let args = List.map fst binders in
        let spat = (* LATER: use a cleaner separation with Case of (cstr,args) | Default *)
          if spat = "case ::" then begin
            let (x,y) = match args with [x;y] -> (x,y) | _ -> assert false in
            Printf.sprintf "case (%s::%s)" x y
          end else if args = [] then begin
            spat 
          end else begin
            ppf_apply spat (show_list ",@ " args)
          end in
        (token_start, spat, "", token_stop)
    | Mode_unlogged _ ->
        (token_start, spat, sbinders_common(), token_stop)
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
        spreintro token_loc newctx in
      ("", spat, sbinders_common(), sintro)
    in
  (Printf.sprintf "@[<v 0>%s%s:%s%s@;<1 2>@[<v 0>%s%s@]@]"
     shead spat sbinders sintro sbody
     (if need_break then "@,break;" else ""))

let ppf_match sintro sarg sbranches =
  let sswitch, sbranches = 
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_pseudo _ -> (*"match"*) "switch", sbranches
    | Mode_unlogged _ -> "switch", sbranches
    | Mode_logged -> "switch", sbranches 
      (* TODO: put back if there is not already a default case:
          ^ "@,default: throw \"No matching case for switch\";" *)
    in
  Printf.sprintf "%s%s (%s) {@;<1 2>@[<v 0>%s@]@,}@,"
    sintro sswitch sarg sbranches

let generate_logged_match loc ctx sintro sarg sbranches arg_is_constant =
  (* sintro is useful not just in the logged case, but also in unlogged;
     this is needed for the semantics *)
  (* arg_is_constant describes whether the argument of switch is a basic JS value,
     or whether it is an encoded object from which we need to read the tag field *)
  let sarg = if arg_is_constant || is_mode_pseudo() then sarg else sarg ^ ".tag" in
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     let sarg_with_token = Printf.sprintf "%s%s%s" token_start sarg token_stop in
     ppf_match sintro sarg_with_token sbranches 
  | Mode_logged ->
     let sbody = ppf_match "" sarg sbranches in
     Printf.sprintf "%slog_event(%s, %s, \"switch\");@,%s"
        sintro token_loc ctx sbody

  (* TODO: extend the ctx with switch_arg *)

(*--------- let ---------*)

let generate_logged_let loc ids ctx newctx sdecl sbody =
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ -> 
     Printf.sprintf "%s%s%s@,%s" token_start sdecl token_stop sbody  
  | Mode_logged ->
    let mk_binding x =
      Printf.sprintf "{key: \"%s\", val: %s}" x x in
    let bindings =
      Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding ids)) in 
    Printf.sprintf "%s@,var %s = ctx_push(%s, %s);@,log_event(%s, %s, \"let\");@,%s@,"
      sdecl newctx ctx bindings token_loc newctx sbody


(*--------- function call ---------*)

let generate_logged_apply loc ctx sbody =
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     Printf.sprintf "%s%s%s" token_start sbody token_stop
  | Mode_logged ->
     Printf.sprintf "log_event(%s, %s, \"call\");@,%s" token_loc ctx sbody


(*--------- enter function body ---------*)

let generate_logged_enter loc arg_ids ctx newctx sbody = 
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  let (shead1, shead2, sintro) =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_unlogged _ | Mode_pseudo _ -> (token_start, token_stop, "")
    | Mode_logged ->
      let mk_binding x =
        Printf.sprintf "{key: \"%s\", val: %s}" x x
      in
      let bindings =
        Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding arg_ids))
      in 
      let sintro = Printf.sprintf "var %s = ctx_push(%s, %s);@,log_event(%s, %s, \"enter\");@,"
        newctx ctx bindings token_loc newctx in
      ("", "", sintro)
  in
  let args = String.concat ", " arg_ids in
  Printf.sprintf "%sfunction (%s)%s {@;<1 2>@[<v 0>%s%s@]@,}" shead1 args shead2 sintro sbody


(*--------- return ---------*)

(* possibly: optimize return when it's a value *)

let generate_logged_return loc ctx sbody = 
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     Printf.sprintf "@[<hv 2>%sreturn (@,%s);%s@]" token_start sbody token_stop
  | Mode_logged ->
    let id = id_fresh "_return_" in
    Printf.sprintf "var %s = %s;@,log_event(%s, ctx_push(%s, [{key: \"#RETURN_VALUE#\", val: %s}]), \"return\");@,return (%s); "
      id sbody token_loc ctx id id



(****************************************************************)
(* DESTINATIONS *)

(** Destination-style translation of expressions *)

type dest =
  | Dest_ignore
  | Dest_return
  | Dest_assign of string * bool (* bool indicates shadowing *)
  | Dest_inline

let apply_dest loc ctx dest sbody =
  match dest with
  | Dest_ignore -> sbody
  | Dest_return -> generate_logged_return loc ctx sbody
  | Dest_assign (id,s) -> Printf.sprintf "%s%s = %s;" (if s then "" else "var ") id sbody
  | Dest_inline -> sbody

(* LATER: pull out the "var" out of switch *)

exception Not_good_for_dest_inline

let reject_inline dest =
  if dest = Dest_inline
    then raise Not_good_for_dest_inline


(****************************************************************)
(* HELPER FUNCTIONS *)

and js_of_constant = function
  | Const_int       n     -> string_of_int n
  | Const_char      c     -> String.make 1 c
  | Const_string   (s, _) -> "\"" ^ (String.escaped (String.escaped s)) ^ "\"" (* Warning: 2 levels of printf *)
  | Const_float     f     -> f
  | Const_int32     n     -> Int32.to_string n
  | Const_int64     n     -> Int64.to_string n
  | Const_nativeint n     -> Nativeint.to_string n

let js_of_path_longident sm path ident =
  match String.concat "." @@ Longident.flatten ident.txt with
  (* for unit: *)
  | "()"  -> unit_repr
  (* for bool: *)
  | "&&"  -> "&&"
  | "||"  -> "||"
  (* for float: *)
  | "="  -> "=="
  | "+."  -> "+"
  | "*."  -> "*"
  | "-."  -> "-"
  | "~-." -> "-"
  | "/."  -> "/"
  | "<"   -> "<"
  | ">"   -> ">"
  | "<="   -> "<="
  | ">="   -> ">="
  (* for int: *)
  | "+"  -> "+"
  | "*"  -> "*"
  | "-"  -> "-"
  | "/"  -> "/"
  (* for string *)
  | "^"   -> "+" (* !!TODO: we want to claim ability to type our sublanguage, so we should not use this *)
  | res   ->
      let res = if !generate_qualified_names && (Path.head path).Ident.name <> "Stdlib"
                   then ppf_path path else res in
      ppf_ident_name res sm

let is_triple_equal_comparison e sm =
   match e.exp_desc with
   | Texp_ident (path, ident,  _) ->
      let sexp = js_of_path_longident sm path ident in
      sexp = "==="
      (* TODO: this text could be optimized *)
   | _ -> false

let ppf_ident_of_pat sm pat = match pat.pat_desc with
  | Tpat_var (id, _) -> ppf_ident id sm
  | Tpat_any         -> id_fresh "_pat_any_"
  | _ -> error ~loc:pat.pat_loc "functions can't deconstruct values"


(* takes a list of pairs made of: list of strings, and list of strings,
   and return a pair of a string (the string concat with newlines of the fst strings),
   and a list of strings (the list flatten of the snd strings) *)

let combine_list_output args =
   let (strs,bss) = List.split args in
   (show_list "@,@," strs), (List.flatten bss)

(* returns a pair (x,e), where [x] in the name in [pat]
   and where [e] is the access to "stupleobj[index]" *)
let tuple_component_bind stupleobj pat (result, index, sm) =
   let loc = pat.pat_loc in
   match pat.pat_desc with
   | Tpat_var (id, _) ->
       let sm = update_shadow_map sm pat.pat_env id in
       let sid = ppf_ident id sm in
       ((sid, Printf.sprintf "%s[%d]" stupleobj index)::result, index-1, sm)
   | Tpat_any -> (result, index-1, sm)
   | _ -> out_of_scope loc "Nested pattern matching"

(* returns a list of pairs of the form (x,e), corresponding
   to the bindings to be performed for decomposing [stupleobj]
    as the tuple of patterns [pl]. *)
let tuple_binders stupleobj sm pl =
  let nb_args = List.length pl in
  let (result, _, sm) = List.fold_right (tuple_component_bind stupleobj) pl ([], nb_args - 1, sm) in
  (result, sm)

(****************************************************************)
(* TRANSLATION *)

let rec js_of_structure s =
   let rec extract_opens acc items =
      match items with
      | { str_desc = Tstr_open od }::items2 ->
         extract_opens (od.open_path::acc) items2
      | _ -> (List.rev acc, items)
      in
   let open_paths, items = extract_opens [] s.str_items in
   let contents, namesbound = combine_list_output (List.map (fun strct -> js_of_structure_item strct) items) in
   let prefix = List.fold_left (fun str path -> str ^ "with (" ^ ppf_path path ^ ") {@,") "" open_paths in
   let postfix = List.fold_left (fun str path -> str ^ "@,}// end of with " ^ ppf_path path) "" open_paths in
   (prefix ^ "@," ^ contents ^ postfix, namesbound)

and js_of_structure_item s =
  let loc = s.str_loc in
  match s.str_desc with
  | Tstr_eval (e, _)     -> 
     let str = Printf.sprintf "%s" @@ js_of_expression ShadowMapM.empty ctx_initial Dest_ignore e in
     (str, [])
  | Tstr_value (_, vb_l) ->
     combine_list_output (~~ List.map vb_l (fun vb ->
        let id = ppf_ident_of_pat ShadowMapM.empty vb.vb_pat in
        if ident_is_shadowing s.str_env id then error ~loc "Variable shadowing not permitted at toplevel"
        else
        let sbody = js_of_expression_inline_or_wrap ShadowMapM.empty ctx_initial vb.vb_expr in
        let s = Printf.sprintf "@[<v 0>var %s = %s;@]" id sbody in
        (s, [id])))
  | Tstr_type (rec_flag, decls) ->
     combine_list_output (~~ List.map decls (fun decl -> 
        match decl.typ_type.type_kind with
        | Type_variant cstr_decls ->
           let styp = decl.typ_name.txt in
           combine_list_output (~~ List.map cstr_decls (fun (cd:Types.constructor_declaration) -> 
              let cstr_name = cd.Types.cd_id.Ident.name in
              let fields = extract_cstr_attrs_basic cstr_name cd.cd_attributes in
              let sargs = show_list ", " fields in
              let sbindings = map_opt2 (fun x y -> ppf_cstr x y) fields fields in (* FIXME: twice fields, really?! *)
              let rest = show_list ", " sbindings in
              let cstr_name = rename_constructor cstr_name in
              let sobj = ppf_cstrs styp cstr_name rest in 
              let sbody = Printf.sprintf "function %s(%s) { return %s; }" cstr_name sargs sobj in
              (sbody, [cstr_name])
              ))
        | _ -> ("", [])
        ))
  | Tstr_open       _  -> ("",[]) (* Handle modules by use of multiple compilation/linking *)
  | Tstr_modtype    _  -> ("",[])
  | Tstr_module     b  -> out_of_scope loc "modules" (* Partial implementation present in commit e1e6e4b *)
  | Tstr_primitive  _  -> out_of_scope loc "primitive functions"
  | Tstr_typext     _  -> out_of_scope loc "type extensions"
  | Tstr_exception  _  -> out_of_scope loc "exceptions"
  | Tstr_recmodule  _  -> out_of_scope loc "recursive modules"
  | Tstr_class      _  -> out_of_scope loc "objects"
  | Tstr_class_type _  -> out_of_scope loc "class types"
  | Tstr_include    _  -> out_of_scope loc "includes"
  | Tstr_attribute (l, _) ->
      if l.txt = "ocaml.doc" || l.txt = "ocaml.text" then ("",[])
      else out_of_scope loc "attributes"

(* Translates each pattern/subexpression pair branch of a match expression *)
and js_of_branch sm ctx dest b eobj =
  let spat, binders, sm = js_of_pattern sm b.c_lhs eobj in
  let newctx = if binders = [] then ctx else ctx_fresh() in
  let sbody = js_of_expression sm newctx dest b.c_rhs in
  let need_break = (dest <> Dest_return) in
  generate_logged_case b.c_lhs.pat_loc spat binders ctx newctx sbody need_break
  (* there is no need to propagate the updated [sm] back up the tree, as pattern bound only in [sbody] *)

and js_of_expression_inline_or_wrap sm ctx e =
  try 
    js_of_expression sm ctx Dest_inline e
  with Not_good_for_dest_inline ->
    js_of_expression_wrapped sm ctx e

and js_of_expression_wrapped sm ctx e = (* dest = Dest_return *)
  ppf_lambda_wrap (js_of_expression sm ctx Dest_return e)

and js_of_expression_naming_argument_if_non_variable sm ctx obj name_prefix =
  if is_mode_pseudo() then begin
    "", js_of_expression sm ctx Dest_ignore obj
  end else begin
    match obj.exp_desc with
    | Texp_ident (path, ident,  _) -> 
        "", (js_of_path_longident sm path ident)
    | _ ->  (* generate  var id = sexp;  *)
        let id = id_fresh name_prefix in
        let sintro = js_of_expression sm ctx (Dest_assign (id, false)) obj in
        (sintro ^ "@,"), id
  end

and js_of_expression (sm : shadow_map) ctx dest e =
  let inline_of_wrap = js_of_expression_inline_or_wrap sm ctx in (* shorthand *)
  let loc = e.exp_loc in
  let apply_dest' = apply_dest loc in
  match e.exp_desc with

  | Texp_ident (path, ident,  _) -> 
      let sexp = js_of_path_longident sm path ident in
      let sexp = if sexp = "not" then "!" else sexp in (* hack for renaming "not" on the fly *)
      apply_dest' ctx dest sexp

  | Texp_constant c -> 
      let sexp = js_of_constant c in
      apply_dest' ctx dest sexp

  | Texp_let (recur, vb_l, e) ->
    (* [vb_l] is a list of value bindings, corresponding to each term of a [let vb_0 and vb_1 and vb_2] *)
    (* TODO: Handle mixed tuple/record/standard vbs let expressions *)
    reject_inline dest;
    let (ids, sdecl, sm') = begin match vb_l with
      | [ { vb_pat = { pat_desc = Tpat_tuple pl }; vb_expr = obj } ] -> (* binding tuples *)
          let (sintro, stupleobj) = js_of_expression_naming_argument_if_non_variable sm ctx obj "_tuple_arg_" in
          let (binders, sm') = tuple_binders stupleobj sm pl in
          let ids = List.map fst binders in
          let sdecl =
            if is_mode_pseudo() then begin
              ppf_let_tuple ids stupleobj
            end else begin
              ppf_match_binders binders
            end in
          (ids, sintro ^ sdecl, sm')
      | [ { vb_pat = { pat_desc = Tpat_record (args, closed_flag) }; vb_expr = obj } ] ->
          (* binding records -- used in JsCommon.ml *)
          (* args : (Longident.t loc * label_description * pattern) list *)
         let (sintro, seobj) = js_of_expression_naming_argument_if_non_variable sm ctx obj "_record_arg_" in
         let bind sm' (arg_loc,label_descr,pat) =
            let name = label_descr.lbl_name in
            match pat.pat_desc with
            | Tpat_var (id, _) -> 
                let sm' = update_shadow_map sm' pat.pat_env id in
                let sid = ppf_ident id sm' in
                (sm', (sid, Printf.sprintf "%s.%s" seobj name))
            | Tpat_any -> out_of_scope e.exp_loc "Underscore pattern in let-record"
            | _ -> out_of_scope e.exp_loc "Nested pattern matching"
            in
          let sm', binders = map_state bind sm args in
          let ids = List.map fst binders in
          let sdecl =
            if is_mode_pseudo() then begin
              ppf_let_record ids seobj 
            end else begin
              ppf_match_binders binders
            end in
          (ids, sintro ^ sdecl, sm')
      | _ -> (* other cases *)
        (* vb subexpressions are in the context of overall expression: use constant sm for this,
           but fold over a changing new_sm for the created bindings *)
        let folder vb (sids, jsexprs, new_sm) =
          let (sid, jsexpr, new_sm) = js_of_let_pattern sm new_sm ctx vb recur in
          (sid::sids, jsexpr::jsexprs, new_sm)
        in
        let (ids, sdecls, new_sm) = List.fold_right folder vb_l ([], [], sm) in
        let sdecl = String.concat lin1 sdecls in
        (ids, sdecl, new_sm)
      end in
    let sbody = js_of_expression sm' ctx dest e in
    let newctx = ctx_fresh() in
    let sexp = generate_logged_let loc ids ctx newctx sdecl sbody in
    sexp

  | Texp_function (_, c :: [], Total) ->
    let pats, body = function_get_args_and_body e in
    let pats_clean = List.filter (fun pat -> is_mode_not_pseudo() || not (is_hidden_type pat.pat_type)) pats in
    let arg_ids = List.map (ppf_ident_of_pat sm) pats_clean in   (******* HERE *******)
       (* FUTURE USE: (for function taking tuples as args)
       let arg_idss, tuplebindingss = List.split (List.map (fun pat ->
         match pat.pat_desc with
         | Tpat_var (id, _) -> let x = ppf_ident id in [x], []
         | Tpat_any         -> let x = id_fresh "_pat_any_" in [x], [] 
         | Tpat_tuple pl -> 
            let a = id_fresh "_tuple_arg_" in
            let binders = tuple_binders a pl in
            if is_mode_pseudo() then begin
               (* the name [a] is ignored in this case *)
               let xs = List.map fst binders in
               let x = Printf.sprintf "(%s)" (show_list ",@ " xs) in
               [x], []
            end else begin 
               [a], binders
            end
         | _ -> error ~loc:pat.pat_loc "functions can't deconstruct values unless tuple"
         ) pats_clean) in
       let arg_ids = List.concat arg_idss in
       let tuple_bindings = List.concat tuplebindingss in
       (* - In normal mode, [arg_ids] contains the list of all identifiers
            bound by the arguments (including those in tuples), and
            [tuplebindings] is a list of pairs of the form (xi,ei), where
            each [xi] is a variable bound by a tuple pattern, and each
            [ei] is an expression of the form "tuple_arg_[3]" giving the
            expression to which [xi] should be bound. 
         - In pseudo-code mode, [arg_ids] contains the names of the arguments,
           possibly directly in the tupled form, e.g. "(x,y)", and [tuplebindings]
           is empty. *)
       let stuplebindings = ppf_match_binders tuple_bindings in
       *)
    let stuplebindings = "" in
    let newctx = ctx_fresh() in
    let sbody = js_of_expression sm newctx Dest_return body in
    let sexp = generate_logged_enter body.exp_loc arg_ids ctx newctx sbody in
    apply_dest' ctx dest (stuplebindings ^ sexp)

  | Texp_apply (f, exp_l) when is_monadic_texpr e ->
      let sl_clean = exp_l
              |> List.map (fun (_, eo) -> match eo with 
                                             | None -> out_of_scope loc "optional apply arguments" 
                                             | Some ei -> ei) in
      let (e1,e2) =
        match sl_clean with 
        | [e1;e2] -> (e1,e2) 
        | _ -> out_of_scope loc  "not exactly two arguments provided to monad"
        in
      let fname =
        match f.exp_desc with
        | Texp_ident (path, ident,  _) -> Path.last path
        | _ -> assert false
        in
      let monad_name = 
        let n = String.length fname in
        if n <= 3 then out_of_scope loc "monad name does not start with 'if_'";
        String.sub fname 3 (n-3)
        in
      let sexp1 = inline_of_wrap e1 in
      let pats,body = function_get_args_and_body e2 in
      let pats_clean = List.filter (fun pat -> is_mode_not_pseudo() || not (is_hidden_type pat.pat_type)) pats in
      let sm, bindings = map_state (fun sm pat ->
        match pat.pat_desc with
        | Tpat_var (id, _) -> let x = ppf_ident id sm in sm, ([x], [x], [])
        | Tpat_any         -> let x = id_fresh "_pat_any_" in sm, ([x], [], [])
        | Tpat_tuple pl ->
           let a = id_fresh "_tuple_arg_" in
           let binders, sm = tuple_binders a sm pl in
           let xs = List.map fst binders in
           if is_mode_pseudo() then
             (* the name [a] is ignored in this case *)
             let arg = Printf.sprintf "(%s)" (show_list ",@ " xs) in
             sm, ([arg], xs, [])
           else
             sm, ([a], xs, binders)
        | _ -> error ~loc:pat.pat_loc "functions can't deconstruct values unless tuple"
        ) sm pats_clean in
      let arg_idss, bound_idss, tuplebindingss = list_split3 bindings in
      let arg_ids = List.concat arg_idss in
      let bound_ids = List.concat bound_idss in
      let tuple_bindings = List.concat tuplebindingss in
       (* - In normal mode, [arg_ids] contains the name of the arguments
            as identifiers, [bound_ids] contains the list of all identifiers
            bound by the arguments (including those in tuples), and
            [tuplebindings] is a list of pairs of the form (xi,ei), where
            each [xi] is a variable bound by a tuple pattern, and each
            [ei] is an expression of the form "tuple_arg_[3]" giving the
            expression to which [xi] should be bound. 
         - In pseudo-code mode, [arg_ids] contains the names of the arguments,
           possibly directly in the tupled form, e.g. "(x,y)", [bound_ids] is
           as before, and [tuplebindings] are empty. *)
      let stuplebindings = ppf_match_binders tuple_bindings in

      let newctx = ctx_fresh() in
      let sbody = js_of_expression sm newctx Dest_return body in

      let (token_start1, token_stop1, _token_loc) = token_fresh !current_mode loc in 
      let (token_start2, token_stop2, _token_loc) = token_fresh !current_mode loc in 
       (* token1 placed on sexp1
          token2 placed on ids *)
      if is_mode_pseudo() then begin
        let sargs =
           match arg_ids with
           | [] -> "_"
              (*deprecated: Printf.sprintf "@[<hov 2>%s%s;%s@]@,%s" token_start sexp1 token_stop sbody*)
           | [sarg] -> sarg
           | _ -> out_of_scope loc "two argument bound by monad in pseudo-code mode"
           in
        (* e.g.:  var%spec x = expr in cont *)
        let (token_start3, token_stop3, _token_loc) = token_fresh !current_mode loc in (* for logged_let *)
        let sexp = Printf.sprintf "@[<hov 2>%svar%s%s %s%s%s%s = %s%s%s;@]@,%s" token_start3 "%%" monad_name token_start2 sargs token_stop2 token_stop3 token_start1 sexp1 token_stop1 sbody in
        begin match dest with
        | Dest_assign _ ->
          apply_dest' ctx dest (ppf_lambda_wrap sexp)
        | Dest_ignore -> sexp
        | Dest_return ->   (* do not display redundand return, but count it *)
            let (token_start, token_stop, _token_loc) = token_fresh !current_mode loc in 
            Printf.sprintf "%s%s%s" token_start sexp token_stop
        | Dest_inline -> sexp (* TODO: check if ok *)
        end

      end else begin
        (* e.g.:  if_spec(expr, (function(s, x) -> cont)) *)
        let sexp1_token = Printf.sprintf "%s%s%s" token_start1 sexp1 token_stop1 in
        let sbody_logged = generate_logged_let loc bound_ids ctx newctx "" sbody in
        let cont_token = Printf.sprintf "function(%s%s%s) {@;<1 2>@[<v 0>%s%s%s@]@,}" token_start2 (String.concat ",@ " arg_ids) token_stop2 stuplebindings (if stuplebindings <> "" then "@," else "") sbody_logged in
        let sexp = ppf_apply fname (String.concat ",@ " [sexp1_token; cont_token]) in
        apply_dest' ctx dest sexp

      end 

  | Texp_apply (f, exp_l) ->

     (* first check not partial application *)
     let is_result_arrow = 
        let ty = e.exp_type in
        let ty = Ctype.repr ty in
        match ty.desc with
        | Tarrow(l, ty1, ty2, _) -> true
        | _ -> false
        in
     if is_result_arrow then out_of_scope loc "partial application";
     
     let sl_clean = exp_l
              |> List.map (fun (_, eo) -> match eo with 
                                             | None -> out_of_scope loc "optional apply arguments" 
                                             | Some ei -> ei) in

     (* TODO: reimplement using list.mapfilter *)
     let sl_and_translated = List.map (fun ei -> ei, inline_of_wrap ei) sl_clean in
     let sl_and_translated = List.filter (fun (ei,sei) -> 
        is_mode_not_pseudo() || not (is_hidden_arg ei)) sl_and_translated in
     let sl = List.map snd sl_and_translated in

     let se = inline_of_wrap f in
     let sexp = 
        if is_triple_equal_comparison f sm then begin
          if (List.length exp_l <> 2) 
            then out_of_scope loc "=== should be applied to 2 arguments";
          let typ = (List.hd sl_clean).exp_type in
          let stype = get_type_name typ in
          if is_triple_equal_type typ then begin
            let (x,y) = match sl with [x;y] -> (x,y) | _ -> assert false in
            ppf_apply_infix "===" x y
          end else begin
            let stype = Str.global_replace (Str.regexp "\\.") "_" stype in
            ppf_apply ("_compare_" ^ stype) (String.concat ",@ " sl)
          end
        end else if is_infix f sl_clean && List.length exp_l = 2 then begin
           ppf_apply_infix se (List.hd sl) (List.hd (List.tl sl))
        end else begin
           ppf_apply se (String.concat ",@ " sl)
        end in
  
     let sexp_instrumented =
       if is_function_without_event f then begin
           if is_mode_pseudo() && is_coercion_function f then begin
              if (List.length sl) <> 1
                then out_of_scope loc "coercion is not applied to a single element";
              (String.concat ",@ " sl)
           end else begin
              sexp
           end
       end else if !current_mode = Mode_logged then begin
          (* use this to prevent logging of the result
             let return_exp = Printf.sprintf "return %s;" sexp in *)
          let return_exp = apply_dest' ctx Dest_return sexp in
          let logged_sexp = generate_logged_apply loc ctx return_exp in
          let wrapped_exp = ppf_lambda_wrap logged_sexp in
          wrapped_exp
       end else begin
          (* we need a token to match the Dest_return above *)
          let (token_start, token_stop, _token_loc) = token_fresh !current_mode loc in 
          let sexp2 = generate_logged_apply loc ctx sexp in
          let sexp3 = Printf.sprintf "%s%s%s" token_start sexp2 token_stop in
          sexp3
       end 
       in
     apply_dest' ctx dest sexp_instrumented

  | Texp_match (obj, l, [], Total) ->
     reject_inline dest;
     let (sintro, sarg) = js_of_expression_naming_argument_if_non_variable sm ctx obj "_switch_arg_" in
     let sbranches = String.concat "@," (List.map (fun b -> js_of_branch sm ctx dest b sarg) l) in
     let arg_is_constant = exp_type_is_constant obj in
     generate_logged_match loc ctx sintro sarg sbranches arg_is_constant

  | Texp_tuple (tl) -> 
     let sexp = ppf_tuple @@ show_list_f (fun exp -> inline_of_wrap exp) ", " tl in
     apply_dest' ctx dest sexp

  | Texp_construct (p, cd, el) ->
    let cstr_fullname = string_of_longident p.txt in
    let cstr_name = cd.cstr_name in
    let cstr_fullname = 
      if cstr_fullname = "[]" then "mk_nil" 
      else if cstr_fullname = "::" then "mk_cons" 
      else begin (* rename the constructor to remove "Coq_" prefix *)
        let id2 = 
          match p.txt with
          | Longident.Lident s -> Longident.Lident (rename_constructor s)
          | Longident.Ldot(l, s) -> Longident.Ldot(l, rename_constructor s)  
          | Longident.Lapply(_, _) -> unsupported "Longident.Lapply"
          in
        string_of_longident id2 
      end in  
    (*let styp = string_of_type_exp e.exp_type in*)

     (* TODO: factorize the pattern below with function applications *)
     let sl_and_translated = List.map (fun ei -> ei, inline_of_wrap ei) el in
     let sl_and_translated = List.filter (fun (ei,sei) -> 
        is_mode_not_pseudo() || not (is_hidden_arg ei)) sl_and_translated in
     let sl = List.map snd sl_and_translated in

    let sexp =
      if is_sbool cstr_name then 
        cstr_name 
      else if is_unit cstr_name then 
        unit_repr 
      else if is_mode_pseudo() && is_coercion_constructor p.txt then begin
        if (List.length sl) <> 1
          then out_of_scope loc "coercion is not applied to a single element";
        (String.concat ",@ " sl)
      end else begin
        ppf_cstrs_fct cstr_fullname sl
      end in
    apply_dest' ctx dest sexp

  | Texp_array      (exp_l)           -> ppf_array @@ show_list_f (fun exp -> inline_of_wrap exp) ", " exp_l
  | Texp_ifthenelse (e1, e2, None)    -> out_of_scope loc "if without else"
    (* ppf_ifthen (js_of_expression e1) (js_of_expression e2) *)
  | Texp_ifthenelse (e1, e2, Some e3) ->
     reject_inline dest;
     let (sintro, se1) = 
       match !current_mode with
       | Mode_logged -> 
           let (sintro, sobj) = js_of_expression_naming_argument_if_non_variable sm ctx e1 "_if_arg_" in
           (sintro, sobj)
       | _ ->  ("", inline_of_wrap e1)
       in
     generate_logged_if loc ctx sintro se1 (js_of_expression sm ctx dest e2) (js_of_expression sm ctx dest e3)
  | Texp_sequence (e1, e2) -> 
     ppf_sequence (inline_of_wrap e1) (js_of_expression sm ctx dest e2)
  | Texp_while      (cd, body)        -> out_of_scope loc "while"
    (* ppf_while (js_of_expression cd) (js_of_expression body) *)
  | Texp_for        (id, _, st, ed, fl, body) -> out_of_scope loc "for"
    (* ppf_for (ppf_ident id) (js_of_expression st) (js_of_expression ed) fl (js_of_expression body) *)

  | Texp_record     {fields=llde; extended_expression=None} ->
      let sexp = ppf_record (Array.to_list (Array.map
          (fun (lbl, def) -> match def with
          | Overridden (_, exp) -> (lbl.lbl_name, inline_of_wrap exp)
          | _ -> assert false)
        llde)) in
      apply_dest' ctx dest sexp

  | Texp_record  {fields=af; extended_expression=Some einit} ->
      let assigns = Array.fold_right
        (fun (lbl, def) accu -> match def with
          | Overridden (_, exp) -> (lbl.lbl_name, inline_of_wrap exp) :: accu
          | _ -> accu)
        af [] in
      let sexp = ppf_record_with (inline_of_wrap einit) (ppf_record assigns) in
      apply_dest' ctx dest sexp

  | Texp_field      (exp, _, lbl)     ->
      let sexp = ppf_field_access (inline_of_wrap exp) lbl.lbl_name in
      apply_dest' ctx dest sexp

  | Texp_function (Nolabel, cases, Total) ->
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
      let exp = mk_exp (Texp_function (Nolabel, [thecase], Total)) in
      js_of_expression sm ctx dest exp

  | Texp_assert      _                -> out_of_scope loc "assert (please use assert ppx syntax)"
  | Texp_match      (_,_,_, Partial)  -> out_of_scope loc "partial matching"
  | Texp_match      (_,_,_,_)         -> out_of_scope loc "matching with exception branches"
  | Texp_try        (_,_)             -> out_of_scope loc "exceptions"
  | Texp_function (_, _, _)           -> out_of_scope loc "use of labels"
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
  | _                                 -> out_of_scope loc "Unknown js_of_expression Texp value"

(* returns the name bound and the code that assigns a value to this name *)
and js_of_let_pattern sm new_sm ctx vb recur =
  let { vb_pat = pat; vb_expr = expr } = vb in
  let id =
    match pat.pat_desc with
    | Tpat_var (id, _) -> id
    | Tpat_any -> out_of_scope pat.pat_loc "_ in let"
    | Tpat_alias _ -> out_of_scope pat.pat_loc "alias in let"
    | Tpat_constant _ -> out_of_scope pat.pat_loc "constant in let"
    | Tpat_tuple _ -> out_of_scope pat.pat_loc "tuple in let"
    | Tpat_construct _ -> out_of_scope pat.pat_loc "construct in let"
    | Tpat_variant _ -> out_of_scope pat.pat_loc "variant in let"
    | Tpat_record _ -> out_of_scope pat.pat_loc "record in let"
    | Tpat_array _ -> out_of_scope pat.pat_loc "array in let"
    | Tpat_or _ -> out_of_scope pat.pat_loc "or in let"
    | Tpat_lazy _ -> out_of_scope pat.pat_loc "lazy"
      (*  error ~loc:pat.pat_loc "let can't deconstruct values"  *)
    in
  let new_sm = update_shadow_map new_sm pat.pat_env id in
  let sid = ppf_ident id new_sm in
  let sm = if recur = Recursive then update_shadow_map sm pat.pat_env id else sm in
  let js_expr = js_of_expression sm ctx (Dest_assign (sid, false (*FIXME*))) expr in
  (sid, js_expr, new_sm)

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
and js_of_pattern sm pat obj =
  let loc = pat.pat_loc in
  match pat.pat_desc with
  | Tpat_any -> 
     "default", [], sm
  | Tpat_constant c -> 
     ppf_match_case (js_of_constant c), [], sm
  | Tpat_construct (_, cd, el) ->
     let c = cd.cstr_name in
     let spat = if is_sbool c || is_mode_pseudo() then ppf_match_case c else ppf_match_case ("\"" ^ c ^ "\"") in
     let bind sm field var =
        match var.pat_desc with
        | Tpat_var (id, _) ->
            let sm = update_shadow_map sm var.pat_env id in
            let sid = ppf_ident id sm in
            Some (sm, (sid, Printf.sprintf "%s.%s" obj field))
        | Tpat_any -> None
        | _ -> out_of_scope var.pat_loc "Nested pattern matching"
        in
     let sm, binders = map_cstr_fields ~loc sm bind cd el in
     spat, binders, sm
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
  let (content,names_bound) = js_of_structure typedtree in
  let pre_res = ppf_module_wrap module_name content names_bound in
  let str_ppf = Format.str_formatter in
  begin match !current_mode with
  | Mode_unlogged TokenTrue | Mode_pseudo TokenTrue ->
    Format.pp_set_tags str_ppf true;
    Format.pp_set_mark_tags str_ppf true;
    Format.pp_set_formatter_tag_functions str_ppf
     { Format.mark_open_tag = (fun t -> Printf.sprintf "#<%s#" t);
       Format.mark_close_tag = (fun t -> Printf.sprintf "#%s>#" t);
       Format.print_open_tag = (fun _ -> ());
       Format.print_close_tag = (fun _ -> ()) };
  | _ ->
    Format.pp_set_tags str_ppf false;
    Format.pp_set_mark_tags str_ppf false;
    Format.pp_set_formatter_tag_functions str_ppf
     { Format.mark_open_tag = (fun t -> "");
       Format.mark_close_tag = (fun t -> "");
       Format.print_open_tag = (fun _ -> ());
       Format.print_close_tag = (fun _ -> ()) };
  end;
  Format.fprintf str_ppf (Scanf.format_from_string pre_res "");
  Format.flush_str_formatter ()


