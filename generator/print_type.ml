open Misc
open Asttypes
open Types
open Typedtree
open Mytools
open Longident
open Format
open Ctype
open Path
open Asttypes
open Btype
open Printtyp
open Outcometree

(** This file contains a data structure for representing types in an
    explicit form, as well as an algorithm for extracting such types
    from the representation used by OCaml's compiler. *)

(*#########################################################################*)
(* ** Simple representation of types, called [btyp] *)

type btyp =
   | Btyp_alias of btyp * string
   | Btyp_arrow of btyp * btyp
   | Btyp_constr of Path.t * btyp list
   | Btyp_tuple of btyp list
   | Btyp_var of bool * string 
   | Btyp_poly of string list * btyp
   | Btyp_val 

   (*--later:
   | Btyp_abstract
   | Btyp_stuff of string
   | Btyp_manifest of out_type * out_type
   | Btyp_record of (string * bool * out_type) list
   | Btyp_object of (string * out_type) list * bool option
   | Btyp_class of bool * out_ident * out_type list
   | Btyp_sum of (string * out_type list) list 
   *)

(*#########################################################################*)
(* ** Helper functions *)

(** Gathering of free type variables of a btyp *)

type occ = Occ_gen of type_expr | Occ_alias of type_expr 
let occured : (occ list) ref = ref []
let add_occured t = 
  if not (List.memq t !occured) 
     then occured := t :: !occured
let extract_occured () =
   let r = List.rev !occured in
   occured := [];
   r

(** Wrapper for functions from [Printtyp.ml] *)

let mark_loops = mark_loops 

let name_of_type ty = 
   let ty = proxy ty in
   let x = Printtyp.name_of_type ty in
   "_" ^ (String.uppercase x)

let reset_names = reset_names 


(*#########################################################################*)
(* ** Generation of simple type representations *)

(** Algorithm translating an OCaml's typechecker type into a btyp *)

let rec btree_of_typexp sch ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names && not (List.memq px !delayed) then
   let mark = is_non_gen sch ty in
   if is_aliased px && aliasable ty 
      then Btyp_val (* todo: hack ok ? *)
      else Btyp_var (mark, name_of_type px) else

  let pr_typ () =
    match ty.desc with
    | Tvar _ ->
        add_occured (Occ_gen ty);
        Btyp_var (is_non_gen sch ty, name_of_type ty)
    | Tarrow(l, ty1, ty2, _) ->
        (* with labels
        let pr_arrow l ty1 ty2 =
          let lab =
            if !print_labels &&  l <> "" || is_optional l then l else ""
          in
          let t1 =
            if is_optional l then
              match (repr ty1).desc with
              | Tconstr(path, [ty], _)
                when Path.same path Predef.path_option ->
                  btree_of_typexp sch ty
              | _ -> Btyp_stuff "<hidden>"
            else btree_of_typexp sch ty1 in
          Btyp_arrow (lab, t1, btree_of_typexp sch ty2) in
        pr_arrow l ty1 ty2
        *)
        let b1 = btree_of_typexp sch ty1 in
        let b2 = btree_of_typexp sch ty2 in
        ignore (b1,b2);
         Btyp_arrow (b1, b2) 
    | Ttuple tyl ->
        Btyp_tuple (btree_of_typlist sch tyl)
    | Tconstr(p, tyl, abbrev) ->
        Btyp_constr (p, btree_of_typlist sch tyl)
    | Tvariant row -> unsupported "variant"
    | Tobject (fi, nm) -> unsupported "object"
    | Tsubst ty ->
        btree_of_typexp sch ty
    | Tlink _ | Tnil | Tfield _ ->
        fatal_error "Printtyp.btree_of_typexp"
    | Tpoly (ty, []) -> 
        btree_of_typexp sch ty
    | Tpoly (ty, tyl) -> 
        let tyl = List.map repr tyl in
        (* let tyl = List.filter is_aliased tyl in *)
        if tyl = [] then btree_of_typexp sch ty else begin
          let old_delayed = !delayed in
          List.iter add_delayed tyl;
          let tl = List.map name_of_type tyl in
          let tr = Btyp_poly (tl, btree_of_typexp sch ty) in
          delayed := old_delayed; tr
        end
    | Tunivar _ ->
        Btyp_var (false, name_of_type ty)
    | Tpackage _ ->
       unsupported "packaged types"
  in
  if List.memq px !delayed then delayed := List.filter ((!=) px) !delayed;
  if is_aliased px && aliasable ty then begin 
    check_name_of_type px;
    add_occured (Occ_alias ty); (* todo: devrait pas compter ? *)
    Btyp_alias (pr_typ (), name_of_type px) end
  else pr_typ ()

and btree_of_typlist sch tyl =
  List.map (btree_of_typexp sch) tyl


(*#########################################################################*)
(* ** Main functions *)

(** --todo: there is some redundancy with, e.g., [string_of_type_exp] *)

(** Translates a type expression [t] into a [btyp], including the call 
    to [mark_loops]. *)

let btyp_of_typ_exp t =
   mark_loops t;
   btree_of_typexp false t

(** Translates of a type scheme [t] into a [btyp], including the call
    to [mark_loops]. *)

let btyp_of_typ_sch t =
   mark_loops t;
   let typ = btree_of_typexp true t in
   let fvt = extract_occured () in
   let fvtg = list_concat_map (function Occ_gen x -> [x] | _ -> []) fvt in
   let fvta = list_concat_map (function Occ_alias x -> [x] | _ -> []) fvt in
   (fvtg, fvta, typ)


(*#########################################################################*)
(* ** Printing of simple type representations *)

(** Helper functions *)

let ign f () = f

let print_list pr sep =
   show_list pr sep

let pr_vars s =
  print_list (fun s -> sprintf "'%s" s) " " s

(** Printing of paths and identifiers *)

let print_path s = 
   Path.name s

let rec print_ident =
  function
  | Oide_ident s -> sprintf "%s" s
  | Oide_dot (id, s) -> sprintf "%a.%s" (ign print_ident) id s
  | Oide_apply (id1, id2) ->
      sprintf "%a(%a)" (ign print_ident) id1 (ign print_ident) id2
 

(** Printing of types *)

let rec print_out_type =
  function
  | Btyp_val -> "Val"
  | Btyp_alias (ty, s) ->
      sprintf "@[%a as '%s]" (ign print_out_type) ty s
  | Btyp_poly (sl, ty) ->
      sprintf "@[<hov 2>%a.@ %a@]"
        (ign pr_vars) sl
        (ign print_out_type) ty
  | ty ->
      print_out_type_1 ty
and print_out_type_1 =
  function
    Btyp_arrow (ty1, ty2) ->
      sprintf "@[%a -> %a@]" 
        (ign print_out_type_2) ty1 (ign print_out_type_1) ty2
  | ty -> print_out_type_2 ty
and print_out_type_2 =
  function
    Btyp_tuple tyl ->
      sprintf "@[<0>%a@]" (ign (print_typlist print_simple_out_type " *")) tyl
  | ty -> print_simple_out_type ty
and print_simple_out_type =
  function
  | Btyp_constr (id, tyl) ->
      sprintf "@[%a%a@]" (ign print_typargs) tyl (ign print_path) id
  | Btyp_var (ng, s) -> sprintf "'%s%s" (if ng then "_" else "") s 
  | Btyp_val | Btyp_alias _ | Btyp_poly _ | Btyp_arrow _ | Btyp_tuple _ as ty ->
      sprintf "@[<1>(%a)@]" (ign print_out_type) ty
  (*| Btyp_abstract -> ""
    | Btyp_sum _ | Btyp_record _ | Btyp_manifest (_, _)*)
and print_typlist (print_elem : 'a -> string) (sep : string) (t:btyp list) : string =
  match t with
  | [] -> ""
  | [ty] -> print_elem ty
  | ty :: tyl ->
      sprintf "%a%s %a" (ign print_elem) ty sep (ign (print_typlist print_elem sep))
        tyl
and print_typargs =
  function
    [] -> ""
  | [ty1] -> sprintf "%a " (ign print_simple_out_type) ty1
  | tyl -> sprintf "@[<1>(%a)@]@ " (ign (print_typlist print_out_type ",")) tyl


(*#########################################################################*)
(* ** Main functions *)

(** Translates an OCaml's compiler type [t] into a string.
    Boolean parameter [sch] indicates whether free type variables
    should be quantified at head. The function [mark_loops] should
    be called on [t] first for recursive types to be handled correctly. *)

let show_typ sch t =
   print_out_type (btree_of_typexp sch t)

(** Translates a type expression [t] into a string, including the call 
    to [mark_loops]. *)

let string_of_type_exp t =
   mark_loops t;
   show_typ false t

(** Translates of a type scheme [t] into a string, including the call
    to [mark_loops]. *)

let string_of_type_sch fvs t =
   mark_loops t;
   let s = show_typ true t in
   let gs = List.map (fun x -> "'" ^ name_of_type x) (List.rev fvs) in
   if gs <> []
      then sprintf "forall %s. %s" (show_list (fun x->x) " " gs) s
      else s

