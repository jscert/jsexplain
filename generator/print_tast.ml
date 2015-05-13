open Misc
open Asttypes
open Types
open Typedtree
open Mytools
open Longident
open Format
open Print_type

(** Printing facility for typed abstract syntax trees produced by the 
    type-checker*)

(*#########################################################################*)
(* ** Printing of base values *)

let string_of_ident s = 
   Ident.name s

let string_of_lident idt =
   let names = Longident.flatten idt in
   String.concat "." names

let string_of_lident_loc li =
   string_of_lident li.txt


let string_of_constant = function
  | Const_int n -> string_of_int n
  | Const_char c -> String.make 1 c
  | Const_string (s, _) -> s
  | Const_float f -> f
  | Const_int32 _ -> unsupported "int32 type"
  | Const_int64 _ -> unsupported "int64 type"
  | Const_nativeint _ -> unsupported "native int type"

let string_of_recflag = function
  | Nonrecursive -> ""
  | Recursive -> " rec" 



(*#########################################################################*)
(* ** Printing of items *)

let string_of_typed_var s t =
   sprintf "(%s : %s)" s (string_of_type_exp t)

let string_of_path p = 
   Path.name p

let show_string s = 
  s

(*#########################################################################*)
(* ** Printing of patterns *)

let string_of_pattern par p =
   let rec aux par p =
     match p.pat_desc with
     | Tpat_any -> "_"
     | Tpat_var (id,_) -> string_of_typed_var (string_of_ident id) p.pat_type
     | Tpat_alias (p, ak, _) -> unsupported "alias patterns"
       (* let sp = aux false p in
         begin match ak with 
         | TPat_alias id -> show_par par (sprintf "%s as %s" (string_of_typed_var (string_of_ident id) p.pat_type) sp) 
         | TPat_constraint _ -> sp
         | TPat_type pp -> sp (* ignore type *)
         end  *)
     | Tpat_constant c -> 
         sprintf "%s" (string_of_constant c)
     | Tpat_tuple l -> 
         show_par true (sprintf "%s" (show_list (aux false) "," l))
     | Tpat_construct (p,cd,ps) -> 
         let c = string_of_lident_loc p in
         if ps = []
            then c
         else if List.length ps = 1 
            then show_par par (c ^ " " ^ aux true (List.hd ps))
         else
            show_par par (sprintf "%s (%s)" c (show_list (aux false) "," ps)) 
     | Tpat_or (p1,p2,_) -> 
         show_par par (sprintf "%s | %s" (aux false p1) (aux false p2))
     | Tpat_lazy p1 -> 
        show_par par (sprintf "lazy %s" (aux true p1))
     | Tpat_variant (_,_,_) -> unsupported "variant patterns"
     | Tpat_record _ -> unsupported "record patterns"
     | Tpat_array pats -> unsupported "array patterns"
     in
  aux false p

let string_of_let_pattern par p =
   let _typ = p.pat_type in
   sprintf "%s" (string_of_pattern par p) 
   (* 
   match p.pat_desc with
   | Tpat_var id -> 
      let typ = p.pat_type in
      sprintf "%s : %s" (string_of_ident id) (string_of_type_sch fvs typ)
   | _ -> unsupported "let-pattern not reduced to a variable"
   *)  

(*#########################################################################*)
(* ** Printing of expression *)

let rec string_of_expression par e =
   let aux ?par e =
      string_of_expression (bool_of_option par) e in
   let aux_pat ?par e =
      string_of_pattern (bool_of_option par) e in
   let string_of_branch b =
      let p = b.c_lhs in
      let e = b.c_rhs in
      (* TODO: check b.c_guard is not used *)
      Format.sprintf "@[@[%s@] ->@ @[%s@]@]" (aux_pat p) (aux e) in
   (*let typ = e.exp_type in*)
   match e.exp_desc with
   | Texp_ident (p,loc,vd) -> string_of_path p (*  string_of_typed_var (string_of_path p) vd.val_type*)
   | Texp_constant c -> string_of_constant c
   | Texp_let (rf, l, e) ->
       let l = List.map (fun b -> (b.vb_pat, b.vb_expr)) l in
       let show_pe (p,e) =
          let sp = (string_of_let_pattern false p) in
          let se = aux e in
          Format.sprintf "%s =@ @[%s@]" sp se in
       let sl = show_list show_pe " and " l in
       let se = aux e in
       Format.sprintf "@[let%s %s in@ @[%s@]@]" (string_of_recflag rf) sl se 
   | Texp_function (_,c1::[], pa) ->
       let _p = c1.c_lhs in
       let e = c1.c_rhs in
       let rec explore pats e =
          match e.exp_desc with 
          | Texp_function (_,c1::[], pa) ->
             let p1 = c1.c_lhs in
             let e1 = c1.c_rhs in
             explore (p1::pats) e1
          | _ -> List.rev pats, e
          in
       let pats,body = explore [] e in
       let sp = show_list aux_pat " " pats in
       let sb = aux ~par:false body in
       let s = Format.sprintf "@[fun @[%s@] ->@ @[%s@]@]" sp sb in
      show_par par s
   | Texp_function (_,l, pa) ->  
       Format.sprintf "@[function %s@]" (show_listp string_of_branch "\n  | " l)
   | Texp_apply (e, l) -> (* todo: afficher les infixes correctement *)
      let l = List.map (fun (lab,eo,opt) -> match eo with None -> unsupported "optional apply arguments" | Some ei -> ei) l in
      let se = aux ~par:true e in
      let show_arg ei =
         let s_ei = aux ~par:false ei in
         let t_ei = string_of_type_exp ei.exp_type in
         sprintf "(%s : %s)" s_ei t_ei in
      let sl = show_list show_arg " " l in
      let s = sprintf "%s %s" se sl in
      show_par par s
   | Texp_match (e, l, le, pa) -> 
       if le <> [] then unsupported "match with exception branches";
       let se = aux e in
       let s = Format.sprintf "@[match@ @[%s@] with@ @[%s@]@]" 
          se (show_list string_of_branch " | " l) in
       show_par par s
   | Texp_try (e,l) -> unsupported "exceptions"
   | Texp_tuple l -> 
       show_par true (show_list aux ", " l)
   | Texp_construct (p, cd, es) -> 
         let c = string_of_lident_loc p in
         if es = []
            then c
         else if List.length es = 1 
            then show_par par (c ^ " " ^ aux ~par:true (List.hd es))
         else
            show_par par (sprintf "%s (%s)" c (show_list aux "," es))
   | Texp_variant (l,eo) -> unsupported "variants"
   | Texp_record (l,Some eo) -> unsupported "record-with"
   | Texp_record (l,None) ->        
       let print_item (p,li,ei) = 
          Format.sprintf "%s = %s" (string_of_lident_loc p) (aux ei) in
       let s = Format.sprintf "@[{%s}@]" (show_list print_item "; " l) in
       show_par par s
   | Texp_field (e,p,i) -> 
       let s = Format.sprintf "@[%s.%s@]" (aux e) (string_of_lident_loc p) in
       show_par par s
   | Texp_setfield (e,p,i,e2) -> 
       let s = Format.sprintf "@[%s.%s <- %s@]" (aux e) (string_of_lident_loc p) (aux e2) in
       show_par par s
   | Texp_array l -> unsupported "array expression" (* Texp_array (List.map aux l)*)
   | Texp_ifthenelse (e1, e2, None) ->
       let s = Format.sprintf "@[if %s@ then %s@]" (aux e1) (aux e2) in
       show_par par s
   | Texp_ifthenelse (e1, e2, Some e3) ->
       let s = Format.sprintf "@[if %s@ then %s@ else %s@]" (aux e1) (aux e2) (aux e3) in
       show_par par s
   | Texp_sequence (e1,e2) -> 
       let s = Format.sprintf "@[%s@ ; %s@]" (aux e1) (aux e2) in
       show_par par s
   | Texp_while (e1,e2) -> 
       let s = Format.sprintf "@[while %s@ do %s@ done@]" (aux e1) (aux e2) in
       show_par par s
   | Texp_for (i,pat,e1,e2,d,e3) -> 
       (* TODO: what is pat? *)
       let s = Format.sprintf "@[for %s = %s to %s do@ %s@ done@]" (Ident.name i) (aux e1) (aux e2) (aux e3) in
       show_par par s
   | Texp_send (_,_,_) -> unsupported "send expression"
   | Texp_new _ -> unsupported "new expression"
   | Texp_instvar (_,_,_) -> unsupported "inst-var expression"
   | Texp_setinstvar (_,_,_,_) -> unsupported "set-inst-var expression"
   | Texp_override _ -> unsupported "Pexp_override expression"
   | Texp_letmodule (_,_,_,_) -> unsupported "let-module expression"
   | Texp_assert e -> 
       let s = Format.sprintf "@[assert %s@]" (aux e) in
       show_par par s
   | Texp_lazy e -> 
       let s = Format.sprintf "@[lazy %s@]" (aux e) in
       show_par par s
   | Texp_object _ -> unsupported "objects"
   | Texp_pack _ -> unsupported "pack"


(*#########################################################################*)
(* ** Printing of type declarations *)

(* TODO

let show_core_type par t =
  let rec aux par t =
     match t.ctyp_desc with
     | Ttyp_any -> "_"
     | Ttyp_var x -> "'"^x
     | Ttyp_arrow (_,t1,t2) -> show_par par (sprintf "%s -> %s" (aux false t1) (aux false t2))
     | Ttyp_tuple ts -> show_par true (show_list (aux true) " * " ts)
     | Ttyp_constr (p,ts) ->
         let args = match ts with
           | [] -> ""
           | [x] -> sprintf "%s" (aux true x)
           | l -> show_par true (show_list (aux false) ", " l)
           in
         sprintf "%s %s" args (string_of_path p) 
     | Ttyp_object _ -> unsupported "object types"
     | Ttyp_class _ -> unsupported "class types"
     | Ttyp_alias _ -> unsupported "alias types"
     | Ttyp_variant  _ -> unsupported "variant types"
     | Ttyp_poly _ -> unsupported "poly types"
     | Ttyp_package _ -> unsupported "package types"
     in
  aux par t


let show_type_decl (name,decl) =
   let show_type t = 
     show_core_type false t in
   let params = 
      match decl.typ_params with
      | [] -> ""
      | [a] -> sprintf "'%s " a
      | l -> show_par true (show_list show_string ", " l) ^ " "
      in
   let header = sprintf " %s%s" params (string_of_ident name) in
   match decl.typ_kind with 
   | Ttype_abstract -> 
       begin match decl.typ_manifest with
       | None -> header
       | Some def -> sprintf "%s = %s" header (show_type def)
       end 
   | Ttype_record _ (* (string * mutable_flag * core_type * Location.t) list *) ->
        unsupported "records type def (todo)"
   | Ttype_variant branches ->
       let show_branch (constr, args, _) =
          match args with
          | [] -> constr
          | [a] -> sprintf "%s of %s" constr (show_type a)
          | l -> sprintf "%s of %s" constr (show_par true (show_list show_type "* " l))
          in
       header ^ " = " ^ show_list show_branch " | " branches

let is_simple_type_decl (name,decl) =
  match decl.typ_kind with
    Ttype_record _ -> true
  | Ttype_abstract -> true
  | _ -> false

*)

(*#########################################################################*)
(* ** Printing of modules and top-level phrases *)

let rec string_of_module m =
   match m.mod_desc with
   | Tmod_ident (p, li) -> string_of_path p
   | Tmod_structure s -> sprintf "struct\n%s\nend\n" (string_of_structure s) 
   | Tmod_functor (id,_,mt,me) -> sprintf "%s : _ ==>%s\n" (string_of_ident id) (string_of_module me) 
   | Tmod_apply (me1,me2,mc) -> sprintf "%s %s" (string_of_module me1) (string_of_module me2)
   | Tmod_constraint (me,_,mt,mc) -> sprintf "(%s : _)" (string_of_module me)
   | Tmod_unpack (_,_) -> unsupported "unpack"

and string_of_structure (s:structure) =
  show_list string_of_structure_item lin2 s.str_items

and string_of_structure_item (si:structure_item) =
   Printtyp.reset();
   match si.str_desc with
   | Tstr_eval (e,_) -> sprintf "let _ = %s" (string_of_expression false e)
   | Tstr_value (r,l) -> 
       let show_pe b =
          let p = b.vb_pat in
          let e = b.vb_expr in
          let sp = string_of_let_pattern false p in
          let se = string_of_expression false e in
          Format.sprintf "%s =@ @[%s@]" sp se in
       let sl = show_list show_pe " and " l in
       Format.sprintf "@[let%s %s@]" (string_of_recflag r) sl 
      (* Format.sprintf "@[let%s %s =@ @[<2>%s@]@]" *)
   | Tstr_primitive _ -> unsupported "primitive"
     (* sprintf "val %s : _" (string_of_ident id) *)
   | Tstr_type l -> sprintf "type _ = _"
   | Tstr_exception _ -> unsupported "exception"
     (* sprintf "exception %s = _" (string_of_ident id) *)
   | Tstr_module mb -> Format.sprintf "@[module %s =@ @[<2>%s] @]" (string_of_ident mb.mb_id) (string_of_module_binding mb)
   | Tstr_recmodule _ -> unsupported "recursive modules"
   | Tstr_modtype _ ->  unsupported "module type declaration"
      (* sprintf "module type %s = _" (string_of_ident id) *)
   | Tstr_open p ->  unsupported "open" (*  sprintf "open %s = _" (string_of_path p) *)
   | Tstr_class _ -> unsupported "objects"
   | Tstr_class_type _ -> unsupported "objects"
   | Tstr_include _ -> unsupported "includ" 
      (* sprintf "include %s" (string_of_module m) *)
  | Tstr_typext _ -> unsupported "extension" 
  | Tstr_attribute _ -> unsupported "attribute" 

and string_of_module_binding mb =
  string_of_module mb.mb_expr