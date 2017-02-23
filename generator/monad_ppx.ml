open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let monad_mapping = Monadic_binder_list.monad_mapping

(* e.g. 

  let%some x = expr in cont
becomes
   if_some expr (fun x -> cont)

  let%if_spec (s,x) = expr in cont
becomes
   if_spec expr (fun s x -> cont) 

and in pseudo:
  Let%spec x = expr in
  cont

 *)

let mk_lid ?(loc=Location.none) ident = Location.mkloc (Longident.Lident ident) loc
let mk_ident ?loc ident = Exp.ident (mk_lid ?loc ident)

let generate_mapper namesid = function argv ->
  { default_mapper with
    expr = fun mapper expr ->
        let aux e = mapper.expr mapper e in
        match expr with
        (* Is this an extension node? *)
        | { pexp_desc = Pexp_extension ({txt = name; loc}, pstr)} ->
          begin try
            match pstr with
            | PStr [{ pstr_desc = Pstr_eval ({pexp_loc = loc; pexp_desc = extended_expression}, _)}] ->

              begin match extended_expression with
              (* let%exn bindings *)
              | Pexp_let (rf, [{pvb_pat = pat; pvb_expr = e}], cont) ->
                begin try
                  let ident = List.assoc name namesid in
                  let (param, body) = match pat.ppat_desc with
                    (* let%exn _ = ... or let%exn a = ... *)
                    | Ppat_var _
                    | Ppat_any           -> (pat, aux cont)
                    (* let%exn (a,b) = ... *)
                    | Ppat_tuple [p1;p2] -> (p1, (Exp.fun_ ~loc Nolabel None p2 (aux cont)))
                    | _ -> raise (Location.Error (Location.error ~loc:pat.ppat_loc ("unknown pattern type with let%"^name)))
                  in
                  Exp.apply ~loc (mk_ident ident) [(Nolabel, aux e); (Nolabel, Exp.fun_ ~loc Nolabel None param body)]
                with
                  | Not_found -> raise (Location.Error (Location.error ~loc ("no let%"^name)))
                end

              (* if%exn e_if then e_then else e_else *)
              | Pexp_ifthenelse (e_if, e_then, e_else) ->
                if name <> "ret" then
                  raise (Location.Error (Location.error ~loc ("if%"^ name ^ " extension is unknown")))
                else begin
                  let (e_if, e_then, e_else) = (aux e_if, aux e_then, map_opt aux e_else) in
                  match e_if.pexp_desc with

                  (* if%ret (condition, state) then e_then else e_else *)
                  | Pexp_tuple [condition; state] ->
                    Exp.ifthenelse ~loc condition (Exp.construct ~loc (mk_lid ~loc "Return") (Some e_then)) (Some
                      (Mytools.option_app
                        (Exp.construct ~loc (mk_lid ~loc "Continue") (Some state))
                        (fun rv -> Exp.construct ~loc (mk_lid ~loc "Return") (Some rv))
                        e_else))

                  | _ -> raise (Location.Error (Location.error ~loc:e_if.pexp_loc "conditional of if%ret must syntactically be a pair"))
                end

              | _ -> raise (Location.Error (Location.error ~loc "unable to extend this sort of expression"))
              end
            | _ -> raise (Location.Error ( Location.error ~loc "Expression extension node containing non-expression"))
            with Location.Error error -> {expr with pexp_desc = Pexp_extension (extension_of_error error)}
          end
        (* Delegate to the default mapper. *)
        | x -> default_mapper.expr mapper x;
  }

let () = register "my_monads" (generate_mapper monad_mapping)
