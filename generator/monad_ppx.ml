open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let monad_mapping =
   [("run", "if_run");
    ("string", "if_string");
    ("object", "if_object");
    ("value", "if_value");
    ("prim", "if_prim");
    ("number", "if_number");
    ("some", "if_some");
    ("bool", "if_bool");
    ("void", "if_void");
    ("success", "if_success");
    ("not_throw", "if_not_throw");
    ("ter", "if_ter");
    ("break", "if_break");
   ]

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


let generate_mapper namesid = function argv ->
  { default_mapper with
    expr = fun mapper expr ->
      let aux e = mapper.expr mapper e in
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
            Pexp_extension ({ txt = name; loc }, pstr)} ->
        begin
          try
            let ident = List.assoc name namesid in
            match pstr with
            | PStr [{ pstr_desc =
                        Pstr_eval ({ pexp_loc  = loc;
                                     pexp_desc = Pexp_let
                                         (rf,
                                          [{pvb_pat = {ppat_desc = Ppat_var _} as p;
                                            pvb_expr = e}],
                                          cont)
                                   }, _)}] ->
              Exp.apply ~loc (Exp.ident
                                (Location.mkloc
                                   (Longident.Lident ident) Location.none))
                [("", aux e);
                 ("", Exp.fun_ "" None p (aux cont))]
            | PStr [{ pstr_desc =
                        Pstr_eval ({ pexp_loc  = loc;
                                     pexp_desc = Pexp_let
                                         (rf,
                                          [{pvb_pat =
                                              {ppat_desc =
                                                 Ppat_tuple [p1;p2]};
                                            pvb_expr = e}],
                                          cont)
                                   }, _)}] ->
              Exp.apply ~loc (Exp.ident
                                (Location.mkloc
                                   (Longident.Lident ident) Location.none))
                [("", aux e);
                 ("", Exp.fun_ "" None p1 (Exp.fun_ "" None p2 (aux cont)))]
            | _ ->
              raise (Location.Error (
                  Location.error ~loc ("error with let%"^name)))
          with
          | Not_found ->
            raise (Location.Error (
                Location.error ~loc ("no let%"^name)))
        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "my_monads" (generate_mapper monad_mapping)
