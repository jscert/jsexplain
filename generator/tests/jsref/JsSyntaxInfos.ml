open JsSyntax
open JsSyntaxAux
open LibBool
open List0

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val add_infos_exp : strictness_flag -> expr -> expr **)

let rec add_infos_exp str e =
  let f = add_infos_exp str in
  (match e with
   | Coq_expr_this -> e
   | Coq_expr_identifier s -> e
   | Coq_expr_literal l -> e
   | Coq_expr_object l -> e
   | Coq_expr_array oes ->
     Coq_expr_array
       (map (fun oe ->
         match oe with
         | Some e0 -> Some (f e0)
         | None -> None) oes)
   | Coq_expr_function (so, ss, fb) ->
     Coq_expr_function (so, ss, (add_infos_funcbody str fb))
   | Coq_expr_access (e1, e2) -> Coq_expr_access ((f e1), (f e2))
   | Coq_expr_member (e0, s) -> Coq_expr_member ((f e0), s)
   | Coq_expr_new (e0, es) -> Coq_expr_new ((f e0), (map f es))
   | Coq_expr_call (e0, es) -> Coq_expr_call ((f e0), (map f es))
   | Coq_expr_unary_op (op, e0) -> Coq_expr_unary_op (op, (f e0))
   | Coq_expr_binary_op (e1, op, e2) ->
     Coq_expr_binary_op ((f e1), op, (f e2))
   | Coq_expr_conditional (e1, e2, e3) ->
     Coq_expr_conditional ((f e1), (f e2), (f e3))
   | Coq_expr_assign (e1, op, e2) -> Coq_expr_assign ((f e1), op, (f e2)))

(** val add_infos_funcbody : strictness_flag -> funcbody -> funcbody **)

and add_infos_funcbody str = function
| Coq_funcbody_intro (p, s) -> Coq_funcbody_intro ((add_infos_prog str p), s)

(** val add_infos_stat : strictness_flag -> label_set -> stat -> stat **)

and add_infos_stat str labs t =
  let opt = fun _ f smth ->
    match smth with
    | Some smth0 -> Some (f smth0)
    | None -> None
  in
  let f = add_infos_stat str label_set_empty in
  let fo = opt __ f in
  let fe = add_infos_exp str in
  let feo = opt __ fe in
  let fsb = add_infos_switchbody str in
  (match t with
   | Coq_stat_expr e -> Coq_stat_expr (fe e)
   | Coq_stat_label (l, t0) ->
     Coq_stat_label (l,
       (add_infos_stat str (label_set_add (Coq_label_string l) labs) t0))
   | Coq_stat_block ts -> Coq_stat_block (map f ts)
   | Coq_stat_var_decl vars ->
     Coq_stat_var_decl
       (map (fun var -> let (s, eo) = var in (s, (feo eo))) vars)
   | Coq_stat_if (e, t0, to0) -> Coq_stat_if ((fe e), (f t0), (fo to0))
   | Coq_stat_do_while (l, t0, e) ->
     Coq_stat_do_while ((label_set_add_empty labs), (f t0), (fe e))
   | Coq_stat_while (l, e, t0) ->
     Coq_stat_while ((label_set_add_empty labs), (fe e), (f t0))
   | Coq_stat_with (e, t0) -> Coq_stat_with ((fe e), (f t0))
   | Coq_stat_throw e -> Coq_stat_throw (fe e)
   | Coq_stat_return eo -> Coq_stat_return (feo eo)
   | Coq_stat_break lopt -> Coq_stat_break lopt
   | Coq_stat_continue lopt -> Coq_stat_continue lopt
   | Coq_stat_try (t0, catch, to0) ->
     Coq_stat_try ((f t0),
       (opt __ (fun c -> let (cs, t1) = c in (cs, (f t1))) catch), (fo to0))
   | Coq_stat_for (l, eo1, eo2, eo3, t0) ->
     Coq_stat_for ((label_set_add_empty labs), (feo eo1), (feo eo2),
       (feo eo3), (f t0))
   | Coq_stat_for_var (l, vars, eo2, eo3, t0) ->
     Coq_stat_for_var ((label_set_add_empty labs),
       (map (fun var -> let (s, eo) = var in (s, (feo eo))) vars), (feo eo2),
       (feo eo3), (f t0))
   | Coq_stat_for_in (l, e1, e2, t0) ->
     Coq_stat_for_in ((label_set_add_empty labs), (fe e1), (fe e2), (f t0))
   | Coq_stat_for_in_var (l, str0, eo, e, t0) ->
     Coq_stat_for_in_var ((label_set_add_empty labs), str0, (feo eo), 
       (fe e), (f t0))
   | Coq_stat_debugger -> Coq_stat_debugger
   | Coq_stat_switch (labs0, e, ts) ->
     Coq_stat_switch ((label_set_add_empty labs0), (fe e), (fsb ts)))

(** val add_infos_switchbody :
    strictness_flag -> switchbody -> switchbody **)

and add_infos_switchbody str ts =
  let fe = add_infos_exp str in
  let fs = add_infos_stat str label_set_empty in
  let f = fun sc ->
    let Coq_switchclause_intro (e, l) = sc in
    Coq_switchclause_intro ((fe e), (map fs l))
  in
  (match ts with
   | Coq_switchbody_nodefault l -> Coq_switchbody_nodefault (map f l)
   | Coq_switchbody_withdefault (l1, s, l2) ->
     Coq_switchbody_withdefault ((map f l1), (map fs s), (map f l2)))

(** val add_infos_prog : strictness_flag -> prog -> prog **)

and add_infos_prog str = function
| Coq_prog_intro (str', els) ->
  let str'' = coq_or str str' in
  Coq_prog_intro (str'', (map (add_infos_element str'') els))

(** val add_infos_element : strictness_flag -> element -> element **)

and add_infos_element str = function
| Coq_element_stat t ->
  Coq_element_stat (add_infos_stat str label_set_empty t)
| Coq_element_func_decl (s, ss, fb) ->
  Coq_element_func_decl (s, ss, (add_infos_funcbody str fb))

