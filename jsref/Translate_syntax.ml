open JsNumber
open Parser_syntax
open List

exception CoqSyntaxDoesNotSupport of string
exception Empty_list

let string_to_coq s = s
  
let unary_op_to_coq op : JsSyntax.unary_op =
  match op with
      | Not -> JsSyntax.Unary_op_not
      | TypeOf -> JsSyntax.Unary_op_typeof
      | Positive -> JsSyntax.Unary_op_add
      | Negative -> JsSyntax.Unary_op_neg
      | Pre_Decr -> JsSyntax.Unary_op_pre_decr
      | Post_Decr -> JsSyntax.Unary_op_post_decr
      | Pre_Incr -> JsSyntax.Unary_op_pre_incr
      | Post_Incr -> JsSyntax.Unary_op_post_incr
      | Bitnot -> JsSyntax.Unary_op_bitwise_not
      | Void -> JsSyntax.Unary_op_void

let arith_op_to_coq op : JsSyntax.binary_op =
  begin match op with
    | Plus -> JsSyntax.Binary_op_add
    | Minus -> JsSyntax.Binary_op_sub
    | Times -> JsSyntax.Binary_op_mult
    | Div -> JsSyntax.Binary_op_div
    | Mod -> JsSyntax.Binary_op_mod
    | Bitand -> JsSyntax.Binary_op_bitwise_and
    | Bitor -> JsSyntax.Binary_op_bitwise_or
    | Bitxor -> JsSyntax.Binary_op_bitwise_xor
    | Ursh -> JsSyntax.Binary_op_unsigned_right_shift
    | Lsh -> JsSyntax.Binary_op_left_shift
    | Rsh -> JsSyntax.Binary_op_right_shift
  end

let bin_op_to_coq op : JsSyntax.binary_op =
  match op with
    | Comparison op ->
      begin match op with
              | Lt -> JsSyntax.Binary_op_lt
              | Le -> JsSyntax.Binary_op_le
              | Gt -> JsSyntax.Binary_op_gt
              | Ge -> JsSyntax.Binary_op_ge
              | Equal -> JsSyntax.Binary_op_equal
              | NotEqual -> JsSyntax.Binary_op_disequal
              | TripleEqual -> JsSyntax.Binary_op_strict_equal
              | NotTripleEqual -> JsSyntax.Binary_op_strict_disequal
              | In -> JsSyntax.Binary_op_in
              | InstanceOf -> JsSyntax.Binary_op_instanceof
      end
    | Arith op -> arith_op_to_coq op
    | Boolean op ->
      begin match op with
        | And -> JsSyntax.Binary_op_and
        | Or -> JsSyntax.Binary_op_or
      end
  
let exp_to_literal exp : JsSyntax.literal =
  match exp.exp_stx with
      | Num n -> JsSyntax.Literal_number n
      | String s -> JsSyntax.Literal_string (string_to_coq s)
      | Null -> JsSyntax.Literal_null 
      | Bool b -> JsSyntax.Literal_bool b
      | _ -> raise Parser.InvalidArgument

let rec exp_to_exp exp : JsSyntax.expr =
  let f = exp_to_exp in 
  let string_to_coq_op e = match e with 
    | None -> None
    | Some e -> Some (string_to_coq e) in
  match exp.exp_stx with
      (* Literals *)
      | Num _
      | String _ 
      | Null 
      | Bool _ -> JsSyntax.Expr_literal (exp_to_literal exp)

      | RegExp _ -> raise (CoqSyntaxDoesNotSupport (Pretty_print.string_of_exp false exp))
      | This -> JsSyntax.Expr_this
      | Var v -> JsSyntax.Expr_identifier (string_to_coq v)
      | Delete e -> JsSyntax.Expr_unary_op (JsSyntax.Unary_op_delete, f e)
      | Access (e, v) -> JsSyntax.Expr_member (f e, string_to_coq v)
      | Unary_op (op, e) -> JsSyntax.Expr_unary_op (unary_op_to_coq op, f e)
      | BinOp (e1, op, e2) -> JsSyntax.Expr_binary_op (f e1, bin_op_to_coq op, f e2)
      | Assign (e1, e2)  -> JsSyntax.Expr_assign (f e1, None, f e2)
      | AssignOp (e1, op, e2) -> JsSyntax.Expr_assign (f e1, Some (arith_op_to_coq op), f e2)
      | CAccess (e1, e2) -> JsSyntax.Expr_access (f e1, f e2)
      | Comma (e1, e2) -> JsSyntax.Expr_binary_op (f e1, JsSyntax.Binary_op_coma, f e2)
      | Call (e1, e2s) -> JsSyntax.Expr_call (f e1, map (fun e2 -> f e2) e2s)
      | New (e1, e2s) -> JsSyntax.Expr_new (f e1, map (fun e2 -> f e2) e2s)
      | FunctionExp (s, n, vs, e) -> JsSyntax.Expr_function ((string_to_coq_op n), (map string_to_coq vs), exp_to_funcbody e s)
      | Function (s, n, vs, e) -> JsSyntax.Expr_function ((string_to_coq_op n), (map string_to_coq vs), exp_to_funcbody e s)
      | Obj xs -> JsSyntax.Expr_object (map (fun (pn,p,e) -> 
        (match pn with 
          | PropnameId id -> JsSyntax.Propname_identifier (string_to_coq id)
          | PropnameString s -> JsSyntax.Propname_string (string_to_coq s)
          | PropnameNum n -> JsSyntax.Propname_number n
        ),
        (match p with
          | PropbodyVal -> JsSyntax.Propbody_val (f e)
          | PropbodyGet -> 
            begin match e.exp_stx with 
              | FunctionExp (s, None, vs, e) -> JsSyntax.Propbody_get (exp_to_funcbody e s)
              | Function (s, None, vs, e) -> JsSyntax.Propbody_get (exp_to_funcbody e s) 
              | _ -> raise Parser.InvalidArgument
            end
          | PropbodySet ->
            begin match e.exp_stx with
              | FunctionExp (s, None, vs, e) -> JsSyntax.Propbody_set (map string_to_coq vs, exp_to_funcbody e s) 
              | Function (s, None, vs, e) -> JsSyntax.Propbody_set (map string_to_coq vs, exp_to_funcbody e s) 
              | _ -> raise Parser.InvalidArgument
            end) 
        ) xs)
      (* _ARRAYS_ : Parsing array arguments *)
      | Array oes -> JsSyntax.Expr_array (map (fun oe -> begin match oe with
                                                               | None   -> None
                                                               | Some e -> Some (f e)
                                                              end) oes)
      | ConditionalOp (e1, e2, e3) -> JsSyntax.Expr_conditional (f e1, f e2, f e3)

      (*Statements*)
      | Skip 
      | Return _
      | Break _ 
      | Continue _ 
      | Debugger  
      | VarDec _ 
      | Throw _  
      | Label _
      | While _
      | DoWhile _
      | With _
      | Try _
      | If _
      | ForIn _
      | For _
      | Switch _
      | Block _
      | Script _ -> raise Parser.InvalidArgument

and exp_to_stat exp : JsSyntax.stat =
  let f = exp_to_stat in 
  match exp.exp_stx with
        (* Literals *)
      | Num _
      | String _
      | Null 
      | Bool _
      
      (* Expressions *)
      | RegExp _  
      | This
      | Var _
      | Delete _ 
      | Access _
      | Unary_op _ 
      | BinOp _ 
      | Assign _  
      | AssignOp _
      | CAccess _
      | Comma _
      | Call _
      | New _
      | Obj _
      | Array _ 
      | ConditionalOp _
      | FunctionExp _ -> JsSyntax.Stat_expr (exp_to_exp exp)

      | Function _ -> raise Parser.InvalidArgument
         (* If a function appears in the middle of a statement, it shall not be interpreted as an expression function, but as a function declaration *)
         (* NOTE in spec p.86 *)
         (* ... It is recommended that ECMAScript implementations either disallow this usage of FunctionDeclaration or issue a warning *)

      (*Statements*)
	  | Skip -> JsSyntax.Stat_block []
      | Return (Some e) -> JsSyntax.Stat_return (Some (exp_to_exp e))
      | Return None -> JsSyntax.Stat_return None
      | Break (Some l) -> JsSyntax.Stat_break (JsSyntax.Label_string (string_to_coq l))
      | Break None -> JsSyntax.Stat_break JsSyntax.Label_empty
      | Continue (Some l) -> JsSyntax.Stat_continue (JsSyntax.Label_string (string_to_coq l))
      | Continue None -> JsSyntax.Stat_continue JsSyntax.Label_empty
      | Debugger -> JsSyntax.Stat_debugger
      | VarDec vs -> JsSyntax.Stat_var_decl (map (fun (v, e) ->
          string_to_coq v, match e with None -> None | Some e -> Some (exp_to_exp e)) vs)
      | Throw e -> JsSyntax.Stat_throw (exp_to_exp e)
      | Label (l, e) -> JsSyntax.Stat_label (string_to_coq l, f e)
	  | While (e1, e2)  -> JsSyntax.Stat_while ([], exp_to_exp e1, f e2)
	  | DoWhile (e1, e2) -> JsSyntax.Stat_do_while ([], f e1, exp_to_exp e2)
      | With (e1, e2) -> JsSyntax.Stat_with (exp_to_exp e1, f e2)
      | Try (e, None, None) -> JsSyntax.Stat_try (f e, None, None)
      | Try (e, None, Some fe) -> JsSyntax.Stat_try (f e, None, Some (f fe))
      | Try (e, Some (s, ce), None) -> JsSyntax.Stat_try (f e, Some (string_to_coq s, f ce), None)
      | Try (e, Some (s, ce), Some fe) -> JsSyntax.Stat_try (f e, Some (string_to_coq s, f ce), Some (f fe))  
      | If (e1, e2, Some e3) -> JsSyntax.Stat_if (exp_to_exp e1, f e2, Some (f e3))
      | If (e1, e2, None) -> JsSyntax.Stat_if (exp_to_exp e1, f e2, None)
      | ForIn (e1, e2, e3) -> raise (CoqSyntaxDoesNotSupport (Pretty_print.string_of_exp false exp))
      | For (e1, e2, e3, e4) ->
        let to_option expr = begin match expr with
                                 | None -> None
                                 | Some(real_e) -> Some (exp_to_exp real_e) end in
        (match e1 with
          | Some ({exp_offset; exp_stx = (VarDec vs); exp_annot}) ->
                JsSyntax.Stat_for_var ([], (map (fun (v, e) ->
                    string_to_coq v, match e with None -> None
                        | Some e -> Some (exp_to_exp e)) vs),
                    to_option e2, to_option e3, f e4)
          | _ ->
                  JsSyntax.Stat_for ([], to_option e1, to_option e2, to_option e3, f e4))
      | Switch (e1, e2s) -> 
        let (firstpart, defaultcase, secondpart) = List.fold_left (fun (fi, de, se) el -> (
          if de = None then
          match el with
            | (DefaultCase, {exp_stx = Block es}) -> (fi, Some (List.map f es), [])
            | (Case cexp, {exp_stx = Block es}) -> (fi @ [JsSyntax.Switchclause_intro (exp_to_exp cexp, List.map f es)], None, [])
            | _ -> raise CannotHappen
          else match el with
            | (Case cexp, {exp_stx = Block es}) -> (fi, de, se @ [JsSyntax.Switchclause_intro (exp_to_exp cexp, List.map f es)])
            | _ -> raise CannotHappen           
          )) ([], None, []) e2s in
        let switchbody = match defaultcase with
          | None -> JsSyntax.Switchbody_nodefault firstpart
          | Some de -> JsSyntax.Switchbody_withdefault (firstpart, de, secondpart) in
        JsSyntax.Stat_switch ([], exp_to_exp e1, switchbody)
      | Block es -> JsSyntax.Stat_block (List.map f es)

      | Script _ -> raise Parser.InvalidArgument

and exp_to_prog exp : JsSyntax.prog =
  match exp.exp_stx with
    | Script (s, e2s) -> JsSyntax.Prog_intro (s, map exp_to_elem e2s)
    | Block (e2s) -> JsSyntax.Prog_intro (false, map exp_to_elem e2s)
    | _ ->  JsSyntax.Prog_intro (false, [exp_to_elem exp])

and exp_to_elem exp : JsSyntax.element = 
    let tos = string_to_coq in
    match exp.exp_stx with
      | FunctionExp (s, (Some name), args, body) -> JsSyntax.Element_func_decl (tos name, map tos args, exp_to_funcbody body s)
      | Function (s, (Some name), args, body) -> JsSyntax.Element_func_decl (tos name, map tos args, exp_to_funcbody body s)
      | _ -> JsSyntax.Element_stat (exp_to_stat exp)

and exp_to_funcbody exp strict : JsSyntax.funcbody =
  let body =
	match exp_to_prog exp with
    | JsSyntax.Prog_intro (_, elems) -> JsSyntax.Prog_intro (strict, elems)
  in JsSyntax.Funcbody_intro (body, "")

module JsSyntaxInfos = struct
  open JsSyntax
  open JsSyntaxAux

  let rec add_infos_exp str e =
    let f = add_infos_exp str in
    (match e with
     | Expr_this -> e
     | Expr_identifier s -> e
     | Expr_literal l -> e
     | Expr_object l -> e
     | Expr_array oes ->
       Expr_array
         (List.map (fun oe ->
           match oe with
           | Some e0 -> Some (f e0)
           | None -> None) oes)
     | Expr_function (so, ss, fb) ->
       Expr_function (so, ss, (add_infos_funcbody str fb))
     | Expr_access (e1, e2) -> Expr_access ((f e1), (f e2))
     | Expr_member (e0, s) -> Expr_member ((f e0), s)
     | Expr_new (e0, es) -> Expr_new ((f e0), (List.map f es))
     | Expr_call (e0, es) -> Expr_call ((f e0), (List.map f es))
     | Expr_unary_op (op, e0) -> Expr_unary_op (op, (f e0))
     | Expr_binary_op (e1, op, e2) ->
       Expr_binary_op ((f e1), op, (f e2))
     | Expr_conditional (e1, e2, e3) ->
       Expr_conditional ((f e1), (f e2), (f e3))
     | Expr_assign (e1, op, e2) -> Expr_assign ((f e1), op, (f e2)))

  (** val add_infos_funcbody : strictness_flag -> funcbody -> funcbody **)

  and add_infos_funcbody str = function
  | Funcbody_intro (p, s) -> Funcbody_intro ((add_infos_prog str p), s)

  (** val add_infos_stat : strictness_flag -> label_set -> stat -> stat **)

  and add_infos_stat str labs t =
    let opt = fun f smth ->
      match smth with
      | Some smth0 -> Some (f smth0)
      | None -> None
    in
    let f = add_infos_stat str label_set_empty in
    let fo = opt f in
    let fe = add_infos_exp str in
    let feo = opt fe in
    let fsb = add_infos_switchbody str in
    (match t with
     | Stat_expr e -> Stat_expr (fe e)
     | Stat_label (l, t0) ->
       Stat_label (l,
         (add_infos_stat str (label_set_add (Label_string l) labs) t0))
     | Stat_block ts -> Stat_block (List.map f ts)
     | Stat_var_decl vars ->
       Stat_var_decl
         (List.map (fun var -> let (s, eo) = var in (s, (feo eo))) vars)
     | Stat_if (e, t0, to0) -> Stat_if ((fe e), (f t0), (fo to0))
     | Stat_do_while (l, t0, e) ->
       Stat_do_while ((label_set_add_empty labs), (f t0), (fe e))
     | Stat_while (l, e, t0) ->
       Stat_while ((label_set_add_empty labs), (fe e), (f t0))
     | Stat_with (e, t0) -> Stat_with ((fe e), (f t0))
     | Stat_throw e -> Stat_throw (fe e)
     | Stat_return eo -> Stat_return (feo eo)
     | Stat_break lopt -> Stat_break lopt
     | Stat_continue lopt -> Stat_continue lopt
     | Stat_try (t0, catch, to0) ->
       Stat_try ((f t0),
         (opt (fun c -> let (cs, t1) = c in (cs, (f t1))) catch), (fo to0))
     | Stat_for (l, eo1, eo2, eo3, t0) ->
       Stat_for ((label_set_add_empty labs), (feo eo1), (feo eo2),
         (feo eo3), (f t0))
     | Stat_for_var (l, vars, eo2, eo3, t0) ->
       Stat_for_var ((label_set_add_empty labs),
         (List.map (fun var -> let (s, eo) = var in (s, (feo eo))) vars), (feo eo2),
         (feo eo3), (f t0))
     | Stat_for_in (l, e1, e2, t0) ->
       Stat_for_in ((label_set_add_empty labs), (fe e1), (fe e2), (f t0))
     | Stat_for_in_var (l, str0, eo, e, t0) ->
       Stat_for_in_var ((label_set_add_empty labs), str0, (feo eo), 
         (fe e), (f t0))
     | Stat_debugger -> Stat_debugger
     | Stat_switch (labs0, e, ts) ->
       Stat_switch ((label_set_add_empty labs0), (fe e), (fsb ts)))

  (** val add_infos_switchbody :
      strictness_flag -> switchbody -> switchbody **)

  and add_infos_switchbody str ts =
    let fe = add_infos_exp str in
    let fs = add_infos_stat str label_set_empty in
    let f = fun sc ->
      let Switchclause_intro (e, l) = sc in
      Switchclause_intro ((fe e), (List.map fs l))
    in
    (match ts with
     | Switchbody_nodefault l -> Switchbody_nodefault (List.map f l)
     | Switchbody_withdefault (l1, s, l2) ->
       Switchbody_withdefault ((List.map f l1), (List.map fs s), (List.map f l2)))

  (** val add_infos_prog : strictness_flag -> prog -> prog **)

  and add_infos_prog str = function
  | Prog_intro (str', els) ->
    let str'' = str || str' in
    Prog_intro (str'', (List.map (add_infos_element str'') els))

  (** val add_infos_element : strictness_flag -> element -> element **)

  and add_infos_element str = function
  | Element_stat t ->
    Element_stat (add_infos_stat str label_set_empty t)
  | Element_func_decl (s, ss, fb) ->
    Element_func_decl (s, ss, (add_infos_funcbody str fb))
end

let parse (p : ?force_strict:bool -> string -> Parser_syntax.exp) strictness s =
  let exp = p ~force_strict:strictness s in
  JsSyntaxInfos.add_infos_prog strictness (exp_to_prog exp)

let parse_js_syntax strictness str =
  try Some (parse Parser_main.exp_from_string strictness str)
  with
  | Parser.ParserFailure _ -> (prerr_endline ("Parser failure on input \"" ^ str ^ "\""); None)

let parse_js_syntax_from_file = parse (Parser_main.exp_from_file ~init:false)
