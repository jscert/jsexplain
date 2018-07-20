let monad_mapping = [
    ("string", "if_string");
    ("STRING", "assert_string");
    ("object", "if_object");
    ("OBJECT", "assert_object");
    ("value", "if_value");
    ("VALUE", "assert_value");
    ("prim", "if_prim");
    ("number", "if_number");
    ("some", "if_some");
    ("bool", "if_bool");
    ("BOOL", "assert_bool");
    ("void", "if_void");
    ("success", "if_success");
    ("SUCCESS", "assert_success");
    ("not_throw", "if_not_throw");
    ("ter", "if_ter");
    ("break", "if_break");
    ("spec", "if_spec");
    ("assert", "check_assert");

    ("ret", "let_ret");
    ("some_ret", "if_some_ret");
    ("number_ret", "if_number_ret");
    ("string_ret", "if_string_ret");
    ("STRING_ret", "assert_string_ret");
    ("value_ret", "if_value_ret");
    ("VALUE_ret", "assert_value_ret");
    ("bool_ret", "if_bool_ret");
    ("BOOL_ret", "assert_bool_ret");
    ("object_ret", "if_object_ret");
    ("OBJECT_ret", "assert_object_ret");
    ("assert_ret", "check_assert_ret");
    ("spec_ret", "if_spec_ret");
    ("some_ret", "if_some_ret");
    ("ret_ret", "let_ret_ret");
   ]

let monad_identifiers = List.map (fun (_, f) -> "JsInterpreterMonads." ^ f) monad_mapping

open Ast_helper
open Asttypes
open Parsetree
open Typedtree

let attr_tag = "jsexplain.monad"

let monadic_expr e = Exp.attr e ({ txt = attr_tag; loc = e.pexp_loc }, PStr [])
let is_monadic_attr ({ txt = a; _ }, _) = a = attr_tag
let is_monadic_expr e = List.exists is_monadic_attr e.pexp_attributes
let is_monadic_texpr e = List.exists is_monadic_attr e.exp_attributes
