open JsCommon
(*open JsNumber*)
open JsSyntax
open JsSyntaxAux
open LibReflect
open LibString
open Shared

(** val convert_number_to_bool : number -> bool **)

let convert_number_to_bool n =
  if or_decidable (number_comparable n JsNumber.zero)
       (or_decidable (number_comparable n JsNumber.neg_zero)
         (number_comparable n JsNumber.nan))
  then false
  else true

(** val convert_string_to_bool : string -> bool **)

let convert_string_to_bool s =
  if string_comparable s "" then false else true
  (* Arthur hack string.empty *)

(** val convert_prim_to_boolean : prim -> bool **)

let convert_prim_to_boolean _foo_ = match _foo_ with
| Coq_prim_undef -> false
| Coq_prim_null -> false
| Coq_prim_bool b -> b
| Coq_prim_number n -> convert_number_to_bool n
| Coq_prim_string s -> convert_string_to_bool s

(** val convert_value_to_boolean : value -> bool **)

let convert_value_to_boolean _foo_ = match _foo_ with
| Coq_value_prim p -> convert_prim_to_boolean p
| Coq_value_object o -> true

(** val convert_prim_to_number : prim -> number **)

let convert_prim_to_number _foo_ = match _foo_ with
| Coq_prim_undef -> JsNumber.nan
| Coq_prim_null -> JsNumber.zero
| Coq_prim_bool b -> if b then JsNumber.one else JsNumber.zero
| Coq_prim_number n -> n
| Coq_prim_string s -> JsNumber.from_string s

(** val convert_number_to_integer : number -> number **)

let convert_number_to_integer n =
  if number_comparable n JsNumber.nan
  then JsNumber.zero
  else if or_decidable (number_comparable n JsNumber.zero)
            (or_decidable (number_comparable n JsNumber.neg_zero)
              (or_decidable (number_comparable n JsNumber.infinity)
                (number_comparable n JsNumber.neg_infinity)))
       then n
       else  (JsNumber.sign n) *. (JsNumber.floor (JsNumber.absolute n))

(** val convert_bool_to_string : bool -> string **)

let convert_bool_to_string _foo_ = match _foo_ with
| true -> "true"
| false -> "false"

(** val convert_prim_to_string : prim -> string **)

let convert_prim_to_string _foo_ = match _foo_ with
| Coq_prim_undef ->
  "undefined"
| Coq_prim_null -> "null"
| Coq_prim_bool b -> convert_bool_to_string b
| Coq_prim_number n -> JsNumber.to_string n
| Coq_prim_string s -> s

(** val equality_test_for_same_type : coq_type -> value -> value -> bool **)

let equality_test_for_same_type ty v1 v2 =
  match ty with
  | Coq_type_undef -> true
  | Coq_type_null -> true
  | Coq_type_bool -> value_comparable v1 v2
  | Coq_type_number ->
    (match v1 with
     | Coq_value_prim p ->
       (match p with
        | Coq_prim_undef -> false
        | Coq_prim_null -> false
        | Coq_prim_bool b -> false
        | Coq_prim_number n1 ->
          (match v2 with
           | Coq_value_prim p0 ->
             (match p0 with
              | Coq_prim_undef -> false
              | Coq_prim_null -> false
              | Coq_prim_bool b -> false
              | Coq_prim_number n2 ->
                if number_comparable n1 JsNumber.nan
                then false
                else if number_comparable n2 JsNumber.nan
                     then false
                     else if and_decidable (number_comparable n1 JsNumber.zero)
                               (number_comparable n2 JsNumber.neg_zero)
                          then true
                          else if and_decidable
                                    (number_comparable n1 JsNumber.neg_zero)
                                    (number_comparable n2 JsNumber.zero)
                               then true
                               else number_comparable n1 n2
              | Coq_prim_string s -> false)
           | Coq_value_object o -> false)
        | Coq_prim_string s -> false)
     | Coq_value_object o -> false)
  | Coq_type_string -> value_comparable v1 v2
  | Coq_type_object -> value_comparable v1 v2

(** val strict_equality_test : value -> value -> bool **)

let strict_equality_test v1 v2 =
  let ty1 = type_of v1 in
  let ty2 = type_of v2 in
  if type_comparable ty1 ty2
  then equality_test_for_same_type ty1 v1 v2
  else false

(** val inequality_test_number : number -> number -> prim **)

let inequality_test_number n1 n2 =
  if or_decidable (number_comparable n1 JsNumber.nan) (number_comparable n2 JsNumber.nan)
  then Coq_prim_undef
  else if number_comparable n1 n2
       then Coq_prim_bool false
       else if and_decidable (number_comparable n1 JsNumber.zero)
                 (number_comparable n2 JsNumber.neg_zero)
            then Coq_prim_bool false
            else if and_decidable (number_comparable n1 JsNumber.neg_zero)
                      (number_comparable n2 JsNumber.zero)
                 then Coq_prim_bool false
                 else if number_comparable n1 JsNumber.infinity
                      then Coq_prim_bool false
                      else if number_comparable n2 JsNumber.infinity
                           then Coq_prim_bool true
                           else if number_comparable n2 JsNumber.neg_infinity
                                then Coq_prim_bool false
                                else if number_comparable n1 JsNumber.neg_infinity
                                     then Coq_prim_bool true
                                     else Coq_prim_bool (n1 < n2)

(** val inequality_test_string : string -> string -> bool **)

(* ARTHUR hack 
let rec inequality_test_string s1 s2 =
  match s1 with
  | [] ->
    (match s2 with
     | [] -> false
     | a::s -> true)
  | c1::s1' ->
    (match s2 with
     | [] -> false
     | c2::s2' ->
       if ascii_comparable c1 c2
       then inequality_test_string s1' s2'
       else lt_int_decidable (int_of_char c1) (int_of_char c2))
*)
let inequality_test_string s1 s2 = (not (string_eq s1 s2))


(** val inequality_test_primitive : prim -> prim -> prim **)

let inequality_test_primitive w1 w2 =
  match w1 with
  | Coq_prim_undef ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_null ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_bool b ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_number n ->
    inequality_test_number (convert_prim_to_number w1)
      (convert_prim_to_number w2)
  | Coq_prim_string s1 ->
    (match w2 with
     | Coq_prim_undef ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_null ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_bool b ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_number n ->
       inequality_test_number (convert_prim_to_number w1)
         (convert_prim_to_number w2)
     | Coq_prim_string s2 -> Coq_prim_bool (inequality_test_string s1 s2))

(** val typeof_prim : prim -> string **)

let typeof_prim _foo_ = match _foo_ with
| Coq_prim_undef ->
  "undefined"
| Coq_prim_null -> "object"
| Coq_prim_bool b -> "boolean"
| Coq_prim_number n -> "number"
| Coq_prim_string s -> "string"

(** val string_of_propname : propname -> prop_name **)

let string_of_propname _foo_ = match _foo_ with
| Coq_propname_identifier s -> s
| Coq_propname_string s -> s
| Coq_propname_number n -> JsNumber.to_string n

(** val string_of_native_error : native_error -> string **)

let string_of_native_error _foo_ = match _foo_ with
| Coq_native_error_eval ->
  "EvalError"
| Coq_native_error_range ->
  "RangeError"
| Coq_native_error_ref ->
  "ReferenceError"
| Coq_native_error_syntax ->
  "SyntaxError"
| Coq_native_error_type ->
  "TypeError"
| Coq_native_error_uri ->
  "URIError"

