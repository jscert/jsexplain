open Fappli_IEEE_bits
open LibReflect

let nat_eq = int_eq

type number = binary64

(** val nan : number **)

let nan = nan

(** val zero : number **)

let zero = 0.

(** val neg_zero : number **)

let neg_zero = (-0.)

(** val one : float **)

let one = 1.

(** val infinity : number **)

let infinity = infinity

(** val neg_infinity : number **)

let neg_infinity = neg_infinity

(** val max_value : number **)

let max_value = max_float

(** val min_value : number **)

let min_value = (Int64.float_of_bits Int64.one)

(** val pi : number **)

let pi = (4. *. atan 1.)

(** val e : number **)

let e = (exp 1.)

(** val ln2 : number **)

let ln2 = (log 2.)

(** val from_string : string -> number **)

let from_string = (fun s ->
    (*try*)
     (* let s = (String.concat "" (List.map (String.make 1) s)) in ARTHUR hack*)
      if string_eq s "" then 0. else float_of_string s
    (* FIXME: with Failure "float_of_string" -> nan *) )
   (* Note that we're using `float_of_string' there, which does not have the same
      behavior than JavaScript.  For instance it will read "022" as 22 instead of
      18, which should be the JavaScript result for it. *)

(** val to_string : number -> string **)

let to_string = (fun f -> 
    prerr_string ("Warning:  JsNumber.to_string called.  This might be responsible for errors.  Argument value:  " ^ string_of_float f ^ ".");
    prerr_newline();
    let string_of_number n =
      let sfn = string_of_float n in
      (if (string_eq sfn "inf") then "Infinity" else
       if (string_eq sfn  "-inf") then "-Infinity" else
       if (string_eq sfn  "nan") then "NaN" else
       let inum = int_of_float n in
       if (float_of_int inum = n) then (string_of_int inum) else (string_of_float n)) in
    string_of_number f

    (* ARTHUR hack
    let ret = ref [] in (* Ugly, but the API for OCaml string is not very functional... *)
    String.iter (fun c -> ret := c :: !ret) (string_of_number f);
    List.rev !ret
    *)
    )
   (* Note that this is ugly, we should use the spec of JsNumber.to_string here (9.8.1). *)


(** val neg : number -> number **)

let neg = (float_neg)

(** val floor : number -> number **)

let floor = floor

(** val absolute : number -> number **)

let absolute = abs_float

(** val sign : number -> number **)

let sign = (fun f -> float_of_int (float_compare f 0.))

(*
   (** val lt_bool : number -> number -> bool **)

   let lt_bool = (<)

   (** val add : number -> number -> number **)

   let add = (+.)

   (** val sub : number -> number -> number **)

   let sub = (-.)

   (** val fmod : number -> number -> number **)

   let fmod = mod_float

   (** val mult : number -> number -> number **)

   let mult = ( *. )

   (** val div : number -> number -> number **)

   let div = (/.)

*)

(** val number_comparable : number coq_Comparable **)

let number_comparable = (fun n1 n2 -> int_eq 0  (float_compare n1 n2))

(** val of_int : float -> number **)

let of_int = fun x -> x

let number_of_int = fun x -> float_of_int x

(** val to_int32 : number -> float **)


(* ARTHUR hacked this in *)
let classify_float n =
  let x = classify_float n in
  match x with
  | FP_normal -> FP_normal
  | FP_subnormal -> FP_normal
  | _ -> x

let to_int32 = fun n ->
  match classify_float n with
  | FP_normal -> (* ARTHUR hacked this from | FP_normal | FP_subnormal *)
    let i32 = float_exp 2. 32. in
    let i31 = float_exp 2. 31. in
    let posint = (if n < 0. then (-1.) else 1.) *. (floor (abs_float n)) in
    let int32bit =
      let smod = mod_float posint i32 in
      if smod < 0. then smod +. i32 else smod
    in
    (if int32bit >= i31 then int32bit -. i32 else int32bit)
  | _ -> 0.

(** val to_uint32 : number -> float **)

let to_uint32 = fun n ->
  match classify_float n with
  | FP_normal -> (* ARTHUR hacked this from | FP_normal | FP_subnormal *)
    let i32 = float_exp 2. 32. in
    let posint = (if n < 0. then (-1.) else 1.) *. (floor (abs_float n)) in
    let int32bit =
      let smod = mod_float posint i32 in
      if smod < 0. then smod +. i32 else smod
    in
    int32bit
  | _ -> 0.

(** val modulo_32 : float -> float **)

let modulo_32 = (fun x -> let r = mod_float x 32. in if x < 0. then r +. 32. else r)

(** val int32_bitwise_not : float -> float **)

let int32_bitwise_not = fun x -> Int32.to_float (Int32.lognot (Int32.of_float x))

(** val int32_bitwise_and : float -> float -> float **)

let int32_bitwise_and = fun x y -> Int32.to_float (Int32.logand (Int32.of_float x) (Int32.of_float y))

(** val int32_bitwise_or : float -> float -> float **)

let int32_bitwise_or = fun x y -> Int32.to_float (Int32.logor (Int32.of_float x) (Int32.of_float y))

(** val int32_bitwise_xor : float -> float -> float **)

let int32_bitwise_xor = fun x y -> Int32.to_float (Int32.logxor (Int32.of_float x) (Int32.of_float y))

(** val int32_left_shift : float -> float -> float **)

let int32_left_shift = (fun x y -> Int32.to_float (Int32.shift_left (Int32.of_float x) (int_of_float y)))

(** val int32_right_shift : float -> float -> float **)

let int32_right_shift = (fun x y -> Int32.to_float (Int32.shift_right (Int32.of_float x) (int_of_float y)))

(** val uint32_right_shift : float -> float -> float **)

let uint32_right_shift = (fun x y ->
  let i31 = float_exp 2. 31. in
  let i32 = float_exp 2. 32. in
  let newx = if x >= i31 then x -. i32 else x in
  let r = Int32.to_float (Int32.shift_right_logical (Int32.of_float newx) (int_of_float y)) in
  if r < 0. then r +. i32 else r)

