type number = float

let floor = floor;;

let zero = 0.;;
let neg_zero = -0.;;
let one  = 1.;;
let infinity = infinity;;
let neg_infinity = neg_infinity;;
let max_value = max_float;;
let min_value = (Int64.float_of_bits Int64.one);;
let nan = nan;;
let pi  = 4. *. atan 1.;;
let e   = exp 1.;;
let ln2 = log 2.

(* Note that we're using `float_of_string' there, which does not have the same
   behavior than JavaScript.  For instance it will read "022" as 22 instead of
   18, which should be the JavaScript result for it. *)
let from_string s =
  try
    if s = "" then 0. else float_of_string s
  with
    Failure "float_of_string" -> nan

(* Note that this is ugly, we should use the spec of JsNumber.to_string here (9.8.1). *)
let to_string f =
    let sfn = string_of_float f in
    (if (sfn = "inf") then "Infinity" else
     if (sfn = "-inf") then "-Infinity" else
     if (sfn = "nan") then "NaN" else
     let inum = int_of_float f in
     if (float_of_int inum = f)
       then (string_of_int inum)
       else (string_of_float f))

let to_int32 = fun n ->
  match classify_float n with
  | FP_normal | FP_subnormal ->
    let i32 = 2. ** 32. in
    let i31 = 2. ** 31. in
    let posint = (if n < 0. then (-1.) else 1.) *. (floor (abs_float n)) in
    let int32bit =
      let smod = mod_float posint i32 in
      if smod < 0. then smod +. i32 else smod
    in
    (if int32bit >= i31 then int32bit -. i32 else int32bit)
  | _ -> 0.

let to_uint32 = fun n ->
  match classify_float n with
  | FP_normal | FP_subnormal ->
    let i32 = 2. ** 32. in
    let posint = (if n < 0. then (-1.) else 1.) *. (floor (abs_float n)) in
    let int32bit =
      let smod = mod_float posint i32 in
      if smod < 0. then smod +. i32 else smod
    in
    int32bit
  | _ -> 0.

let int32_left_shift x y =
  Int32.to_float (Int32.shift_left (Int32.of_float x) (int_of_float y))
let int32_right_shift x y =
  Int32.to_float (Int32.shift_right (Int32.of_float x) (int_of_float y))

let int32_bitwise_and x y =
  Int32.to_float (Int32.logand (Int32.of_float x) (Int32.of_float y))
let int32_bitwise_or x y =
  Int32.to_float (Int32.logor (Int32.of_float x) (Int32.of_float y))
let int32_bitwise_xor x y =
  Int32.to_float (Int32.logxor (Int32.of_float x) (Int32.of_float y))
let int32_bitwise_not x =
  Int32.to_float (Int32.lognot (Int32.of_float x))

let uint32_right_shift x y =
  let i31 = 2. ** 31. in
  let i32 = 2. ** 32. in
  let newx = if x >= i31 then x -. i32 else x in
  let r = Int32.to_float (Int32.shift_right_logical (Int32.of_float newx) (int_of_float y)) in
  if r < 0. then r +. i32 else r

let neg = (~-.)
let sign f = float_of_int (compare f 0.)
let absolute = abs_float

let modulo_32 x =
  let r = mod_float x 32. in
  if x < 0. then r +. 32. else r

let fmod = mod_float

let isnan n = match classify_float n with
  | FP_nan -> true
  | _      -> false

let isposzero n = (n = 0.) && (copysign 1. n > 0.)
let isnegzero n = (n = 0.) && (copysign 1. n < 0.)
