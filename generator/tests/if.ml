type monadic_continue_type =
| Return of int
| Continue of char

let x = if%ret (true, 'a') then 0
let x1 = if true then Return 0 else Continue 'a'
let y = if%ret (false, 'b') then 0
let z = if%ret (false, 'c') then 0 else 1

(* Parans-less tuple syntax *)
let x0 = if%ret true, 'a' then 0
;;

assert (x = Return 0);
assert (y = Continue 'b');
assert (z = Return 1);
assert (x0 = Return 0)
