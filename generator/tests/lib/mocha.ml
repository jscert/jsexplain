(** Mocha-compatible shim for OCaml, OCaml implementation, see mocha.mli for documentation *)

let caller = ref ""
let test_path = ref None
let tests = ref []
let test_cases = ref []

let opt_iter f = function
  | None -> ()
  | Some v -> f v

let opt_default f def = function
  | None -> def
  | Some v -> f v

let end_caller () =
  tests := (!caller, List.rev !test_cases) :: !tests;
  test_cases := []

let update_caller () =
  if !test_path = None then
  let open Printexc in
  let opt_slots = backtrace_slots (get_callstack 3) in
  opt_iter (fun slots ->
    let opt_loc = Slot.location slots.(2) in
    opt_iter (fun loc ->
      let new_caller = loc.filename in
      if new_caller <> !caller then (end_caller (); caller := new_caller)
      ) opt_loc
  ) opt_slots


let get_test_path path test_name =
  opt_default (fun p -> Printf.sprintf "%s:%s" p test_name) test_name path

let update_test_path app =
  let old_path = !test_path in
  test_path := Some (get_test_path old_path app);
  old_path


let describe str f_suite =
  update_caller ();
  let old_path = update_test_path str in
  f_suite ();
  test_path := old_path

let it str f_test =
  update_caller ();
  let name = get_test_path (!test_path) str in
  test_cases := (Alcotest.test_case name `Quick f_test) :: !test_cases

let exit_hooked = ref false
let () = at_exit (fun () ->
  if not !exit_hooked then begin
    exit_hooked := true;
    end_caller ();
    print_string "*** ";
    Alcotest.run "OCaml generated tests ***\n" !tests
  end
)

(* Assertions
   A compatible subset of node.js' assert library, and Alcotest *)
open Alcotest
let assert_ok value msg = check bool msg value true
let assert_fail = fail

let assertion typ act exp msg = check typ msg act exp
let assert_bool = assertion bool
let assert_int = assertion int
let assert_float = assertion (float 0.001)
let assert_char = assertion char
let assert_string = assertion string
let assert_unit x = assertion unit x () "is not unit"
let assert_struct_eq a b x = assertion (testable (Fmt.always "<val>") (=)) a b x
let assert_failwith f e m = check_raises m (Failure e) (fun _ -> f (); ())

