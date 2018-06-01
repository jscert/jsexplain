open Mocha

let test _ =
  let (y,z) = (1,2)
  in y

let test2 _ =
  let _,z = (1,2)
  in z

let test3 _ =
  let (x,y,z) = (1,2,3)
  in (x,y,z)

(*
let test3a _ =
  let (x,x,x) = (1,2,3)
(*       ^--- OCaml Syntax Error expected here *)
  in x
*)

let test4 _ =
  let (w,x,y,z) = (1,2,3,4)
  in (w,x,y,z)
;;

describe "lettuple.ml" (fun _ ->
  it "test" (fun _ ->
    assert_int (test ()) 1 "test () === 1"
  );
  it "test2" (fun _ ->
    assert_int (test2 ()) 2 "test2 () === 2"
  );
  it "test3" (fun _ ->
    assert_struct_eq (test3 ()) (1,2,3) "test3 () === (1,2,3)"
  );
  it "test4" (fun _ ->
    assert_struct_eq (test4 ()) (1,2,3,4) "test4 () === (1,2,3,4)"
  )
)
