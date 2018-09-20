open Mocha

let testp1 x = 
  let (a,b,c) = x in
  a+b


let testa x = 
  x

let testb x = 
  let a = x in a

let testc x = 
  let y = x in 
  let x = y+1 in
  x+y

let testd x =
  let f x = 
    let y = x in y in
  f x

let teste x =
  let y =
    let z = x in z in
  y

let testf x =
  let f a = 
    let y = a in x+y in
  f x


let testg x =
  if x then 1 else 0



let test00 x = 
  match x with
  | None -> 2
  | Some y -> y




let test0 f = 
  match Some 3 with
  | None -> 2
  | Some y -> y


let test1 on = 
  let x = 
    match on with 
    | None -> 0
    | Some n -> n
    in
  2*x

let test2 v =
  let x = 
    let y = 3 in y+1 
    in
  x


let test3 x =
  let y =
    match x with 
    | None -> 1
    | Some z -> z+2
    in
  y

let test4 x =
  match x with
  | None -> 1
  | Some y -> 
    begin match y with
    | None -> 2
    | Some z -> z 
    end


(*
let test4 a =
  let y = a+1 in
  let f x = x in
  f (let x = y in x)

let test5 x =
  match x with
  | None -> 0
  | Some x -> let x = x+x in x+1
*)

(*

let test6 = 
  function z -> match z with (x,y) -> x
*)
(*
let test6 (x,y) =
  x
  *)
  
;;

describe "testctx.ml" (fun _ ->
  it "testp1" (fun _ ->
    assert_int (testp1 (40,2,10)) 42 "testp1 (40,2,10) == 42"
  );
  it "testa" (fun _ ->
    assert_int (testa 42) 42 "testa 42 == 42"
  );
  it "testb" (fun _ ->
    assert_int (testb 42) 42 "testb 42 == 42"
  );
  it "testc" (fun _ ->
    assert_int (testc 42) 85 "testc 42 == 85"
  );
  it "testd" (fun _ ->
    assert_int (testd 42) 42 "testd 42 == 42"
  );
  it "teste" (fun _ ->
    assert_int (teste 42) 42 "teste 42 == 42"
  );
  it "testf" (fun _ ->
    assert_int (testf 42) 84 "testf 42 == 84"
  );
  it "testg" (fun _ ->
    assert_int (testg true) 1 "testg true == 1";
    assert_int (testg false) 0 "testg false == 0"
  );
  it "test00" (fun _ ->
    assert_int (test00 None) 2 "test00 None == 2";
    assert_int (test00 (Some 3)) 3 "test00 (Some 3) == 3"
  );
  it "test1" (fun _ ->
    assert_int (test1 None) 0 "test1 None == 0";
    assert_int (test1 (Some 42)) 84 "test1 (Some 42) == 84"
  );
  it "test2" (fun _ ->
    assert_int (test2 42) 4 "test2 _ == 4";
    assert_int (test2 ()) 4 "test2 _ == 4";
    assert_int (test2 "test") 4 "test2 _ == 4"
  );
  it "test3" (fun _ ->
    assert_int (test3 None) 1 "test3 None == 1";
    assert_int (test3 (Some 42)) 44 "test3 (Some 42) == 44"
  );
  it "test4" (fun _ ->
    assert_int (test4 None) 1 "test4 None == 1";
    assert_int (test4 (Some None)) 2 "test4 Some None == 2";
    assert_int (test4 (Some (Some 42))) 42 "test4 (Some (Some 42)) == 42"
  )
)
