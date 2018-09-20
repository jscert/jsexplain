open Mocha

let test0 x =
  let y = 2*x in
  y+y


let foo x =
  let abr = 1
  and bli = 4 in
  if true then abr else bli

let app x = x

let app2 = fun x -> x

type carte =
  | As
  | Petite of int [@f petite] 


let affiche x = match x with
  | As -> "As"
  | Petite n -> "Petite"

let pet = Petite 5
let cinq = 5

let test1 b = match b with
  | true -> ()
  | false -> ()

let test2 x = match x with
  | 1 -> ()
  | 2 -> ()
  | _ -> ()

;;

describe "let.ml" (fun _ ->
  it "test0" (fun _ ->
    assert_int (test0 5) 20 "test0 5 === 20";
    assert_int (test0 0) 0 "test0 0 === 0"
  );
  it "foo" (fun _ ->
    assert_int (foo 1) 1 "foo _ === 1";
    assert_int (foo 0) 1 "foo _ === 1"
  );
  it "app" (fun _ ->
    assert_int (app 0) 0 "app x === x";
    assert_string (app "test") "test" "app x === x";
    assert_float (app 1.1) 1.1 "app x === x";
    assert_bool (app true) true "app x === x";
    assert_unit (app ())
  );
  it "app2" (fun _ ->
    assert_int (app2 0) 0 "app2 x === x";
    assert_string (app2 "test") "test" "app2 x === x";
    assert_float (app2 1.1) 1.1 "app2 x === x";
    assert_bool (app2 true) true "app2 x === x";
    assert_unit (app2 ())
  );
  it "affiche" (fun _ ->
    assert_string (affiche As) "As" "affiche As === 'As'";
    assert_string (affiche (Petite 1)) "Petite" "affiche (Petite _) === 'Petite'";
  );
  it "consts" (fun _ ->
    assert_struct_eq pet (Petite 5) "pet === (Petite 5)";
    assert_int cinq 5 "cinq === 5"
  );
  it "test1" (fun _ ->
    assert_unit (test1 true);
    assert_unit (test1 false)
  );
  it "test2" (fun _ ->
    assert_unit (test2 0);
    assert_unit (test2 1);
    assert_unit (test2 2)
  )
)
