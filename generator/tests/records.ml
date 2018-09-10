open Mocha

type pers =
  { name : string
  ; status : string
  ; age : int
  }

let boss =
  { name = "smith"
  ; status = "boss"
  ; age = 48
  }

let ab = boss.age

let newboss1 = { boss with name = "john" }

let newboss2 = { boss with name = "john"; status = "newboss" }

(* Field punning *)
let newboss3 =
  let name = "pun" in
  let status = "awful" in
  let age = 0 in
  { name; status; age }

;;

describe "records.ml" (fun _ ->
  it "boss" (fun _ ->
    assert_string boss.name "smith" "boss name correct";
    assert_string boss.status "boss" "boss status correct";
    assert_int boss.age 48 "boss age correct"
  );
  it "newboss1" (fun _ ->
    assert_string newboss1.name "john" "newboss1 name correct";
    assert_string newboss1.status "boss" "newboss1 status correct";
    assert_int newboss1.age 48 "newboss1 age correct"
  );
  it "newboss2" (fun _ ->
    assert_string newboss2.name "john" "newboss2 name correct";
    assert_string newboss2.status "newboss" "newboss2 status correct";
    assert_int newboss2.age 48 "newboss2 age correct"
  );
  it "newboss3" (fun _ ->
    assert_string newboss3.name "pun" "newboss3 name correct";
    assert_string newboss3.status "awful" "newboss3 status correct";
    assert_int newboss3.age 0 "newboss3 age correct"
  )
)
