open Mocha

let myadd x y = x +. y

let subst x = x-1

;;

describe "arith.ml" (fun _ ->
  describe "myadd x y" (fun _ ->
    it "1 2" (fun _ ->
      assert_float (myadd 1. 2.) 3. "!= 3"
    );
    it "-1 1" (fun _ ->
      assert_float (myadd 1. (-1.)) 0. "!= 0"
    )
  );
  describe "subst x" (fun _ ->
    it "1" (fun _ ->
      assert_int (subst 1) 0 "!= 0"
    );
    it "0" (fun _ ->
      assert_int (subst 0) (-1) "!= -1"
    )
  )
)
