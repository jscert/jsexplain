open Mocha

let incr i = i + 1

type 'a liste =
  | Nil
  | Cons of 'a * 'a liste [@f hd, tl]

let head d l = match l with
 | Nil -> d
 | Cons (x, xs) -> x

let tail d l = match l with
 | Nil -> d
 | Cons (x, xs) -> xs

let init l = match l with
 | Nil -> Nil
 | Cons (x, xs) -> xs

let rec last d l = match l with
 | Nil -> d
 | Cons (x, xs) -> (match xs with
   | Nil -> x
   | _   -> last d xs)

let rec fold_left f acc l = match l with
 | Nil -> acc
 | Cons (x, xs) -> fold_left f (f acc x) xs

let rec fold_right f l acc = match l with
 | Nil -> acc
 | Cons (x, xs) -> f (fold_right f xs acc) x

let rev_map f l = fold_left (fun acc x -> Cons(f x, acc)) Nil l
let map f l = fold_right (fun acc x -> Cons(f x, acc)) l Nil
let rev l = fold_left (fun acc x -> Cons(x, acc)) Nil l

let length l = fold_left (fun acc x -> incr acc) 0 l

let rec range i j acc = if int_le i j then range (incr i) j (Cons (i, acc)) else acc

;;

(* Tests *)
describe "mylist.ml" (fun _ ->
  let list0 = Nil in
  let list1 = range 0 1 Nil in
  let list2 = range 1 5 Nil in

  let sub x y = x - y in
  let sqr x = x * x in

  it "head" (fun _ ->
    assert_int (head 42 list0) 42 "default returned for head Nil";
    assert_int (head 42 list1) 1 "head value returned";
    assert_int (head 42 list2) 5 "head value returned"
  );
  it "tail" (fun _ ->
    assert_struct_eq (tail list1 list0) list1 "default returned for tail Nil";
    assert_struct_eq (tail list0 list1) (Cons(0,Nil)) "tail list1";
    assert_struct_eq (tail list0 list2) (Cons(4,Cons(3,Cons(2,Cons(1,Nil))))) "tail list2"
  );
  it "init" (fun _ ->
    assert_struct_eq (init list0) Nil "init list0";
    assert_struct_eq (init list1) (Cons(0,Nil)) "init list1";
    assert_struct_eq (init list2) (Cons(4,Cons(3,Cons(2,Cons(1,Nil))))) "init list2"
  );
  it "last" (fun _ ->
    assert_int (last 42 list0) 42 "last list0";
    assert_int (last 42 list1) 0 "last list1";
    assert_int (last 42 list2) 1 "last list2";
  );
  it "fold_left" (fun _ ->
    assert_int (fold_left sub 0 list0) 0 "fold_left sub 0 list0";
    assert_int (fold_left sub 3 list1) 2 "fold_left sub 0 list1";
    assert_int (fold_left sub 0 list2) (-15) "fold_left sub 0 list2"
  );
  it "fold_right" (fun _ ->
    assert_int (fold_right sub list0 0) 0 "fold_right sub 0 list0";
    assert_int (fold_right sub list1 3) 2 "fold_right sub 0 list1";
    assert_int (fold_right sub list2 0) (-15) "fold_right sub 0 list2"
    (* FIXME: The fold_left and fold_right functions should probably not return the same values... *)
  );
  it "rev_map" (fun _ ->
    assert_struct_eq (rev_map sqr list0) Nil "rev_map _ Nil === Nil";
    assert_struct_eq (rev_map sqr list1) (Cons(0, Cons(1,Nil))) "rev_map sqr [1,0] === [0,1]";
    assert_struct_eq (rev_map sqr list2) (Cons(1, Cons(4, Cons(9, Cons(16, Cons(25, Nil)))))) "rev_map sqr [1..5] === [1,4,9,16,25]"
  );
  it "map" (fun _ ->
    assert_struct_eq (map sqr list0) Nil "map _ Nil === Nil";
    assert_struct_eq (map sqr list1) (Cons(1, Cons(0,Nil))) "map sqr [0,1] === [0,1]";
    assert_struct_eq (map sqr list2) (Cons(25, Cons(16, Cons(9, Cons(4, Cons(1, Nil)))))) "map sqr [1..5] === [1,4,9,16,25]"
  );
  it "rev" (fun _ ->
    assert_struct_eq (rev list0) Nil "rev Nil === Nil";
    assert_struct_eq (rev list1) (Cons(0, Cons(1,Nil))) "rev [0,1] === [1,0]";
    assert_struct_eq (rev list2) (Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))) "rev [1..5] === [5..1]"
  );
  it "length" (fun _ ->
    assert_int (length list0) 0 "length [] === 0";
    assert_int (length list1) 2 "length [0..1] === 1";
    assert_int (length list2) 5 "length [1..5] === 5"
  );
)
