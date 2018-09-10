open Mocha

type stack =
  | C of int * stack [@f value, stack]
  | N [@f]

let is_empty s = s === N

let push x stack = C(x, stack)

let pop stack =
  match stack with
  | C (x, xs) -> x
  | N -> failwith "Empty list"

type expr =
  | Const of int [@f value]
  | Add of expr * expr [@f left, right]
  | Sub of expr * expr [@f left, right]
  | Mul of expr * expr [@f left, right]
  | Div of expr * expr [@f left, right]
  | Pop of sexpr [@f stack]
and sexpr =
  | Emp [@f]
  | Push of expr * sexpr [@f value, stack]

let rec run expr = match expr with
  | Const n -> n
  | Add (ls, rs) -> run ls + run rs
  | Sub (ls, rs) -> run ls - run rs
  | Mul (ls, rs) -> run ls * run rs
  | Div (ls, rs) -> run ls / run rs
  | Pop s -> pop (evals s)
and evals sexpr = match sexpr with
  | Emp -> N
  | Push (v, s) -> push (run v) (evals s)

let rec mapMystack f s = match s with
  | N -> N
  | C (x, xs) -> C (f x, mapMystack f xs)

;;

describe "calc.ml" (fun _ ->
  it "is_empty" (fun _ ->
    assert_ok (is_empty N)                   "N => true";
    assert_ok (not (is_empty (C(0,N))))      "C(0,N) => false";
    assert_ok (not (is_empty (C(1,N))))      "C(1,N) => false";
    assert_ok (not (is_empty (C(0,C(0,N))))) "C(0,C(0,N)) => false"
  );
  it "push" (fun _ ->
    let s = push 0 N in
    assert_struct_eq s (C(0,N)) "push 0 N === C(0,N)";
    let s = push 1 s in
    assert_struct_eq s (C(1,C(0,N))) "push 1 s === C(1,C(0,N))"
  );
  it "pop" (fun _ ->
    assert_int (pop (C (4,N))) 4 "pop S(4,N) === 4";
    assert_failwith (fun _ -> pop N) "Empty list" "Popping an empty list should fail"
  );
  it "run" (fun _ ->
    assert_int (run (Const 42)) 42 "run (Const 42) === 42";
    assert_int (run (Add(Const 10, Const 20))) 30 "run (10+20) === 30";
    assert_int (run (Add (Add(Const 1, Const 2), Const 3))) 6 "run (1+2)+3 === 6";
    assert_int (run (Div(Const 6, Add(Const 1, Const 1)))) 3 "run 6/(1+1) === 3";
    assert_int (run (Mul (Const 2, Const 3))) 6 "run 2*3 === 6";
    assert_int (run (Sub (Sub (Const 6, Const 2), Const 1))) 3 "run (6-2)-1 === 3";
    assert_int (run (Pop (Push (Const 1, Emp)))) 1 "run pop [1] === 1";
    assert_struct_eq (evals Emp) N "evals Emp === N";
    assert_struct_eq (evals (Push(Const 1, Emp))) (C(1,N)) "evals push 1 [] === [1]";
    assert_failwith (fun _ -> run (Pop Emp)) "Empty list" "Popping an empty list should fail"
  );
  it "mapMystack" (fun _ ->
    let f1 x = x + 1 in
    let f2 _ = 0 in
    assert_struct_eq (mapMystack f1 (C(0,C(1,C(2,C(3,N)))))) (C(1,C(2,C(3,C(4,N))))) "mapMystack (+1) [0,1,2,3] === [1,2,3,4]";
    assert_struct_eq (mapMystack f2 (C(0,C(1,C(2,C(3,N)))))) (C(0,C(0,C(0,C(0,N))))) "mapMystack (_->0) [0,1,2,3] === [0,0,0,0]"
  )
)
