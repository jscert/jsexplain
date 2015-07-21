let incr i = add i 1

type 'a liste =
  | Nil
  | Cons [@f hd, tl] of 'a * 'a liste

let head d l = match l with
 | Nil -> d
 | Cons (x, xs) -> x

let tail d l = match l with
 | Nil -> d
 | Cons (x, xs) -> xs

let init l = match l with
 | Nil -> Nil
 | Cons (x, xs) -> xs

let rec last l = match l with
 | Nil -> Nil
 | Cons (x, xs) -> (match xs with
   | Nil -> x
   | _   -> last xs)

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

let rec range i j acc = if i <= j then range (incr i) j (Cons (i, acc)) else acc

(* Test *)

let list0 = Nil
let list1 = range 0 1 Nil
let list2 = range 1 5 Nil

let sqr x = mul x x

let print_list l = 
  let rec aux acc l = match l with
    | Nil -> acc
    | Cons (x, xs) -> print xs; if xs === Nil then aux (x + acc) xs else aux (x + "," + acc) xs
  in "[" + aux "" (rev l) + "]"
;; 
let f = 1 in 
  print (length list0);
  print (length list1);
  print (length list2);
  print (print_list (map sqr list0));
  print (print_list (map sqr list1));
  print (print_list (map sqr list2));
