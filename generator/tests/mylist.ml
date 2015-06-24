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

let length l = fold_left (fun acc x -> incr acc) 0 l

let rec range i j acc = if le i j then range (incr i) j (Cons (i, acc)) else acc

(* Test *)

let list0 = Nil
let list1 = range 0 1 Nil
let list2 = range 1 5 Nil

let sqr x = mul x x;;

length list0;;
length list1;;
length list2;;
map sqr list0;;
map sqr list1;;
map sqr list2;;
