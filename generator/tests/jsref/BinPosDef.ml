open Datatypes
open Peano

module Pos = 
 struct 
  type t = float
  

  (* HACK *)
  let is_even p =
     float_eq (mod_float p 2.) 0.


  (** val succ : float -> float **)
  
  let rec succ x =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p -> (fun p -> 2. *. p)
      (succ p))
      (fun p -> (fun p -> 1. +. (2. *. p))
      p)
      (fun _ -> (fun p -> 2. *. p)
      1.)
      x
  
  (** val add : float -> float -> float **)
  
  let rec add x y =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (fun p -> 2. *. p)
        (add_carry p q))
        (fun q -> (fun p -> 1. +. (2. *. p))
        (add p q))
        (fun _ -> (fun p -> 2. *. p)
        (succ p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (fun p -> 1. +. (2. *. p))
        (add p q))
        (fun q -> (fun p -> 2. *. p)
        (add p q))
        (fun _ -> (fun p -> 1. +. (2. *. p))
        p)
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (fun p -> 2. *. p)
        (succ q))
        (fun q -> (fun p -> 1. +. (2. *. p))
        q)
        (fun _ -> (fun p -> 2. *. p)
        1.)
        y)
      x
  
  (** val add_carry : float -> float -> float **)
  
  and add_carry x y =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (fun p -> 1. +. (2. *. p))
        (add_carry p q))
        (fun q -> (fun p -> 2. *. p)
        (add_carry p q))
        (fun _ -> (fun p -> 1. +. (2. *. p))
        (succ p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (fun p -> 2. *. p)
        (add_carry p q))
        (fun q -> (fun p -> 1. +. (2. *. p))
        (add p q))
        (fun _ -> (fun p -> 2. *. p)
        (succ p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (fun p -> 1. +. (2. *. p))
        (succ q))
        (fun q -> (fun p -> 2. *. p)
        (succ q))
        (fun _ -> (fun p -> 1. +. (2. *. p))
        1.)
        y)
      x
  
  (** val pred_double : float -> float **)
  
  let rec pred_double x =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p -> (fun p -> 1. +. (2. *. p)) ((fun p -> 2. *. p)
      p))
      (fun p -> (fun p -> 1. +. (2. *. p))
      (pred_double p))
      (fun _ ->
      1.)
      x
  
  (** val pred : float -> float **)
  
  let pred x =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p -> (fun p -> 2. *. p)
      p)
      (fun p ->
      pred_double p)
      (fun _ ->
      1.)
      x
  
  (** val pred_N : float -> float **)
  
  let pred_N x =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p -> ((fun p -> 2. *. p)
      p))
      (fun p ->
      (pred_double p))
      (fun _ ->
      0.)
      x
  
  type mask =
  | IsNul [@f]  (** Auto Generated Attributes **)
  | IsPos  [@f label0] of float (** Auto Generated Attributes **)
  | IsNeg [@f]  (** Auto Generated Attributes **)
  
  (** val mask_rect : 'a1 -> (float -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rect f f0 f1 mask = match mask with
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val mask_rec : 'a1 -> (float -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rec f f0 f1 mask = match mask with
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val succ_double_mask : mask -> mask **)
  
  let succ_double_mask mask = match mask with
  | IsNul -> IsPos 1.
  | IsPos p -> IsPos ((fun p -> 1. +. (2. *. p)) p)
  | IsNeg -> IsNeg
  
  (** val double_mask : mask -> mask **)
  
  let double_mask m = match m with
  | IsNul -> IsNul
  | IsPos p -> IsPos ((fun p -> 2. *. p) p)
  | IsNeg -> IsNeg
  
  (** val double_pred_mask : float -> mask **)
  
  let double_pred_mask x =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p -> IsPos ((fun p -> 2. *. p) ((fun p -> 2. *. p)
      p)))
      (fun p -> IsPos ((fun p -> 2. *. p)
      (pred_double p)))
      (fun _ ->
      IsNul)
      x
  
  (** val pred_mask : mask -> mask **)
  
  let pred_mask m = match m with
  | IsNul -> IsNeg
  | IsPos q ->
    ((fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
       (fun p0 -> IsPos
       (pred q))
       (fun p0 -> IsPos
       (pred q))
       (fun _ ->
       IsNul)
       q)
  | IsNeg -> IsNeg
  
  (** val sub_mask : float -> float -> mask **)
  
  let rec sub_mask x y =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        double_mask (sub_mask p q))
        (fun q ->
        succ_double_mask (sub_mask p q))
        (fun _ -> IsPos ((fun p -> 2. *. p)
        p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        succ_double_mask (sub_mask_carry p q))
        (fun q ->
        double_mask (sub_mask p q))
        (fun _ -> IsPos
        (pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p ->
        IsNeg)
        (fun p ->
        IsNeg)
        (fun _ ->
        IsNul)
        y)
      x
  
  (** val sub_mask_carry : float -> float -> mask **)
  
  and sub_mask_carry x y =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        succ_double_mask (sub_mask_carry p q))
        (fun q ->
        double_mask (sub_mask p q))
        (fun _ -> IsPos
        (pred_double p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        double_mask (sub_mask_carry p q))
        (fun q ->
        succ_double_mask (sub_mask_carry p q))
        (fun _ ->
        double_pred_mask p)
        y)
      (fun _ ->
      IsNeg)
      x
  
  (** val sub : float -> float -> float **)
  
  let sub x y =
    match sub_mask x y with
    | IsNul -> 1.
    | IsPos z -> z
    | IsNeg -> 1.
  
  (** val mul : float -> float -> float **)
  
  let rec mul x y =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      add y ((fun p -> 2. *. p) (mul p y)))
      (fun p -> (fun p -> 2. *. p)
      (mul p y))
      (fun _ ->
      y)
      x
  
  (** val iter : float -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let rec iter n f x =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun n' ->
      f (iter n' f (iter n' f x)))
      (fun n' ->
      iter n' f (iter n' f x))
      (fun _ ->
      f x)
      n
  
  (** val pow : float -> float -> float **)
  
  let pow x y =
    iter y (mul x) 1.
  
  (** val square : float -> float **)
  
  let rec square p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 -> (fun p -> 1. +. (2. *. p)) ((fun p -> 2. *. p)
      (add (square p0) p0)))
      (fun p0 -> (fun p -> 2. *. p) ((fun p -> 2. *. p)
      (square p0)))
      (fun _ ->
      1.)
      p
  
  (** val div2 : float -> float **)
  
  let div2 p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      p0)
      (fun p0 ->
      p0)
      (fun _ ->
      1.)
      p
  
  (** val div2_up : float -> float **)
  
  let div2_up p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      succ p0)
      (fun p0 ->
      p0)
      (fun _ ->
      1.)
      p
  
  (** val size_nat : float -> int **)
  
  let rec size_nat p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 -> Pervasives.succ
      (size_nat p0))
      (fun p0 -> Pervasives.succ
      (size_nat p0))
      (fun _ -> Pervasives.succ
      0)
      p
  
  (** val size : float -> float **)
  
  let rec size p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      succ (size p0))
      (fun p0 ->
      succ (size p0))
      (fun _ ->
      1.)
      p
  
  (** val compare_cont : float -> float -> comparison -> comparison **)
  
  let rec compare_cont x y r =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        compare_cont p q r)
        (fun q ->
        compare_cont p q Gt)
        (fun _ ->
        Gt)
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        compare_cont p q Lt)
        (fun q ->
        compare_cont p q r)
        (fun _ ->
        Gt)
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        Lt)
        (fun q ->
        Lt)
        (fun _ ->
        r)
        y)
      x
  
  (** val compare : float -> float -> comparison **)
  
  let compare x y =
    compare_cont x y Eq
  
  (** val min : float -> float -> float **)
  
  let min p p' =
    match compare p p' with
    | Eq -> p
    | Lt -> p
    | Gt -> p'
  
  (** val max : float -> float -> float **)
  
  let max p p' =
    match compare p p' with
    | Eq -> p'
    | Lt -> p'
    | Gt -> p
  
  (** val eqb : float -> float -> bool **)
  
  let rec eqb p q =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        eqb p0 q0)
        (fun p1 ->
        false)
        (fun _ ->
        false)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p1 ->
        false)
        (fun q0 ->
        eqb p0 q0)
        (fun _ ->
        false)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        false)
        (fun p0 ->
        false)
        (fun _ ->
        true)
        q)
      p
  
  (** val leb : float -> float -> bool **)
  
  let leb x y =
    match compare x y with
    | Eq -> true
    | Lt -> true
    | Gt -> false
  
  (** val ltb : float -> float -> bool **)
  
  let ltb x y =
    match compare x y with
    | Eq -> false
    | Lt -> true
    | Gt -> false
  
  (** val sqrtrem_step :
      (float -> float) -> (float -> float) -> (float * mask) -> float * mask **)
  
  let sqrtrem_step f g p =
  let (s, y) = p in
    (match y with
     | IsNul ->
       (((fun p -> 2. *. p) s),
         (sub_mask (g (f 1.)) ((fun p -> 2. *. p) ((fun p -> 2. *. p) 1.))))
     | IsPos r ->
       let s' = (fun p -> 1. +. (2. *. p)) ((fun p -> 2. *. p) s) in
       let r' = g (f r) in
       if leb s' r'
       then (((fun p -> 1. +. (2. *. p)) s), (sub_mask r' s'))
       else (((fun p -> 2. *. p) s), (IsPos r'))
     | IsNeg ->
       (((fun p -> 2. *. p) s),
         (sub_mask (g (f 1.)) ((fun p -> 2. *. p) ((fun p -> 2. *. p) 1.)))))
  
  (** val sqrtrem : float -> float * mask **)
  
  let rec sqrtrem p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p1 ->
        sqrtrem_step (fun x -> (fun p -> 1. +. (2. *. p)) x) (fun x ->
          (fun p -> 1. +. (2. *. p)) x) (sqrtrem p1))
        (fun p1 ->
        sqrtrem_step (fun x -> (fun p -> 2. *. p) x) (fun x ->
          (fun p -> 1. +. (2. *. p)) x) (sqrtrem p1))
        (fun _ -> (1., (IsPos ((fun p -> 2. *. p)
        1.))))
        p0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p1 ->
        sqrtrem_step (fun x -> (fun p -> 1. +. (2. *. p)) x) (fun x ->
          (fun p -> 2. *. p) x) (sqrtrem p1))
        (fun p1 ->
        sqrtrem_step (fun x -> (fun p -> 2. *. p) x) (fun x ->
          (fun p -> 2. *. p) x) (sqrtrem p1))
        (fun _ -> (1., (IsPos
        1.)))
        p0)
      (fun _ -> (1.,
      IsNul))
      p
  
  (** val sqrt : float -> float **)
  
  let sqrt p =
    fst (sqrtrem p)
  
  (** val gcdn : int -> float -> float -> float **)
  
  let rec gcdn n a b =
    (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
      (fun _ ->
      1.)
      (fun n0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun a' ->
        (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
          (fun b' ->
          match compare a' b' with
          | Eq -> a
          | Lt -> gcdn n0 (sub b' a') a
          | Gt -> gcdn n0 (sub a' b') b)
          (fun b0 ->
          gcdn n0 a b0)
          (fun _ ->
          1.)
          b)
        (fun a0 ->
        (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
          (fun p ->
          gcdn n0 a0 b)
          (fun b0 -> (fun p -> 2. *. p)
          (gcdn n0 a0 b0))
          (fun _ ->
          1.)
          b)
        (fun _ ->
        1.)
        a)
      n
  
  (** val gcd : float -> float -> float **)
  
  let gcd a b =
    gcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val ggcdn : int -> float -> float -> float * (float * float) **)
  
  let rec ggcdn n a b =
    (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
      (fun _ -> (1., (a,
      b)))
      (fun n0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun a' ->
        (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
          (fun b' ->
          match compare a' b' with
          | Eq -> (a, (1., 1.))
          | Lt ->
            let (g, p) = ggcdn n0 (sub b' a') a in
            let (ba, aa) = p in (g, (aa, (add aa ((fun p -> 2. *. p) ba))))
          | Gt ->
            let (g, p) = ggcdn n0 (sub a' b') b in
            let (ab, bb) = p in (g, ((add bb ((fun p -> 2. *. p) ab)), bb)))
          (fun b0 ->
          let (g, p) = ggcdn n0 a b0 in
          let (aa, bb) = p in (g, (aa, ((fun p -> 2. *. p) bb))))
          (fun _ -> (1., (a,
          1.)))
          b)
        (fun a0 ->
        (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
          (fun p ->
          let (g, p0) = ggcdn n0 a0 b in
          let (aa, bb) = p0 in (g, (((fun p -> 2. *. p) aa), bb)))
          (fun b0 ->
          let (g, p) = ggcdn n0 a0 b0 in (((fun p -> 2. *. p) g), p))
          (fun _ -> (1., (a,
          1.)))
          b)
        (fun _ -> (1., (1.,
        b)))
        a)
      n
  
  (** val ggcd : float -> float -> float * (float * float) **)
  
  let ggcd a b =
    ggcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val coq_Nsucc_double : float -> float **)
  
  let coq_Nsucc_double x =
    (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
      (fun _ ->
      1.)
      (fun p -> ((fun p -> 1. +. (2. *. p))
      p))
      x
  
  (** val coq_Ndouble : float -> float **)
  
  let coq_Ndouble n =
    (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
      (fun _ ->
      0.)
      (fun p -> ((fun p -> 2. *. p)
      p))
      n
  
  (** val coq_lor : float -> float -> float **)
  
  let rec coq_lor p q =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 -> (fun p -> 1. +. (2. *. p))
        (coq_lor p0 q0))
        (fun q0 -> (fun p -> 1. +. (2. *. p))
        (coq_lor p0 q0))
        (fun _ ->
        p)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 -> (fun p -> 1. +. (2. *. p))
        (coq_lor p0 q0))
        (fun q0 -> (fun p -> 2. *. p)
        (coq_lor p0 q0))
        (fun _ -> (fun p -> 1. +. (2. *. p))
        p0)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        q)
        (fun q0 -> (fun p -> 1. +. (2. *. p))
        q0)
        (fun _ ->
        q)
        q)
      p
  
  (** val coq_land : float -> float -> float **)
  
  let rec coq_land p q =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        coq_Nsucc_double (coq_land p0 q0))
        (fun q0 ->
        coq_Ndouble (coq_land p0 q0))
        (fun _ ->
        1.)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        coq_Ndouble (coq_land p0 q0))
        (fun q0 ->
        coq_Ndouble (coq_land p0 q0))
        (fun _ ->
        0.)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        1.)
        (fun q0 ->
        0.)
        (fun _ ->
        1.)
        q)
      p
  
  (** val ldiff : float -> float -> float **)
  
  let rec ldiff p q =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        coq_Ndouble (ldiff p0 q0))
        (fun q0 ->
        coq_Nsucc_double (ldiff p0 q0))
        (fun _ -> ((fun p -> 2. *. p)
        p0))
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        coq_Ndouble (ldiff p0 q0))
        (fun q0 ->
        coq_Ndouble (ldiff p0 q0))
        (fun _ ->
        p)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        0.)
        (fun q0 ->
        1.)
        (fun _ ->
        0.)
        q)
      p
  
  (** val coq_lxor : float -> float -> float **)
  
  let rec coq_lxor p q =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        coq_Ndouble (coq_lxor p0 q0))
        (fun q0 ->
        coq_Nsucc_double (coq_lxor p0 q0))
        (fun _ -> ((fun p -> 2. *. p)
        p0))
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 ->
        coq_Nsucc_double (coq_lxor p0 q0))
        (fun q0 ->
        coq_Ndouble (coq_lxor p0 q0))
        (fun _ -> ((fun p -> 1. +. (2. *. p))
        p0))
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q0 -> ((fun p -> 2. *. p)
        q0))
        (fun q0 -> ((fun p -> 1. +. (2. *. p))
        q0))
        (fun _ ->
        0.)
        q)
      p
  
  (** val shiftl_nat : float -> int -> float **)
  
  let shiftl_nat p n =
    nat_iter n (fun x -> (fun p -> 2. *. p) x) p
  
  (** val shiftr_nat : float -> int -> float **)
  
  let shiftr_nat p n =
    nat_iter n div2 p
  
  (** val shiftl : float -> float -> float **)
  
  let shiftl p n =
    (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
      (fun _ ->
      p)
      (fun n0 ->
      iter n0 (fun x -> (fun p -> 2. *. p) x) p)
      n
  
  (** val shiftr : float -> float -> float **)
  
  let shiftr p n =
    (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
      (fun _ ->
      p)
      (fun n0 ->
      iter n0 div2 p)
      n
  
  (** val testbit_nat : float -> int -> bool **)
  
  let rec testbit_nat p =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 n ->
      (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
        (fun _ ->
        true)
        (fun n' ->
        testbit_nat p0 n')
        n)
      (fun p0 n ->
      (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
        (fun _ ->
        false)
        (fun n' ->
        testbit_nat p0 n')
        n)
      (fun _ n ->
      (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
        (fun _ ->
        true)
        (fun n0 ->
        false)
        n)
      p
  
  (** val testbit : float -> float -> bool **)
  
  let rec testbit p n =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p0 ->
      (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
        (fun _ ->
        true)
        (fun n0 ->
        testbit p0 (pred_N n0))
        n)
      (fun p0 ->
      (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
        (fun _ ->
        false)
        (fun n0 ->
        testbit p0 (pred_N n0))
        n)
      (fun _ ->
      (fun f0 fp n -> if float_eq n 0. then f0 () else fp n)
        (fun _ ->
        true)
        (fun p0 ->
        false)
        n)
      p
  
  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> float -> 'a1 -> 'a1 **)
  
  let iter_op op =
    let rec iter0 p a =
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if is_even p then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        op a (iter0 p0 (op a a)))
        (fun p0 ->
        iter0 p0 (op a a))
        (fun _ ->
        a)
        p
    in iter0
  
  (** val to_nat : float -> int **)
  
  let to_nat x =
    iter_op plus x (Pervasives.succ 0)
  
  (** val of_nat : int -> float **)
  
  let rec of_nat n =
    (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
      (fun _ ->
      1.)
      (fun x ->
      (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
        (fun _ ->
        1.)
        (fun n0 ->
        succ (of_nat x))
        x)
      n
  
  (** val of_succ_nat : int -> float **)
  
  let rec of_succ_nat n =
    (fun fO fS n -> if int_eq n 0 then fO () else fS (n-1))
      (fun _ ->
      1.)
      (fun x ->
      succ (of_succ_nat x))
      n
 end

