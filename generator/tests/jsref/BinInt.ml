open BinNat
open BinPos
open Bool0
open Datatypes

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module Z = 
 struct 
  type t = float
  
  (** val zero : float **)
  
  let zero =
    0.
  
  (** val one : float **)
  
  let one =
    1.
  
  (** val two : float **)
  
  let two =
    ((fun p -> 2. *. p) 1.)
  
  (** val double : float -> float **)
  
  let double x =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p -> ((fun p -> 2. *. p)
      p))
      (fun p -> (~-.) ((fun p -> 2. *. p)
      p))
      x
  
  (** val succ_double : float -> float **)
  
  let succ_double x =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      1.)
      (fun p -> ((fun p -> 1. +. (2. *. p))
      p))
      (fun p -> (~-.)
      (Pos.pred_double p))
      x
  
  (** val pred_double : float -> float **)
  
  let pred_double x =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ -> (~-.)
      1.)
      (fun p ->
      (Pos.pred_double p))
      (fun p -> (~-.) ((fun p -> 1. +. (2. *. p))
      p))
      x
  
  (** val pos_sub : float -> float -> float **)
  
  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        double (pos_sub p q))
        (fun q ->
        succ_double (pos_sub p q))
        (fun _ -> ((fun p -> 2. *. p)
        p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q ->
        pred_double (pos_sub p q))
        (fun q ->
        double (pos_sub p q))
        (fun _ ->
        (Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun q -> (~-.) ((fun p -> 2. *. p)
        q))
        (fun q -> (~-.)
        (Pos.pred_double q))
        (fun _ ->
        0.)
        y)
      x
  
  (** val add : float -> float -> float **)
  
  let add = (+.)
  
  (** val opp : float -> float **)
  
  let opp = (~-.)
  
  (** val succ : float -> float **)
  
  let succ = (+.) 1.
  
  (** val pred : float -> float **)
  
  let pred = (fun x -> x -. 1.)
  
  (** val sub : float -> float -> float **)
  
  let sub = (-.)
  
  (** val mul : float -> float -> float **)
  
  let mul = ( *. )
  
  (** val pow_pos : float -> float -> float **)
  
  let pow_pos z n =
    Pos.iter n (mul z) 1.
  
  (** val pow : float -> float -> float **)
  
  let pow x y =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      1.)
      (fun p ->
      pow_pos x p)
      (fun p ->
      0.)
      y
  
  (** val square : float -> float **)
  
  let square x =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      (Pos.square p))
      (fun p ->
      (Pos.square p))
      x
  
  (** val compare : float -> float -> comparison **)
  
  let compare = fun x y -> if x=y then Eq else if x<y then Lt else Gt
  
  (** val sgn : float -> float **)
  
  let sgn z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      1.)
      (fun p -> (~-.)
      1.)
      z
  
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
  
  (** val geb : float -> float -> bool **)
  
  let geb x y =
    match compare x y with
    | Eq -> true
    | Lt -> false
    | Gt -> true
  
  (** val gtb : float -> float -> bool **)
  
  let gtb x y =
    match compare x y with
    | Eq -> false
    | Lt -> false
    | Gt -> true
  
  (** val eqb : float -> float -> bool **)
  
  let rec eqb x y =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        true)
        (fun p ->
        false)
        (fun p ->
        false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        false)
        (fun q ->
        Pos.eqb p q)
        (fun p0 ->
        false)
        y)
      (fun p ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        false)
        (fun p0 ->
        false)
        (fun q ->
        Pos.eqb p q)
        y)
      x
  
  (** val max : float -> float -> float **)
  
  let max = max
  
  (** val min : float -> float -> float **)
  
  let min = min
  
  (** val abs : float -> float **)
  
  let abs = abs_float
  
  (** val abs_nat : float -> int **)
  
  let abs_nat z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0)
      (fun p ->
      Pos.to_nat p)
      (fun p ->
      Pos.to_nat p)
      z
  
  (** val abs_N : float -> float **)
  
  let abs_N z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      p)
      (fun p ->
      p)
      z
  
  (** val to_nat : float -> int **)
  
  let to_nat z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0)
      (fun p ->
      Pos.to_nat p)
      (fun p ->
      0)
      z
  
  (** val to_N : float -> float **)
  
  let to_N z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      p)
      (fun p ->
      0.)
      z
  
  (** val of_nat : int -> float **)
  
  let of_nat n =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ ->
      0.)
      (fun n0 ->
      (Pos.of_succ_nat n0))
      n
  
  (** val of_N : float -> float **)
  
  let of_N n =
    (fun f0 fp n -> if n=0. then f0 () else fp n)
      (fun _ ->
      0.)
      (fun p ->
      p)
      n
  
  (** val to_pos : float -> float **)
  
  let to_pos z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      1.)
      (fun p ->
      p)
      (fun p ->
      1.)
      z
  
  (** val iter : float -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let iter n f x =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      x)
      (fun p ->
      Pos.iter p f x)
      (fun p ->
      x)
      n
  
  (** val pos_div_eucl : float -> float -> float * float **)
  
  let rec pos_div_eucl a b =
    (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = add (mul ((fun p -> 2. *. p) 1.) r) 1. in
      if ltb r' b
      then ((mul ((fun p -> 2. *. p) 1.) q), r')
      else ((add (mul ((fun p -> 2. *. p) 1.) q) 1.), (sub r' b)))
      (fun a' ->
      let (q, r) = pos_div_eucl a' b in
      let r' = mul ((fun p -> 2. *. p) 1.) r in
      if ltb r' b
      then ((mul ((fun p -> 2. *. p) 1.) q), r')
      else ((add (mul ((fun p -> 2. *. p) 1.) q) 1.), (sub r' b)))
      (fun _ ->
      if leb ((fun p -> 2. *. p) 1.) b then (0., 1.) else (1., 0.))
      a
  
  (** val div_eucl : float -> float -> float * float **)
  
  let div_eucl a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ -> (0.,
      0.))
      (fun a' ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ -> (0.,
        0.))
        (fun p ->
        pos_div_eucl a' b)
        (fun b' ->
        let (q, r) = pos_div_eucl a' b' in
        ((fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
           (fun _ -> ((opp q),
           0.))
           (fun p -> ((opp (add q 1.)),
           (add b r)))
           (fun p -> ((opp (add q 1.)),
           (add b r)))
           r))
        b)
      (fun a' ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ -> (0.,
        0.))
        (fun p ->
        let (q, r) = pos_div_eucl a' b in
        ((fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
           (fun _ -> ((opp q),
           0.))
           (fun p0 -> ((opp (add q 1.)),
           (sub b r)))
           (fun p0 -> ((opp (add q 1.)),
           (sub b r)))
           r))
        (fun b' ->
        let (q, r) = pos_div_eucl a' b' in (q, (opp r)))
        b)
      a
  
  (** val div : float -> float -> float **)
  
  let div a b =
    let (q, x) = div_eucl a b in q
  
  (** val modulo : float -> float -> float **)
  
  let modulo a b =
    let (x, r) = div_eucl a b in r
  
  (** val quotrem : float -> float -> float * float **)
  
  let quotrem a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ -> (0.,
      0.))
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ -> (0.,
        a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (of_N r)))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (of_N r)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ -> (0.,
        a))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((opp (of_N q)), (opp (of_N r))))
        (fun b0 ->
        let (q, r) = N.pos_div_eucl a0 b0 in ((of_N q), (opp (of_N r))))
        b)
      a
  
  (** val quot : float -> float -> float **)
  
  let quot a b =
    fst (quotrem a b)
  
  (** val rem : float -> float -> float **)
  
  let rem a b =
    snd (quotrem a b)
  
  (** val even : float -> bool **)
  
  let even z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      true)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        false)
        (fun p0 ->
        true)
        (fun _ ->
        false)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        false)
        (fun p0 ->
        true)
        (fun _ ->
        false)
        p)
      z
  
  (** val odd : float -> bool **)
  
  let odd z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      false)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        true)
        (fun p0 ->
        false)
        (fun _ ->
        true)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        true)
        (fun p0 ->
        false)
        (fun _ ->
        true)
        p)
      z
  
  (** val div2 : float -> float **)
  
  let div2 z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        (Pos.div2 p))
        (fun p0 ->
        (Pos.div2 p))
        (fun _ ->
        0.)
        p)
      (fun p -> (~-.)
      (Pos.div2_up p))
      z
  
  (** val quot2 : float -> float **)
  
  let quot2 z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 ->
        (Pos.div2 p))
        (fun p0 ->
        (Pos.div2 p))
        (fun _ ->
        0.)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p0 -> (~-.)
        (Pos.div2 p))
        (fun p0 -> (~-.)
        (Pos.div2 p))
        (fun _ ->
        0.)
        p)
      z
  
  (** val log2 : float -> float **)
  
  let log2 z =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
if p <= 1. then f1 () else if mod_float p 2. = 0. then f2p (floor (p /. 2.)) else f2p1 (floor (p /. 2.)))
        (fun p ->
        (Pos.size p))
        (fun p ->
        (Pos.size p))
        (fun _ ->
        0.)
        p0)
      (fun p ->
      0.)
      z
  
  (** val sqrtrem : float -> float * float **)
  
  let sqrtrem n =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ -> (0.,
      0.))
      (fun p ->
      let (s, m) = Pos.sqrtrem p in
      (match m with
       | Pos.IsNul -> (s, 0.)
       | Pos.IsPos r -> (s, r)
       | Pos.IsNeg -> (s, 0.)))
      (fun p -> (0.,
      0.))
      n
  
  (** val sqrt : float -> float **)
  
  let sqrt n =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun p ->
      (Pos.sqrt p))
      (fun p ->
      0.)
      n
  
  (** val gcd : float -> float -> float **)
  
  let gcd a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      abs b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        abs a)
        (fun b0 ->
        (Pos.gcd a0 b0))
        (fun b0 ->
        (Pos.gcd a0 b0))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        abs a)
        (fun b0 ->
        (Pos.gcd a0 b0))
        (fun b0 ->
        (Pos.gcd a0 b0))
        b)
      a
  
  (** val ggcd : float -> float -> float * (float * float) **)
  
  let ggcd a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ -> ((abs b), (0.,
      (sgn b))))
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ -> ((abs a), ((sgn a),
        0.)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in let (aa, bb) = p in (g, (aa, bb)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in
        let (aa, bb) = p in (g, (aa, ((~-.) bb))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ -> ((abs a), ((sgn a),
        0.)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in
        let (aa, bb) = p in (g, (((~-.) aa), bb)))
        (fun b0 ->
        let (g, p) = Pos.ggcd a0 b0 in
        let (aa, bb) = p in (g, (((~-.) aa), ((~-.) bb))))
        b)
      a
  
  (** val testbit : float -> float -> bool **)
  
  let testbit a n =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      odd a)
      (fun p ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        false)
        (fun a0 ->
        Pos.testbit a0 p)
        (fun a0 ->
        negb (N.testbit (Pos.pred_N a0) p))
        a)
      (fun p ->
      false)
      n
  
  (** val shiftl : float -> float -> float **)
  
  let shiftl a n =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      a)
      (fun p ->
      Pos.iter p (mul ((fun p -> 2. *. p) 1.)) a)
      (fun p ->
      Pos.iter p div2 a)
      n
  
  (** val shiftr : float -> float -> float **)
  
  let shiftr a n =
    shiftl a (opp n)
  
  (** val coq_lor : float -> float -> float **)
  
  let coq_lor a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        a)
        (fun b0 ->
        (Pos.coq_lor a0 b0))
        (fun b0 -> (~-.)
        (N.succ_pos (N.ldiff (Pos.pred_N b0) a0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        a)
        (fun b0 -> (~-.)
        (N.succ_pos (N.ldiff (Pos.pred_N a0) b0)))
        (fun b0 -> (~-.)
        (N.succ_pos (N.coq_land (Pos.pred_N a0) (Pos.pred_N b0))))
        b)
      a
  
  (** val coq_land : float -> float -> float **)
  
  let coq_land a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        0.)
        (fun b0 ->
        of_N (Pos.coq_land a0 b0))
        (fun b0 ->
        of_N (N.ldiff a0 (Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        0.)
        (fun b0 ->
        of_N (N.ldiff b0 (Pos.pred_N a0)))
        (fun b0 -> (~-.)
        (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))
        b)
      a
  
  (** val ldiff : float -> float -> float **)
  
  let ldiff a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      0.)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        a)
        (fun b0 ->
        of_N (Pos.ldiff a0 b0))
        (fun b0 ->
        of_N (N.coq_land a0 (Pos.pred_N b0)))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        a)
        (fun b0 -> (~-.)
        (N.succ_pos (N.coq_lor (Pos.pred_N a0) b0)))
        (fun b0 ->
        of_N (N.ldiff (Pos.pred_N b0) (Pos.pred_N a0)))
        b)
      a
  
  (** val coq_lxor : float -> float -> float **)
  
  let coq_lxor a b =
    (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
      (fun _ ->
      b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        a)
        (fun b0 ->
        of_N (Pos.coq_lxor a0 b0))
        (fun b0 -> (~-.)
        (N.succ_pos (N.coq_lxor a0 (Pos.pred_N b0))))
        b)
      (fun a0 ->
      (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
        (fun _ ->
        a)
        (fun b0 -> (~-.)
        (N.succ_pos (N.coq_lxor (Pos.pred_N a0) b0)))
        (fun b0 ->
        of_N (N.coq_lxor (Pos.pred_N a0) (Pos.pred_N b0)))
        b)
      a
  
  (** val eq_dec : float -> float -> bool **)
  
  let eq_dec x y =
    ((fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
       (fun _ y0 ->
       (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
         (fun _ ->
         true)
         (fun p ->
         false)
         (fun p ->
         false)
         y0)
       (fun x0 y0 ->
       (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
         (fun _ ->
         false)
         (fun p0 ->
         if Pos.eq_dec x0 p0 then true else false)
         (fun p0 ->
         false)
         y0)
       (fun x0 y0 ->
       (fun f0 fp fn z -> if z=0. then f0 () else if z>0. then fp z else fn (~-. z))
         (fun _ ->
         false)
         (fun p0 ->
         false)
         (fun p0 ->
         if Pos.eq_dec x0 p0 then true else false)
         y0)
       x) y
  
  module Private_BootStrap = 
   struct 
    
   end
  
  module Private_OrderTac = 
   struct 
    module IsTotal = 
     struct 
      
     end
    
    module Tac = 
     struct 
      
     end
   end
  
  (** val sqrt_up : float -> float **)
  
  let sqrt_up a =
    match compare 0. a with
    | Eq -> 0.
    | Lt -> succ (sqrt (pred a))
    | Gt -> 0.
  
  (** val log2_up : float -> float **)
  
  let log2_up a =
    match compare 1. a with
    | Eq -> 0.
    | Lt -> succ (log2 (pred a))
    | Gt -> 0.
  
  module Private_NZDiv = 
   struct 
    
   end
  
  module Private_Div = 
   struct 
    module Quot2Div = 
     struct 
      (** val div : float -> float -> float **)
      
      let div =
        quot
      
      (** val modulo : float -> float -> float **)
      
      let modulo =
        rem
     end
    
    module NZQuot = 
     struct 
      
     end
   end
  
  (** val lcm : float -> float -> float **)
  
  let lcm a b =
    abs (mul a (div b (gcd a b)))
  
  (** val b2z : bool -> float **)
  
  let b2z = function
  | true -> 1.
  | false -> 0.
  
  (** val setbit : float -> float -> float **)
  
  let setbit a n =
    coq_lor a (shiftl 1. n)
  
  (** val clearbit : float -> float -> float **)
  
  let clearbit a n =
    ldiff a (shiftl 1. n)
  
  (** val lnot : float -> float **)
  
  let lnot a =
    pred (opp a)
  
  (** val ones : float -> float **)
  
  let ones n =
    pred (shiftl 1. n)

end
