(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(* Functions backported from OCaml 4.06 *)
(* TODO: Remove this file once js_of_ast.ml is converted to 4.06 *)

module Make(Ord:Map.OrderedType) =
  struct
    include Map.Make(Ord)
    type 'a impl =
        Empty
      | Node of {l:'a impl; v:key; d:'a; r:'a impl; h:int}

    external impl_of_t : 'a t -> 'a impl = "%identity"
    external t_of_impl : 'a impl -> 'a t = "%identity"

    let height = function
        Empty -> 0
      | Node {h} -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node {h} -> h in
      let hr = match r with Empty -> 0 | Node {h} -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node{l=ll; v=lv; d=ld; r=lr} ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node{l=lrl; v=lrv; d=lrd; r=lrr}->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node{l=rl; v=rv; d=rd; r=rr} ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node{l=rll; v=rlv; d=rld; r=rlr} ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node {l=Empty; r} -> r
      | Node {l; v; d; r} -> bal (remove_min_binding l) v d r

    let merge' t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding (t_of_impl t2) in
          bal t1 x d (remove_min_binding t2)

    let update x f t =
      let rec update' x f = function
          Empty ->
            begin match f None with
            | None -> Empty
            | Some data -> Node{l=Empty; v=x; d=data; r=Empty; h=1}
            end
        | Node {l; v; d; r; h} as m ->
            let c = Ord.compare x v in
            if c = 0 then begin
              match f (Some d) with
              | None -> merge' l r
              | Some data ->
                  if d == data then m else Node{l; v=x; d=data; r; h}
            end else if c < 0 then
              let ll = update' x f l in
              if l == ll then m else bal ll v d r
            else
              let rr = update' x f r in
              if r == rr then m else bal l v d rr
        in t_of_impl (update' x f (impl_of_t t))

    let rec find_opt x m =
      match (impl_of_t m) with
        Empty ->
          None
      | Node {l; v; d; r} ->
          let c = Ord.compare x v in
          if c = 0 then Some d
          else find_opt x (t_of_impl (if c < 0 then l else r))
  end

