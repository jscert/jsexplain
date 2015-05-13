(** This file contains some helper functions *)


(**************************************************************)
(** Option manipulation functions *)

let option_map f = function
  | None -> None
  | Some x -> Some (f x)

let option_iter f = function
  | None -> ()
  | Some x -> f x

let unsome = function
  | None -> assert false
  | Some v -> v

let option_to_list = function (* todo: rename as [list_of_option] *)
  | None -> []
  | Some v -> [v]

let option_app d f = function
  | None -> d
  | Some x -> f x

let unsome_safe d = function
  | None -> d
  | Some s -> s

let bool_of_option xo =
   match xo with 
   | None -> false 
   | Some x -> x 


(**************************************************************)
(** List manipulation functions *)

let rec list_make n v =
   if n = 0 then [] else v::(list_make (n-1) v)
  
let list_mapi f l =
  let rec aux i = function
    | [] -> []
    | h::t -> (f i h)::(aux (i+1) t)
    in
  aux 0 l

let list_nat n = (* for n >= 0 *)
  let rec aux i = 
    if i = 0 then [] else (i-1)::(aux (i-1)) in
  List.rev (aux n)
    
let rec list_separ sep = function 
  | [] -> []
  | a::[] -> a::[]
  | a::l -> a::sep::(list_separ sep l)

let rec filter_somes = function
  | [] -> []
  | None::l -> filter_somes l
  | (Some x) :: l -> x :: filter_somes l

let list_unique l =
   let rec aux acc = function
      | [] -> acc
      | a::q -> 
         if List.mem a acc then aux acc q else aux (a::acc) q
      in
   aux [] l

let list_intersect l1 l2 =
   List.filter (fun x -> List.mem x l1) l2

let list_minus l1 l2 =
   List.filter (fun t -> not (List.mem t l2)) l1

let list_concat_map f l =
   List.concat (List.map f l)

let list_assoc_option x l =
   try Some (List.assoc x l)
   with Not_found -> None

let rec assoc_list_map f = function
  | [] -> []
  | (k,v)::l -> (k, f v)::(assoc_list_map f l)

let rec list_remove i l = (* i >= 0 *)
   match l with 
   | [] -> failwith "list_remove invalid index" (* todo: illegal argument *)
   | x::t -> if i = 0 then t else x::(list_remove (i-1) t)

let rec list_replace i v l = (* i >= 0 *)
   match l with 
   | [] -> failwith "list_replace invalid index" (* todo: illegal argument *)
   | x::t -> if i = 0 then v::t else x::(list_replace (i-1) v t)

let list_replace_nth i vs xs =
   list_replace i (List.nth vs i) xs 

let list_ksort cmp l =
  List.sort (fun (k1,_) (k2,_) -> cmp k1 k2) l

let list_index k l =
   let rec aux n = function
      | [] -> raise Not_found
      | x::l -> if x = k then n else aux (n+1) l
      in
   aux 0 l 

(**************************************************************)
(** String manipulation functions *)

let str_cmp (s1:string) (s2:string) =
  if s1 < s2 then -1 else if s1 = s2 then 0 else 1

let str_starts_with p s = 
   let n = String.length p in
      String.length s >= n 
   && String.sub s 0 n = p 

let str_replace char1 char2 s =
   let s2 = String.copy s in
   for i = 0 to pred (String.length s) do
      if s2.[i] = char1 then s2.[i] <- char2;
   done;
   s2

let cutlines width s =
   let len = String.length s in
   let b = Buffer.create len in
   let i_last = ref 0 in
   let i = ref 0 in
   while !i < len do
      i := min (!i + width) len;
      while !i < len && (s.[!i]) <> ' ' do
         incr i
      done;
      let line = String.sub s !i_last (!i - !i_last) in
      Buffer.add_string b line;
      Buffer.add_char b '\n';
      incr i;
      i_last := !i;
   done;
   Buffer.contents b

let make_upper s =
   if String.length s <= 0 then s else
   let s' = String.copy s in
   s'.[0] <- Char.uppercase s.[0];
   s'

let make_upper_2 s =
   if String.length s < 2 then s else
   let s' = String.copy s in
   s'.[1] <- Char.uppercase s.[1];
   s'


(**************************************************************)
(** File manipulation functions *)

let file_put_contents filename text =
   try 
      let handle = open_out filename in
      output_string handle text;
      close_out handle
   with Sys_error s -> 
     failwith ("Could not write in file: " ^ filename ^ "\n" ^ s)


(**************************************************************)
(** Try-with manipulation functions *)

(** Tests whether a function throws [Not_found] *)
let gives_not_found f =
   try ignore (f()); false 
   with Not_found -> true


(**************************************************************)
(** Pretty-printing functions *)

let lin0 = ""
let lin1 = "\n"
let lin2 = "\n\n"

let show_list s sep l = 
  String.concat sep (List.map s l)

let show_listp s sep l = 
  if l = [] then "" else
  sep ^ (String.concat sep (List.map s l))

let show_listq s sep l = 
  if l = [] then "" else
  (String.concat sep (List.map s l)) ^ sep

let show_option f ox =
   match ox with
   | None -> ""
   | Some x -> f x

let show_str s =
  s

let show_par required s =
  if required then "(" ^ s ^ ")" else s


(**************************************************************)
(** Error messages *)

let output s = 
  Printf.printf "%s\n" s

let warning s =
  Printf.printf "### WARNING: %s\n" s

let unsupported s =
   failwith ("Unsupported language construction : " ^ s)

