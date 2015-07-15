module Token_generator :
sig
  type token

  val build : int -> token option
  val string_of_token : token -> string
  val token_of_string : string -> token
  val reset : unit -> unit
  val withdraw : unit -> token
end
  =
struct
  type token = int

  let tok = ref 0
  let string_of_token = string_of_int
  let token_of_string = int_of_string
  let build i = if i <= !tok then Some i else None
  let reset () = tok := 0
  let withdraw () = tok := !tok + 1; !tok
end

module Logged
         (G : module type of Token_generator)
         (Sz : sig val size : int end)
       :
sig
  type token
  type token_info
  type ident = string
         
  type ctx_operation =
    | Add of ident
    | Redef of ident
    | Del of ident

  val log_line : string -> ctx_operation -> string
  val logged_output : string -> string
  val unlogged_output : string -> string
end
=
struct
  open Str
  type token = G.token
  type ident = string

  type ctx_operation =
    | Add of ident
    | Redef of ident
    | Del of ident
                 
  type token_info = ctx_operation
                                  
  let info_tbl = Hashtbl.create Sz.size
  let token_delim = "|"

  let token_re =
    regexp (token_delim ^ "[0-9]+" ^ token_delim)
  let endline_re =
    regexp "\n"
  let lfs =
    regexp "\n\\(\\( \\)*\n\\)*"

  let free_token = G.withdraw
           
  let bind_token str =
    let len = String.length str in
    let endline =
      let rec aux i =
        if i < len - 1 then
          if str.[i] = '\n'
          then i
          else aux (i + 1)
        else len
      in aux 0 in
    let token = free_token ()
    in token, String.sub str 0 endline ^ token_delim ^ G.string_of_token token ^ token_delim ^ String.sub str endline (len - endline)
                      
  let token_info = Hashtbl.find info_tbl
                                
  let token_from_line l =
    let len = String.length l in
    let rec extract i acc = match l.[i] with
      | '|' -> G.build acc
      | '0'..'9' -> extract (i - 1) (int_of_char l.[i] * 10 + acc)
      | _ -> None
    in
    if l.[len - 1] = '|' then extract (len - 2) 0
    else None

  let log_line str ctx =
    let token, tokenized = bind_token str in
    Hashtbl.replace info_tbl token ctx;
    tokenized

  let strip_log_info s =
    global_replace token_re "" s

  let lines s =
    let append_token lines =
      List.fold_left
        (fun acc x -> match search_forward token_re x 0 with
                      | exception Not_found -> (None, x) :: acc
                      | _  ->  let m = matched_string x in
                              let m_len = String.length m
                              in (Some (G.token_of_string (String.sub m 1 (m_len - 2))) , String.sub x 0 (String.length x - m_len)) :: acc
        ) [] lines in
    let end_line_markers s =
      let rec build start = match (search_forward endline_re s start) with
        | n -> n :: build (n + 1)
        | exception not_Found -> []
      in build 0 in
    let lines_list = snd @@ List.fold_left (fun (st, acc) ed -> (ed, String.sub s st (ed - st) :: acc)) (0, []) (end_line_markers s)
    in append_token lines_list
                    
  let add_log_info s =
    let buf = Buffer.create 16 in
    let ls = lines s in
    let rec aux i = function
      | [] -> ()
      | (None, str)   :: xs -> Buffer.add_string buf str;
                               aux (i + 1) xs
      | (Some l, str) :: xs -> let log_info = match Hashtbl.find info_tbl l with
                                 | Add x   -> "\nprint (" ^ string_of_int i ^ " + \": Variable\" " ^ x ^ ");\n"
                                 | Redef x -> "o"
                                 | Del x   -> "a"
                               in Buffer.add_string buf str; Buffer.add_string buf log_info;
                                  aux (i + 1) xs
    in aux 1 ls; Buffer.contents buf
                                 
  let logged_output s =
    let str_ppf = Format.str_formatter in
    Format.fprintf str_ppf (Scanf.format_from_string s "");
    add_log_info (Format.flush_str_formatter ())
  (* let bad_output = Format.flush_str_formatter () in *)
  (* let pretty_output = global_replace lfs "\n" bad_output in *)
  (* add_log_info pretty_output *)

  let unlogged_output s =
    let str_ppf = Format.str_formatter in
    let unlogged_info = strip_log_info s in
    Format.fprintf str_ppf (Scanf.format_from_string unlogged_info "");
    Format.flush_str_formatter ()
  (* let bad_output = Format.flush_str_formatter () in *)
  (*     global_replace lfs "\n" bad_output *)
    
    
end
