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
  type typ = string
  type func = string
         
  type ctx_operation =
    | Add of ident * typ
    | CreateCtx of ident
    | ApplyInfix of func * ident * ident
    | ApplyFunc  of func * ident

  val log_line : string -> ctx_operation -> string
  val strip_log_info : string -> string
  val logged_output : string -> string
  val unlogged_output : string -> string
end
=
struct
  open Str
  type token = G.token
  type ident = string
  type typ = string
  type func = string
         
  type ctx_operation =
    | Add of ident * typ
    | CreateCtx of ident
    | ApplyInfix of func * ident * ident
    | ApplyFunc  of func * ident
                 
  type token_info = ctx_operation
                                  
  let info_tbl = Hashtbl.create Sz.size
                                
  let new_margin  = 700
  let token_delim = "|"
  let token_re    = regexp (token_delim ^ "[0-9]+" ^ token_delim)
  let endline_re  = regexp "\n"
  let lfs         = regexp "\n\\(\\( \\)*\n\\)*"

  let to_format s =
    Scanf.format_from_string s ""

  let free_token = G.withdraw
           
  (* Takes a string and inserts a new token after the first new line character or at the end of the string.*)
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
    (* Insert a token after the first '\n' character by creating a substring of before and after. *)
    in token, String.sub str 0 endline ^ token_delim ^ G.string_of_token token ^ token_delim ^ String.sub str endline (len - endline)
  
  (* Appears unused in codebase *)            
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

  (* Helper for lines that looks for all tokens in a line, and
   returns a tuple containing a list of tokens and the detokenized line *)
  let rec line_token_extractor acc pos l =
    match search_forward token_re l pos with
      | exception Not_found -> (acc, l)
      | _  ->  let m = matched_string l in
               let npos = match_beginning () in
               let m_len = String.length m in
               let nl = global_replace (regexp m) "" l in
               let nacc = (Some (G.token_of_string (String.sub m 1 (m_len - 2)))) :: acc in
               line_token_extractor nacc npos nl

  let lines s =
    let end_line_markers s =
      let rec build start = match (search_forward endline_re s start) with
        | n -> n :: build (n + 1)
        | exception not_Found -> []
      in build 0 in
    let lines_list = snd @@ List.fold_left (fun (st, acc) ed -> (ed, String.sub s st (ed - st) :: acc)) (0, []) (end_line_markers s)
    in List.fold_left (fun acc x -> (line_token_extractor [] 0 x) :: acc ) [] lines_list

  (* Wrap the entire logged version in a callable run_trm function, and add a call to return run(code). *)
  (* Assumes entry point called run *)
  let ppf_run_wrap s =
    Format.sprintf "function run_trm(code) {@;<1 2>@[<v 1>@,%s@;<1 0>return run(code);@;}@]" s

  let ppf_call_wrap l s =
    Format.sprintf "@[(function () {@[<v 8>\
      log_custom({line:%d,type:\"enter\"});\
      var res = %s;\
      log_custom({line:%d,type:\"exit\"});\
      return res;}())@]@]" l s l

  (* Return position of first hit pair of brackets from pos *)
  let parse_brackets s pos =
    let slen = String.length s in
    let rec parse s pos acc =
      if pos = slen then slen else
      let c = String.get s pos in
      let npos = pos + 1 in
      match c with
        | '(' -> parse s npos ('(' :: acc)
        | ')' -> if List.length acc = 1 then pos else parse s npos (List.tl acc)
        | _   -> parse s npos acc
    in
    parse s pos []

  (* Take a line and pull out the first occurence of a function in a line *)
  let extract_function f s =
    match search_forward (regexp (f ^ "(")) s 0 with
      | exception Not_found -> (s, "" ,"")
      | _ ->  let pos = match_beginning () in
              let argend = 1 + parse_brackets s pos in
              (String.sub s 0 pos, String.sub s pos (argend - pos), String.sub s argend ((String.length s) - argend))

  let add_log_info s =
    let buf = Buffer.create 16 in
    let ls = lines s in
    (* i is line number of line preceding return *)
    let rec aux i = function
      | [] -> ()
      | (None :: tks, str) :: xs ->
          Buffer.add_string buf str;
          aux (i + 1) xs
      | ([], str)   :: xs ->
          Buffer.add_string buf str;
          aux (i + 1) xs
      | (Some l :: tks, str) :: xs ->
          let pad = 
            let len = String.length str in
            let rec repeat n x = if n = 0 then "" else x ^ repeat (n - 1) x in
            let rec aux i =
              if i < len then
                if str.[i] = ' ' then aux (i + 1)
                else i - 1
              else len 
            in repeat (aux 1) " " in
          match Hashtbl.find info_tbl l with
            | Add (id, typ)   -> 
                let ctx_processing id =
                   let rec aux = function
                     | [] -> ""
                     | x :: xs -> "\n" ^ pad ^ "ctx_push(ctx, \"" ^ x ^  "\", " ^ x ^ ", \"value\");" ^ aux xs
                   in id |> to_format |> Format.sprintf
                         |> global_replace (regexp "var ") "" |> split (regexp ", ") |> List.map (fun x -> List.hd (split (regexp " = ") x))
                         |> aux
                in Buffer.add_string buf @@ ctx_processing id ^ "\n" ^ pad ^ "log("^ string_of_int i ^" , ctx, " ^ typ ^ ");\n";
                   aux i ((tks, str) :: xs)
            | CreateCtx args -> 
                let argslist = split (regexp ", ") args in
                Buffer.add_string buf str;
                Buffer.add_string buf (pad ^ "\nvar ctx = ctx_empty();");
                (* Logging needs changing so we can use args actual name instead of t *)
                List.map (fun x -> Buffer.add_string buf ("\nctx_push(ctx, \"t\", " ^ x ^ ", \"expr\");") ) argslist;
                aux (i + 1) xs
            | ApplyInfix (f, e1, e2) -> aux i ((tks, str):: xs)   (* Skip infix logging*)
            | ApplyFunc  (f, args) ->   aux i ((tks, nstr) :: xs) (* Skip fun appl logging*)
    in aux 0 ls; Buffer.contents buf
                                 
  let logged_output s =
    let str_ppf = Format.str_formatter in
    Format.fprintf str_ppf (Scanf.format_from_string s "");
    ppf_run_wrap (add_log_info (Format.flush_str_formatter ()))
  (* let bad_output = Format.flush_str_formatter () in *)
  (* let pretty_output = global_replace lfs "\n" bad_output in *)
  (* add_log_info pretty_output *)

  let unlogged_output s =
    let str_ppf = Format.str_formatter in
    let unlogged_info = strip_log_info s in
    Format.fprintf str_ppf (to_format unlogged_info);
    Format.flush_str_formatter ()
  (* let bad_output = Format.flush_str_formatter () in *)
  (*     global_replace lfs "\n" bad_output *)
end
