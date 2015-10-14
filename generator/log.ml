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
    | ReturnStrip
    | Enter
    | Exit

  val log_line : string -> ctx_operation list -> string
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
    | ReturnStrip
    | Enter
    | Exit
                 
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

  (* Links list of tokens to string, and returns token annotated string. Stores token info. *)
  let log_line str ctxls =
    let log_ctx str ctx =
      let token, tokenized = bind_token str in
      Hashtbl.replace info_tbl token ctx;
      tokenized in
    List.fold_left log_ctx str ctxls

  (* Removes all tokens from string *)
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

  (* Splits string by new lines and searches for all tokens in that line. Return list of tuples: [([<token_list>], <line>)] *)
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
                     | x :: xs -> "\n" ^ pad ^ "ctx = ctx_push(ctx, \"" ^ x ^  "\", " ^ x ^ ", \"value\");" ^ aux xs
                   in id |> to_format |> Format.sprintf
                         |> global_replace (regexp "var ") "" |> split (regexp ", ") |> List.map (fun x -> List.hd (split (regexp " = ") x))
                         |> aux
                in Buffer.add_string buf @@ ctx_processing id ^ "\n" ^ pad ^ "log("^ string_of_int i ^" , ctx, " ^ typ ^ ");";
                   aux i ((tks, str) :: xs)
            | CreateCtx args ->
                (* Creates new context and logs arguments. *)
                let argslist = split (regexp ", ") args in
                Buffer.add_string buf str;
                Buffer.add_string buf ("\n" ^ pad ^ "var ctx = ctx_empty();");
                (* Logging needs changing so we can use args actual name instead of t *)
                List.map (fun x -> Buffer.add_string buf ("\n" ^ pad ^ "ctx = ctx_push(ctx, \"" ^ x ^ "\", " ^ x ^ ", \"term\");") ) argslist;
                (* Find way to trickle actual function name in log call? *)
                Buffer.add_string buf ("\n" ^ pad ^ "log(" ^ string_of_int (i + 1)  ^ ", ctx, \"function\");");
                aux i ((tks, str) :: xs)
            | ReturnStrip ->
                (* Pull whatever is in a return call out to var returnres and modify the stored string to return returnres *)
                let strsplit = split (regexp "return") str in
                if List.length strsplit > 1 then
                  let nstr = (List.nth strsplit 0) ^ "return returnres;" in
                  Buffer.add_string buf ((List.nth strsplit 0) ^ "var returnres =" ^ (List.nth strsplit 1));
                  aux i ((tks, nstr) :: xs)
                else
                  aux i ((tks, str) :: xs)
            | Enter -> 
                (* Log an entry into a function *)
                Buffer.add_string buf ("\n" ^ pad ^ "log_custom({line:" ^ string_of_int (i + 1) ^ ", type: \"enter\"});");
                aux (i+1) xs
            | Exit ->
                (* Log exit of a function *)
                Buffer.add_string buf ("\n" ^ pad ^ "log_custom({line:" ^ string_of_int (i + 1) ^ ", type: \"exit\"});");
                aux i ((tks, str) :: xs)
    in aux 0 ls; Buffer.contents buf
                                 
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
    Format.fprintf str_ppf (to_format unlogged_info);
    Format.flush_str_formatter ()
  (* let bad_output = Format.flush_str_formatter () in *)
  (*     global_replace lfs "\n" bad_output *)
end
