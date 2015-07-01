module Token_generator :
sig
  type token
  
  val reset : unit -> unit
  val withdraw : unit -> token
end
  =
struct
  type token = int

  let tok = ref 0
  let reset () = tok := 0
  let withdraw () = tok := !tok + 1; !tok
end

module Logged
  (G  : module type of Token_generator)
  (Sz : sig val size : int end)
sig
  type token
  type token_info

  type ctx_operation =
  | Add of ident
  | Redef of ident
  | Del of Ident
      
  val token_delim : char  
  val free_token : unit -> token
  val token_info : token -> token_info
  val token_from_line : string -> token option
  val update_token_info : token -> ctx_operation-> token_info
end
  =
struct
  type token = G.token
  type ident = string

  type token_info = ctx_operation option  

  let info_tbl = Hashtbl.create Sz.sz
  let token_delim = "%"

    
  let free_token = G.withdraw
    
  let token_info = Hashtbl.find info_tbl
    
  let token_for_line l =
    let len = String.lenght l in
    let extract i acc = match l.[i] with
      | '%' -> Some acc
      | '0'..'9' -> extract (i - 1) (int_of_char l.[i] * 10 + acc)
      | _ -> None
    in
    if l.[len - 1] = '%' then extract (len - 2) 0
    else None

  let udpate_token_info tok op = Hashtbl.replace tok (Some op)
end
    
module Logged_printer :
  (L : module type of Logger)
sig
  
end
  =
struct
  let initial_format_functions = get_formatter_out_functions ()
  let custom_out_newline () =
    let tok = L.token_delim ^  (string_of_int (L.free_token ())) ^ L.token_delim ^ "\n" in
    let len = String.length tok
    in initial_format_functions.out_string tok 0 len
     
  let custom_format_functions = {initial_format_functions with out_newline = custom_out_newline}
  in set_formatter_out_functions custom_format_functions  
    
end
