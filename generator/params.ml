let debug = ref false

let (~~) f x y = f y x

let add_to_list li s =
  li := s :: !li

(****************************************************************)
(* MODES *)

type generate_token_flag = 
  | TokenTrue
  | TokenFalse

type generate_mode = 
  | Mode_pseudo of generate_token_flag
  | Mode_unlogged of generate_token_flag
  | Mode_logged
  | Mode_cmi

let current_mode = ref (Mode_unlogged TokenFalse)

let set_current_mode s =
  current_mode := match s with
    | "cmi" -> Mode_cmi
    | "log" -> Mode_logged
    | "unlog" -> Mode_unlogged TokenFalse
    | "token" -> Mode_unlogged TokenTrue
    | "pseudo" -> Mode_pseudo TokenFalse
    | "ptoken" -> Mode_pseudo TokenTrue
    | _ -> failwith "Invalid mode, chose log, unlog, or token"

let get_mode_extension m = match m with
  | Mode_unlogged TokenTrue -> ".token.js"
  | Mode_unlogged TokenFalse -> ".unlog.js"
  | Mode_pseudo TokenTrue -> ".ptoken.js"
  | Mode_pseudo TokenFalse -> ".pseudo.js"
  | Mode_logged -> ".log.js"
  | Mode_cmi -> ".cmi"

let is_mode_pseudo () = 
  (match !current_mode with Mode_pseudo _ -> true | _ -> false)

let is_mode_not_pseudo () = 
  not (is_mode_pseudo())

let generate_qualified_names = ref false
