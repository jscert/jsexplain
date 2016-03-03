let debug = ref false

let (~~) f x y = f y x


(****************************************************************)
(* MODES *)

type generate_mode = 
  | Mode_unlogged
  | Mode_line_token
  | Mode_logged
  | Mode_cmi

let current_mode = ref Mode_unlogged

let set_current_mode s =
  current_mode := match s with
    | "cmi" -> Mode_cmi
    | "log" -> Mode_logged
    | "unlog" -> Mode_unlogged
    | "token" -> Mode_line_token
    | _ -> failwith "Invalid mode, chose log, unlog, or token"

let generate_qualified_names = ref false
