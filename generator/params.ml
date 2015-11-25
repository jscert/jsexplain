let debug = ref false
let logging = ref false

(****************************************************************)
(* MODES *)

type generate_mode = 
  | Mode_unlogged
  | Mode_line_token
  | Mode_logged

let current_mode = ref Mode_unlogged
