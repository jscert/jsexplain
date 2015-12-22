
(*#########################################################################*)

(* Section COPIED FROM /home/charguer/pbench/xlib/XBase.ml *)

(** A generic operator for swapping the order of the two first arguments 
    of a function *)

let ( ~~ ) = fun f x y -> f y x 

module XBase = struct
  exception Break
end

(* Section COPIED FROM /home/charguer/pbench/xlib/XList.ml *)

module XList = struct

  let rev_not_rec l =
     let res = ref [] in
     let cur = ref l in
     begin try while true do
        match !cur with
        | [] -> raise XBase.Break
        | x::xs ->
           res := x::!res;
           cur := xs
     done with XBase.Break -> () end;
     !res
end


(* Section COPIED FROM /home/charguer/pbench/xlib/XFile.ml *)

module XFile = struct

  (** Write the string [str] into a file of given name *)

  let put_contents filename str =
    let channel = open_out filename in
    output_string channel str;
    close_out channel    

  (** Write a list of lines into a file of given name *)

  let put_lines filename ?(sep="\n") lines =
     put_contents filename (String.concat sep (lines @ [""]))

  (** Read the lines of a file; raise FileNotFound if no such file *)

  exception FileNotFound of string

  let get_lines file = 
     if not (Sys.file_exists file)
        then raise (FileNotFound file);
     let lines = ref [] in
     let f = 
        try open_in file with End_of_file -> raise (FileNotFound file);
        in
     begin try while true do
        lines := input_line f :: !lines 
     done with End_of_file -> () end;
     close_in f;
     XList.rev_not_rec !lines

  (** Read the content of a file as a list of lines;
      returns an empty list if no such file exists *)

  let get_lines_or_empty file =
     try get_lines file
     with FileNotFound _ -> []

  (** Read the content of a file as a string, terminated with a newline;
      raise FileNotFound if no such file exists *)

  let get_contents file =
     let lines = get_lines file in
     (String.concat "\n" lines) ^ "\n"

end

(* Extra *)

let hashtbl_keys t =
  Hashtbl.fold (fun key value acc -> key::acc) t []


(*#########################################################################*)

(* Generate a JS function of the following form:

function lineof(filename, token) {
  switch (filename) {
    case "foo.js": 
      switch (token) {
        case 2: return {start: {line: 12, col: 9}, stop: {line: 13, col: 2}};
        case 19: return {start: {line: 15, col: 9}, stop: {line: 14, col: 5}};
        default: throw "lineof does not know token " + token + " in file: " + filename
      }
      break;
    case "bar.js": 
      ...
    default:
      throw "lineof does not know file: " + filename
  }
}

*)

type pos = { pos_line: int; pos_col: int }
type tokens_start = (int, pos) Hashtbl.t
type tokens_stop = (int, pos) Hashtbl.t
type tokens = (string * tokens_start * tokens_stop) list ref

let tokens : tokens = ref []

let gather_tokens basename input =
  let tokens_start = Hashtbl.create 50 in
  let tokens_stop = Hashtbl.create 50 in
  (* start tokens *) 
  begin
    let r = Str.regexp "#<\\([0-9]*\\)#" in 
    let i = ref 0 in
    let mk_pos () = { pos_line = !i; pos_col = 0 } in
    begin try
      while true do 
        (* Printf.printf "search from %d\n" !i; *)
        let j = Str.search_forward r input !i in
        i := j+1;
        let key = Str.matched_group 1 input in
        let pos = mk_pos() in
        (* Printf.printf "matched key: %s\n" key; *)
        Hashtbl.add tokens_start (int_of_string key) pos
      done;
    with
      | Not_found -> () end;
  end;
  (* end tokens *) 
  begin
    let r = Str.regexp "#\\([0-9]*\\)>#" in 
    let i = ref 0 in
    let mk_pos () = { pos_line = !i; pos_col = 0 } in
    begin try
      while true do 
        (* Printf.printf "search from %d\n" !i; *)
        let j = Str.search_forward r input !i in
        i := j+1;
        let key = Str.matched_group 1 input in
        let pos = mk_pos() in
        (* Printf.printf "matched key: %s\n" key;   *)
        Hashtbl.add tokens_stop (int_of_string key) pos
      done;
    with
      | Not_found -> () end;
  end;
  (* final *)
  tokens := (basename, tokens_start, tokens_stop)::!tokens


let generate_lineof_function output_file : string =
  let aux_pos pos =
    Printf.sprintf "{ line: %d, col: %d }" pos.pos_line pos.pos_col 
    in
  let aux_key key pos_start pos_stop =
    Printf.sprintf "case %d: return {start: %s, stop: %s};" key (aux_pos pos_start) (aux_pos pos_stop)
    in
  let aux_file (basename, tokens_start, tokens_stop) =
    let filename = basename ^ "js" in
    let keys = hashtbl_keys tokens_start in
    let skeycases = String.concat "@," (~~ List.map keys (fun key -> 
     let pos_start = try Hashtbl.find tokens_start key
        with Not_found -> assert false (* searching for a key that is there *)
        in
     let pos_stop = try Hashtbl.find tokens_stop key
        with Not_found -> Printf.printf "Warning (error): unclosed token %d in file %s; using pos_start instead.\n" key filename; pos_start
        in 
      aux_key key pos_start pos_stop)) in
    let skeyerr = "@,  default: throw \"lineof does not know token \" + token + \" in file: \" + filename;" in
    Printf.sprintf "case \"%s\": switch (token) { @, @;<1 2>@[<v 0>%s@,%s@]@, }@, break;" 
      filename skeycases skeyerr
    in
  let sfilecases = String.concat "@," (List.map aux_file !tokens) in
  let sfileerr = "throw \"lineof does not know file: \" + filename;" in
  let sfiles = Printf.sprintf "switch (filename) { @;<1 2>@[<v 0>%s@]@,default: %s@,}@,"
    sfilecases sfileerr in
  let sfull = Printf.sprintf "function lineof(filename, token) {@;<1 2>@[<v 0>%s@]@,}@," sfiles in
  (* TODO: use an auxiliary function for the next 3 lines of code *)
  let str_ppf = Format.str_formatter in
  Format.fprintf str_ppf (Scanf.format_from_string sfull "");
  Format.flush_str_formatter ()


(*#########################################################################*)



let files = ref ([]:string list)
let outputfile = ref None

(* TODO: might be useful to take "basename" from the command line *)

let _ =
   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ (* ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs),
                      "includes a directory where to look for interface files"); *)
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       (* ("-debug", Arg.Set debug, "trace the various steps"); *)
       (* ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token")*)
     ]
     (fun f -> files := f :: !files)
     ("usage: [..other options..] -o lineof.js file1.token.js file2.token.js ..");
   if !files = [] then
     failwith "No input file provided";
   let input_filename1 = List.hd !files in
   let dirname = Filename.dirname input_filename1 in
   let output_filename = 
     match !outputfile with
     | None -> Filename.concat dirname "lineof.js"
     | Some f -> f
   in

   (*---------------------------------------------------*)
   (* processing source files *)

   ~~ List.iter !files (fun filename ->
     if not (Filename.check_suffix filename ".token.js") then
       failwith "Input file must be of the form *.token.js";
     let basename = Filename.chop_suffix (Filename.basename filename) ".token.js" in
     let input = XFile.get_contents filename in
     gather_tokens basename input
   );

   (*---------------------------------------------------*)
   (* generating output file *)

   let output = generate_lineof_function () in
   XFile.put_contents output_filename output;
   Printf.printf "Wrote file: %s\n" output_filename;


