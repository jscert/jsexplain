open Config
open Clflags
open Misc
open Format
open Typedtree

(** The purpose of this file is to parse an interface file.
    The content of this file is a copy-pasting from the source
    code of OCamldoc. *)

(*#########################################################################*)
(* ** Parsing of MLI files *)

(** Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)

let init_path () =
  load_path :=
    "stdlib_ml" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
  Env.reset_cache ()

(** Return the initial environment in which compilation proceeds. *)

let initial_env () =
  try
    if !Clflags.nopervasives
    then Env.open_pers_signature "Stdlib" Env.initial_unsafe_string
    else Env.open_pers_signature "Stdlib" Env.initial_unsafe_string
  with Not_found ->
    fatal_error "cannot open stdlib"

(** Optionally preprocess a source file *)
let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
      if Ccomp.command comm <> 0 then begin
        remove_file tmpfile;
        Printf.eprintf "Preprocessing error\n";
        exit 2
      end;
      tmpfile

(** Remove the input file if this file was the result of a preprocessing.*)
let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> remove_file inputfile

let remove_preprocessed_if_ast inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> if inputfile <> !Location.input_name then remove_file inputfile

exception Outdated_version

(** Parse a file or get a dumped syntax tree in it *)
let parse_file inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast


(** Analysis of an implementation file. Returns (Some typedtree) if
   no error occured, else None and an error message is printed.*)
let process_implementation_file ppf sourcefile =

  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  let inputfile = preprocess sourcefile in
  try
  let env = initial_env () in

    let parsetree = parse_file inputfile Parse.implementation ast_impl_magic_number in
    let typedtree = Typemod.type_implementation sourcefile prefixname modulename env parsetree in
    (Some (parsetree, typedtree), inputfile)
  with
    e ->
      match e with
        Syntaxerr.Error err ->
          fprintf Format.err_formatter "@[%a@]@."
            Syntaxerr.report_error err;
          None, inputfile
      | Failure s ->
          prerr_endline s;
          (*incr Odoc_global.errors ;*)
          None, inputfile
      (* ADDED *)
      | Env.Error err -> 
          Env.report_error ppf err;
          print_newline();
          raise e
      | Typecore.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typecore.report_error env ppf err;
          print_newline();
          raise e
      | Typetexp.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typetexp.report_error env ppf err;
          print_newline();
          raise e
      | Typemod.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typemod.report_error env ppf err;
          print_newline();
          raise e
      | e ->
          raise e

(** Analysis of an interface file. Returns (Some signature) if
   no error occured, else None and an error message is printed.
let process_interface_file ppf sourcefile =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  let inputfile = preprocess sourcefile in
  let ast = parse_file inputfile Parse.interface ast_intf_magic_number in
  let sg = Typemod.transl_signature (initial_env()) ast in
  Warnings.check_fatal ();
  (ast, sg, inputfile)
*)

(*#########################################################################*)
(* added *)

let typecheck_implementation_file ppf sourcefile parsetree =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  (* let inputfile = preprocess sourcefile in*)
  let env = initial_env () in
  try
    (* let parsetree = parse_file inputfile Parse.implementation ast_impl_magic_number in *)
    let typedtree = Typemod.type_implementation sourcefile prefixname modulename env parsetree in
    Some typedtree
  with
    e -> (* todo: factorize with above *)
      match e with
        Syntaxerr.Error err ->
          fprintf Format.err_formatter "@[%a@]@."
            Syntaxerr.report_error err;
          None
      | Failure s ->
          prerr_endline s;
          (*incr Odoc_global.errors ;*)
          None
      | Env.Error err -> 
          Env.report_error ppf err;
          print_newline();
          raise e
      | Typetexp.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typetexp.report_error env ppf err;
          print_newline();
          raise e
      | Typecore.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typecore.report_error env ppf err; 
          print_newline();
          raise e
      | Typemod.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typemod.report_error env ppf err;
          print_newline();
          raise e
      | e ->
          raise e


let typecheck_interface_file ppf sourcefile output_prefix =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  let inputfile = preprocess sourcefile in
  let ast = parse_file inputfile Parse.interface ast_intf_magic_number in
  let sg_opt = try
    let sg = Typemod.transl_signature (initial_env()) ast in
    Warnings.check_fatal ();
    Some sg
  with
    e -> (* todo: factorize with above *)
      match e with
        Syntaxerr.Error err ->
          fprintf Format.err_formatter "@[%a@]@."
            Syntaxerr.report_error err;
          None
      | Failure s ->
          prerr_endline s;
          (*incr Odoc_global.errors ;*)
          None
      | Env.Error err -> 
          Env.report_error ppf err;
          print_newline();
          raise e
      | Typetexp.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typetexp.report_error env ppf err;
          print_newline();
          raise e
      | Typecore.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typecore.report_error env ppf err; 
          print_newline();
          raise e
      | Typemod.Error (loc,env,err) -> 
          Location.print_error ppf loc;
          Typemod.report_error env ppf err;
          print_newline();
          raise e
      | e ->
          raise e
     in
  match sg_opt with
  | None -> failwith "could not typecheck"
  | Some sg -> Env.save_signature sg.sig_type modulename (output_prefix ^ ".cmi")

