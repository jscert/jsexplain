
open Params
open Format
open Mytools

(*
   Remark: field name attributes for builtins (eg: ::) are defined in attributes.ml
   Remark: field name attributes should not be "type" or "tag".
*)


(*#########################################################################*)

let ppf = Format.std_formatter
let outputfile = ref None

(* err_formatter *)


(*#########################################################################*)

let add_to_list li s =
  li := s :: !li


let tool_name = "ml2js"

let init_path () =
  Config.load_path :=
    "stdlib_ml" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
  Env.reset_cache ()

(** Return the initial environment in which compilation proceeds. *)

let initial_env () =
  try
    let env = Env.initial_unsafe_string in
    Env.open_pers_signature "Stdlib" env
  with Not_found ->
    Misc.fatal_error "cannot open stdlib"

(** Analysis of an implementation file. Returns (Some typedtree) if
   no error occured, else None and an error message is printed.*)
let process_implementation_file ppf sourcefile =
  init_path ();
  let oprefix = Compenv.output_prefix sourcefile in
  let modulename = Compenv.module_of_filename ppf sourcefile oprefix in
  Env.set_unit_name modulename;
  let env = initial_env () in
  try
    let parsetree = Pparse.parse_implementation ~tool_name ppf sourcefile in
    if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.structure parsetree;
    let typing = Typemod.type_implementation sourcefile oprefix modulename env parsetree in
    (parsetree, typing, modulename)
  with e ->
    Location.report_exception ppf e;
    exit 2




let _ =
   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs),
                      "includes a directory where to look for interface files");
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       ("-debug", Arg.Set debug, "trace the various steps");
       ("-dsource", Arg.Set Clflags.dump_source, "dump source after ppx");
       ("-ppx", Arg.String (add_to_list Clflags.all_ppx (* TODO Compenv.first_ppx *) ), "load ppx");
       ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token")
     ]
     (fun f -> files := f :: !files)
     ("usage: [-I dir] [..other options..] file.ml");


   files := List.rev !files;
   if List.length !files <> 1 then
      failwith "Expects one argument: the filename of the ML source file";
   let sourcefile = List.hd !files in
   if not (Filename.check_suffix sourcefile ".ml") then
     failwith "The file name must be of the form *.ml";
   let basename = Filename.chop_suffix (Filename.basename sourcefile) ".ml" in
   let dirname = Filename.dirname sourcefile in
   let pathname = if dirname = "" then basename else (dirname ^ "/" ^ basename) in
   (* Could use Clflags.output_name and Compenv.output_prefix? *)
   let log_output, unlog_output, token_output, pseudo_output, ptoken_output, mlloc_output =
     match !outputfile with
     | None -> Filename.concat dirname (basename ^ ".log.js"),
               Filename.concat dirname (basename ^ ".unlog.js"),
               Filename.concat dirname (basename ^ ".token.js"),
               Filename.concat dirname (basename ^ ".pseudo.js"),
               Filename.concat dirname (basename ^ ".ptoken.js"),
               Filename.concat dirname (basename ^ ".mlloc.js")
     | Some f -> f ^ ".log.js", f ^ ".unlog.js", f ^ ".token.js", f ^ ".pseudo.js", f ^ ".ptoken.js", f ^ ".mlloc.js"
   in

   (*---------------------------------------------------*)
   (* set flags *)

   if !current_mode <> Mode_cmi
      then Clflags.dont_write_files := true;

   (* TODO: does not work because we don't have easy access to the fully qualified path of constructors
   if !current_mode <> Mode_unlogged 
      then generate_qualified_names := true;
   *)


   (*---------------------------------------------------*)
   (* generation of the mlloc file that binds tokens to positions *)

   let generate_mlloc_file () =
      let outchannel = open_out mlloc_output in
      let put str =
         output_string outchannel str;
         output_string outchannel "\n" in
      put "   lineof_temp = [];";
      let filename = basename ^ ".ml" in
      Js_of_ast.(
         ~~ Hashtbl.iter token_locs (fun key (pos_start,pos_stop) ->
           put (Printf.sprintf "   lineof_temp[%d] = [%d,%d,%d,%d];" 
                  key pos_start.pos_line pos_start.pos_col  
                      pos_stop.pos_line  pos_stop.pos_col);
         ));
      put (Printf.sprintf "lineof_data[\"%s\"] = lineof_temp;" filename);
      close_out outchannel;
      in

   (*---------------------------------------------------*)
   (* "reading and typing source file" *)

   let (parsetree, (typedtree,_), module_name) = process_implementation_file ppf sourcefile in
   match !current_mode with
     | Mode_cmi -> Printf.printf "Wrote %s.cmi\n" pathname
     | _ ->
       let out = Js_of_ast.to_javascript basename module_name typedtree in
       let output_filename = match !current_mode with
         | Mode_unlogged TokenTrue -> token_output
         | Mode_unlogged TokenFalse -> unlog_output
         | Mode_pseudo TokenTrue -> ptoken_output
         | Mode_pseudo TokenFalse -> pseudo_output
         | Mode_logged -> log_output
         | _ -> assert false
       in
       file_put_contents output_filename out;
       Printf.printf "Wrote %s\n" output_filename;
       if !current_mode = (Mode_unlogged TokenTrue) 
         then generate_mlloc_file()


