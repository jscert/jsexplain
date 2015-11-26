open Params
open Format
open Mytools
open Parse_type

(*#########################################################################*)

let ppf = Format.std_formatter
let outputfile = ref None

(* err_formatter *)


(*#########################################################################*)

let _ =
   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs),
                      "includes a directory where to look for interface files");
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       ("-debug", Arg.Set debug, "trace the various steps");
       ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token")
     ]
     (fun f -> files := f :: !files)
     ("usage: [-I dir] [..other options..] file.ml");
   if List.length !files <> 1 then
      failwith "Expects one argument: the filename of the ML source file";
   let sourcefile = List.hd !files in
   if not (Filename.check_suffix sourcefile ".ml") then
     failwith "The file name must be of the form *.ml";
   let basename = Filename.chop_suffix (Filename.basename sourcefile) ".ml" in
   let dirname = Filename.dirname sourcefile in
   let log_output, unlog_output, token_output =
     match !outputfile with
     | None -> Filename.concat dirname (basename ^ ".log.js"),
               Filename.concat dirname (basename ^ ".unlog.js"),
               Filename.concat dirname (basename ^ ".token.js")
     | Some f -> f ^ ".log.js", f ^ ".unlog.js", f ^ ".token.js"
   in

   (*---------------------------------------------------*)
   (* "reading and typing source file" *)
   let (opt, _, modulename) = process_implementation_file ppf sourcefile in
   let ((parsetree1 : Parsetree.structure), typedtree1) =
      match opt with
      | None -> failwith "Could not read and typecheck input file"
      | Some (parsetree1, (typedtree1,_)) -> parsetree1, typedtree1
      in

      let out = Js_of_ast.to_javascript modulename typedtree1 in
      let output_filename = match !current_mode with
        | Mode_unlogged -> unlog_output
        | Mode_logged -> log_output
        | Mode_line_token -> token_output
      in
      file_put_contents output_filename out
