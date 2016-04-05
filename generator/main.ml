
open Params
open Format
open Mytools
open Parse_type

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


let _ =
   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs),
                      "includes a directory where to look for interface files");
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       ("-debug", Arg.Set debug, "trace the various steps");
       ("-ppx", Arg.String (add_to_list Clflags.all_ppx (* TODO Compenv.first_ppx *) ), "load ppx");
       ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token")
     ]
     (fun f -> files := f :: !files)
     ("usage: [-I dir] [..other options..] file.ml");

   (* force: -dsource *)
   Clflags.dump_source := true;


   files := List.rev !files;
   if List.length !files <> 1 then
      failwith "Expects one argument: the filename of the ML source file";
   let sourcefile = List.hd !files in
   if not (Filename.check_suffix sourcefile ".ml") then
     failwith "The file name must be of the form *.ml";
   let basename = Filename.chop_suffix (Filename.basename sourcefile) ".ml" in
   let dirname = Filename.dirname sourcefile in
   let pathname = if dirname = "" then basename else (dirname ^ "/" ^ basename) in
   let log_output, unlog_output, token_output, mlloc_output =
     match !outputfile with
     | None -> Filename.concat dirname (basename ^ ".log.js"),
               Filename.concat dirname (basename ^ ".unlog.js"),
               Filename.concat dirname (basename ^ ".token.js"),
               Filename.concat dirname (basename ^ ".mlloc.js")
     | Some f -> f ^ ".log.js", f ^ ".unlog.js", f ^ ".token.js", f ^ ".mlloc.js"
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

   let (opt, _, module_name) = process_implementation_file ppf sourcefile in
   let ((parsetree1 : Parsetree.structure), typedtree1) =
      match opt with
      | None -> failwith "Could not read and typecheck input file"
      | Some (parsetree1, (typedtree1,_)) -> parsetree1, typedtree1
      in

      match !current_mode with
        | Mode_cmi -> Printf.printf "Wrote %s.cmi\n" pathname
        | _ ->
          let out = Js_of_ast.to_javascript basename module_name typedtree1 in
          let output_filename = match !current_mode with
            | Mode_unlogged -> unlog_output
            | Mode_logged -> log_output
            | Mode_line_token -> token_output
            | _ -> assert false
          in
          file_put_contents output_filename out;
          Printf.printf "Wrote %s\n" output_filename;
          if !current_mode = Mode_line_token 
            then generate_mlloc_file()


