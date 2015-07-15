open Format
open Mytools
open Parse_type

(*#########################################################################*)

let debug = ref false
let ppf = Format.std_formatter
let outputfile = ref None

(* err_formatter *)


(*#########################################################################*)

let _ =
  
   (* disable loading of stdlib *)
   Clflags.nopervasives := false;

   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs), 
                      "includes a directory where to look for interface files");
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       ("-debug", Arg.Set debug, "trace the various steps")
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
   let outputfile : string =
     match !outputfile with
     | None -> Filename.concat dirname (basename ^ ".js")
     | Some f -> f
   in

   Printf.printf "sf: %s\n" sourcefile;

   (*---------------------------------------------------*)
   (* "reading and typing source file" *)
   let (opt, inputfile) = process_implementation_file ppf sourcefile in
   let ((parsetree1 : Parsetree.structure), typedtree1) =
      match opt with
      | None -> failwith "Could not read and typecheck input file"
      | Some (parsetree1, (typedtree1,_)) -> parsetree1, typedtree1
      in

      let (logged, unlogged, pre) = Js_of_ast.to_javascript typedtree1 in
      file_put_contents outputfile unlogged
