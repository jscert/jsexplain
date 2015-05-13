open Format
open Mytools

(*#########################################################################*)

let debug = ref false

let myflag = ref false

let ppf = Format.std_formatter

let outputfile = ref None

(* err_formatter *)


(*#########################################################################*)

let _ =

   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse  
     [ ("-I", Arg.String (fun i -> Clflags.include_dirs := i::!Clflags.include_dirs), 
                      "includes a directory where to look for interface files");
       ("-myflag", Arg.Set myflag, "example of a flag"); 
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       ("-debug", Arg.Set debug, "trace the various steps") ]
     (fun f -> files := f::!files)
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
     | None -> Filename.concat dirname ((String.capitalize basename) ^ ".js")
     | Some f -> f
   in
  
   (*---------------------------------------------------*)
   (* "reading and typing source file" *)
   let (opt,inputfile) = process_implementation_file ppf sourcefile in
   let parsetree1 : Parsetree.structure =
      match opt with
      | None -> failwith "Could not read and typecheck input file"
      | Some (parsetree1, (typedtree1,_)) -> parsetree1
      in
   
   file_put_contents outputfile (Print_tast.string_of_structure typedtree1) 

   (* file_put_contents ("_parsed_file.ml") (Print_past.string_of_structure parsetree1);  *)

   (*---------------------------------------------------*)
   (* typing normalized code
   let (typedtree2, _ : Typedtree.structure * Typedtree.module_coercion) = 
      match typecheck_implementation_file ppf sourcefile parsetree2 with
      | None -> failwith "Could not typecheck the normalized source code\nCheck out the file output/_normalized.ml." 
      | Some typedtree2 -> typedtree2
      in
   file_put_contents (debugdirBase ^ "_normalized_typed.ml") (Print_tast.string_of_structure typedtree2); 
   ignore (typedtree2);
   *)
