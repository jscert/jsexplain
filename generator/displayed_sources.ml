
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


end

(* Extra *)

let hashtbl_keys t =
  Hashtbl.fold (fun key value acc -> key::acc) t []


(*#########################################################################*)


(** DOCUMENTATION
  
    takes as argument a list of javascript filenames,
    and create a javascript file with a definition of
    an array called "tracer_files", storing objects with
    two fields: a filename, and a contents, with newline
    and quotes properly escaped.


   var tracer_files = [
      { 
         file: 'calc.js', 
         contents: 'var Stack = {\n  is_empty: function (s) {\n    return s === {type: "N"};\n  },\n\n  push: function (x, stack) {\n    return {type: "C", value: x, stack: stack};\n  },\n\n ;\n};\n'
      }
   ];

 *)





let files = ref ([]:string list)
let outputfile = ref None
(*let stdlibfile = ref None*)

(* TODO: might be useful to take "basename" from the command line *)

let _ =
   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ (* ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs),
                      "includes a directory where to look for interface files"); *)
       (* ("-stdlib", Arg.String (fun s -> stdlibfile := Some s), "set the stdlib file name"); *)
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       (* ("-debug", Arg.Set debug, "trace the various steps"); *)
       (* ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token")*)
     ]
     (fun f -> files := f :: !files)
     ("usage: [..other options..] -o sources.js file1.js file2.js ...");
     (* -stdlib file.js *)
   if !files = [] then
     failwith "No input file provided";
   files := List.rev !files;
   let input_filename1 = List.hd !files in
   let dirname = Filename.dirname input_filename1 in
   let output_filename = 
     match !outputfile with
     | None -> Filename.concat dirname "displayed_sources.js"
     | Some f -> f
   in

   (*---------------------------------------------------*)
   (* open output file for writing *)

    let outchannel = open_out output_filename in
    let put_no_endline str =
       output_string outchannel str in
    let put str =
       output_string outchannel str;
       output_string outchannel "\n" in


   (*---------------------------------------------------*)
   (* include of logged js files *)

   put "var tracer_files = [";

   ~~ List.iter !files (fun filename ->
      let basename = Filename.chop_suffix (Filename.basename filename) ".unlog.js" in
      let showed_filename = basename ^ ".js" in
      put (Printf.sprintf "\n/* --------------------- %s --------------------- */" showed_filename);
      put_no_endline (Printf.sprintf "  { file: '%s', contents: '" showed_filename);
      let lines = XFile.get_lines filename in
      ~~ List.iter lines (fun line ->
         let line = Str.global_replace (Str.regexp "'") "\\'" line in
         put_no_endline line;
         put_no_endline "\\n";
      );
      put "'},";
      );

   put "];"; 

   (*---------------------------------------------------*)
   (* generating output file *)

   close_out outchannel;
   Printf.printf "Wrote file: %s\n" output_filename;


