
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


let files = ref ([]:string list)
let outputfile = ref None
let stdlibfile = ref None

(* TODO: might be useful to take "basename" from the command line *)

let _ =
   (*---------------------------------------------------*)
   (* parsing of command line *)

   let files = ref [] in
   Arg.parse
     [ (* ("-I", Arg.String (fun i -> Clflags.include_dirs := i :: !Clflags.include_dirs),
                      "includes a directory where to look for interface files"); *)
       ("-stdlib", Arg.String (fun s -> stdlibfile := Some s), "set the stdlib file name");
       ("-o", Arg.String (fun s -> outputfile := Some s), "set the output file name");
       (* ("-debug", Arg.Set debug, "trace the various steps"); *)
       (* ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token")*)
     ]
     (fun f -> files := f :: !files)
     ("usage: [..other options..] -o lineof.js -stdlib file.js file1 file2 ..; \n assuming fileN.log.js and fileN.unlog.js exist.");
   if !files = [] then
     failwith "No input file provided";
   let input_filename1 = List.hd !files in
   let dirname = Filename.dirname input_filename1 in
   let output_filename = 
     match !outputfile with
     | None -> Filename.concat dirname "assembly.js"
     | Some f -> f
   in

   (*---------------------------------------------------*)
   (* open output file for writing *)

    let outchannel = open_out output_filename in
    let put str =
       output_string outchannel str;
       output_string outchannel "\n" in
    let puts lines =
       List.iter put lines in

   (*---------------------------------------------------*)
   (* include of the source code of logged unsource files *)

   (* TODO *)

   (*---------------------------------------------------*)
   (* include of stdlib source *)

   begin match !stdlibfile with
   | None -> ()
   | Some filename ->
      let lines = XFile.get_lines filename in
      put "\n/* --------------------- stdlib --------------------- */\n";
      puts lines;
   end;

   (*---------------------------------------------------*)
   (* include of logged js files *)

   ~~ List.iter !files (fun filename_noext ->
      let filename = filename_noext ^ ".log.js" in
      let lines = XFile.get_lines filename in
      put (Printf.sprintf "\n/* --------------------- %s --------------------- */\n" filename);
      puts lines;
      );

   (*---------------------------------------------------*)
   (* generating output file *)

   close_out outchannel;
   Printf.printf "Wrote file: %s\n" output_filename;


