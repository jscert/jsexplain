open Format
open Str

let ppf = str_formatter
let re = regexp "\n\ *\n"
  
let record ppf name names exps =
  let rec aux ppf (names, exp) = match (names, exp) with
    | [], [] -> () 
    | x :: xs, y :: ys -> fprintf ppf "%s: %s,@,%a" x y aux (xs, ys)      
    | _ -> invalid_arg ""
  in fprintf ppf "@[<v 0>var %s = {@,@[<v 4>@,%a@]@,}@]" name aux (names, exps)

let names = ["a" ; "b" ; "c" ]
let exps  = ["1" ; "2" ; "3" ]

let rm_lf2 s =
  global_replace re "\n" s

    
let f () =
  let ippf = get_formatter_out_functions () in
  record ppf "bli" names exps ;
  rm_lf2 (flush_str_formatter ())
  
