(** esdocgen : Parse @es* ECMAScript crossreference ocamldoc tags *)

open List

let tostr = Odoc_info.string_of_text

let assoc_some a l =
  try  Some (assoc a l)
  with Not_found -> None

let assoc_def_str a l b =
  try  tostr (assoc a l)
  with Not_found -> b

let es_spec_uri = ref ""

let set_es_spec l =
  match (assoc_some "esurl" l) with
  | Some uri ->
    (match (assoc_some "esversion" l) with
    | Some version -> es_spec_uri := (tostr uri) ^ (tostr version) ^ "/"
    | None         -> es_spec_uri := (tostr uri))
  | None -> ()

let html_of_esid l =
  let esid = tostr (assoc "esid" l) in
  let essec = assoc_def_str "essec" l "link" in
  let str = "ECMAScript Specification section: <a href=\"" ^ !es_spec_uri ^ "#" ^ esid ^ "\">" ^ essec ^ "</a>" in
  (* print_endline str; *)
  str

module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
    object(self)
      inherit G.html

      method html_of_custom b l =
        if mem_assoc "esurl" l then set_es_spec l;
        if mem_assoc "esid" l then Buffer.add_string b (html_of_esid l)
  end
end
let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor);;
