let output = ref "../gen/"

type t =
  | Site of string
  | Project of {
    page: t';
    version: Version.t;
    project: string;
  }
and t' =
  | Template
  | Page of string
  | Manual of string
  | Api of {
    subproject: string;
    file: string;
  }

let to_string ?(src=false) = function
  | Site s -> s (* FIXME this won't always work *)
  | Project {page = Template; project; _} -> project ^ "/template"
  | Project {page; version = v; project} ->
    let p =
      match page with
      | Page p -> p
      | Manual m -> "manual/" ^ (if src then "src/" else "") ^ m
      | Api {subproject; file} ->
        "api/" ^ (if subproject = "" then "" else subproject ^ "/") ^ file
      | Template -> assert false (* handled above... *)
    in
    project ^ "/" ^ (v |> Version.to_string) ^ "/" ^ p

let to_source = function
  | Project {page = Page _; _} -> None
  | d -> Some (to_string ~src:true d ^ ".wiki")

let to_output d = !output ^ to_string d ^ ".html"

let to_uri ?(ext=".html") ?fragment x =
  "/" ^ to_string x ^ ext ^
  (match fragment with
  | None -> ""
  | Some f -> "#" ^ f)

let compare a b =
  String.compare (to_output a) (to_output b)

(* FIXME use Tyre to convert both ways? *)
let parse_filename fn =
  if Filename.check_suffix fn ".wiki" then
    Site (Filename.chop_extension fn)
  else
    failwith ("not a .wiki input: " ^ fn)
