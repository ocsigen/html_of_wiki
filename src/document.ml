let output = ref "../gen/"

type t =
  | Site of string
  | Project of {
    page: t';
    version: Version.t;
    project: string;
  }
  | Deadlink of exn
and t' =
  | File of string
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
      | File f -> "manual/files/" ^ f
    in
    project ^ "/" ^ (v |> Version.to_string) ^ "/" ^ p
  | Deadlink e -> Printexc.to_string e

let to_source = function
  | Project {page = Page _; _} -> None
  | Project {page = File _; _} as d -> Some (to_string ~src:true d)
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

let read_file ?(buffer_size=4096) filename =
  let ch = open_in filename in
  let buf = Buffer.create buffer_size in
  begin try
    while true do
      Buffer.add_string buf (input_line ch);
      Buffer.add_char buf '\n';
    done
  with End_of_file ->
    ()
  end;
  close_in ch;
  Buffer.to_bytes buf |> Bytes.to_string

let read d =
  to_source d |>
  Eliom_lib.Option.force |>
  read_file
