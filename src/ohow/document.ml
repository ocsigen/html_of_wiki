let output = ref "../ocsigen.org-repositories/"

let pp_exn _ _ = ()

(* don't care *)

type t =
  | Site of string
  | Project of {page : t'; version : Version.t; project : string}
  | Deadlink of exn
[@@deriving show]

and t' =
  | Static of string * [`File | `Folder]
  | Template
  | Page of string
  | Manual of string
  | Api of {subproject : string; file : string}
[@@deriving show]

let to_string src with_html d =
  let src, ext =
    if src then "src/", ".wiki" else "", if with_html then ".html" else ""
  in
  match d with
  | Site s -> s ^ ext
  | Project {page = Template; project; _} -> project ^ "/template" ^ ext
  | Project {page; version = v; project} ->
      let p =
        match page with
        | Page p -> p ^ ext
        | Manual m -> "manual/" ^ src ^ m ^ ext
        | Api {subproject; file} ->
            "api/" ^ (if subproject = "" then "" else subproject ^ "/") ^ file ^ ext
        | Template -> assert false (* handled above... *)
        | Static (p, `File) | Static (p, `Folder) -> "manual/files/" ^ p
      in
      project ^ "/" ^ (v |> Version.to_string) ^ "/" ^ p
  | Deadlink e -> Printexc.to_string e

let to_source = function
  | Project {page = Page _; _} -> None
  | d -> Some (to_string true false d)

let to_output d = !output ^ to_string false true d

let to_uri ?fragment x =
  "/" ^ to_string false false x ^ match fragment with None -> "" | Some f -> "#" ^ f

let compare a b = String.compare (to_output a) (to_output b)

(* FIXME use Tyre to convert both ways? *)
let parse_filename fn =
  if Filename.check_suffix fn ".wiki"
  then Site (Filename.chop_extension fn)
  else failwith ("not a .wiki input: " ^ fn)

let read_file ?(buffer_size = 4096) filename =
  let ch = open_in filename in
  let buf = Buffer.create buffer_size in
  ( try
      while true do
        Buffer.add_string buf (input_line ch);
        Buffer.add_char buf '\n'
      done
    with End_of_file -> () );
  close_in ch;
  Buffer.to_bytes buf |> Bytes.to_string

let read d = to_source d |> How_lib.Option.force |> read_file
