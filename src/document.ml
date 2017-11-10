type t =
  | Site of string
  | Project of {
    page: t';
    version: Version.t;
    project: string;
  }
and t' =
  | Page of string
  | Manual of string
  | Api of {
    subproject: string;
    file: string;
  }

let to_source = function
  | Site s -> s ^ ".wiki" (* FIXME *)
  | Project {page = Page p; version = v; project} ->
    project ^ "/" ^ (v |> Version.to_string) ^ "/" ^ p ^ ".wiki"

let to_string ?(ext=".html") = function
  | Site s -> s ^ ext (* FIXME *)

let to_uri ?fragment x =
  "/" ^ to_string x ^
  (match fragment with
  | None -> ""
  | Some f -> "#" ^ f)

let compare a b =
  String.compare (to_string a) (to_string b)

(* FIXME use Tyre to convert both ways! *)
let parse_filename fn =
  if Filename.check_suffix fn ".wiki" then
    Site (Filename.chop_extension fn)
  else
    failwith ("not a .wiki input: " ^ fn)
