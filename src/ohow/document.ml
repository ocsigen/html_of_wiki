open Import

type t =
  | Site of string
  | Site_static of string * [ `File | `Folder ]
  | Project of
      { page : project_page
      ; version : Version.t option
      ; project : string
      }
  | Deadlink of exn

and project_page =
  | Static of string * [ `File | `Folder ]
  | Template
  | Page of string
  | Manual of string
  | Api of
      { subproject : string option
      ; file : string
      }

let encode =
  String.map (function
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as x -> x
    | _ -> '_')

let parse_page ~project ~version page =
  let page =
    match String.cut '/' page with
    | Some ("manual", page) -> Manual page
    | Some ("api", file) -> Api { file; subproject = None }
    | Some _ | None -> Page page
  in
  Project { page; version = Some version; project }

let parse_page' ~project page =
  match String.cut '/' page with
  | None ->
    if page = ""
    then Site_static (project ^ "/", `Folder)
    else if Filename.extension page = ""
    then Site (project ^ "/" ^ page)
    else Site_static (project ^ "/" ^ page, `File)
  | Some (v, rest) -> (
    match Version.parse v with
    | version -> parse_page ~project ~version rest
    | exception _ ->
      if page = ""
      then Site_static (project ^ "/", `Folder)
      else if Filename.extension page = ""
      then Site (project ^ "/" ^ page)
      else Site_static (project ^ "/" ^ page, `File))

let page_to_parts page =
  match page with
  | Page p -> [ p ]
  | Manual m -> [ "manual"; m ]
  | Api { subproject; file } -> (
    match subproject with
    | None -> [ "api"; file ]
    | Some subproject -> [ subproject; file ])
  | Template -> assert false (* handled above... *)
  | Static (p, `File) | Static (p, `Folder) -> [ "manual"; "files"; p ]

let to_string d =
  match d with
  | Site "" | Site "/" -> ""
  | Site s ->
    let s = if String.get s 0 = '/' then String.sub s 1 (String.length s - 1) else s
    in
    s ^ Global.suffix ()
  | Site_static (s,_) ->
    let s = if String.get s 0 = '/' then String.sub s 1 (String.length s - 1) else s
    in
    s
  | Project { page = Template; project; _ } ->
    String.concat "/" [ project; "template" ]
  | Project { page; version = Some v; project } ->
    let p = page_to_parts page in
    String.concat "/" (project :: Version.to_string v :: p) ^ Global.suffix ()
  | Project { page; version = None; project } ->
    let p = page_to_parts page in
    String.concat "/" (project :: "latest" :: p) ^ Global.suffix ()
  | Deadlink e -> "data:text/plain;" ^ encode (Printexc.to_string e)

let to_absolute_uri ?fragment x =
  let s = to_string x in
  "/" ^ s
  ^
  match fragment with
  | None -> ""
  | Some f -> "#" ^ f

let to_relative_uri ~from ?fragment x =
  match x with
  | Deadlink _ -> to_string x
  | _ -> (
    let x1 = Paths.list_of_path (to_string from)
    and x2 = Paths.list_of_path (to_string x) in
    let rec up t1 t2 =
      match t1 with
      | [] -> t2
      | [ _ ] -> t2
      | _ :: t1 -> up t1 (Paths.up :: t2)
    in
    let rec loop x1 x2 =
      match (x1, x2) with
      | [], _ -> x2
      | _, [] -> up x1 []
      | h1 :: t1, h2 :: t2 -> if h1 = h2 then loop t1 t2 else up x1 x2
    in
    let uri = String.concat "/" (loop x1 x2) in
    match fragment with
    | None -> uri
    | Some fragment -> uri ^ "#" ^ fragment)
