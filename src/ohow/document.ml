type t =
  | Site of string
  | Project of
      { page : project_page
      ; version : Version.t
      ; project : string
      }
  | Deadlink of exn

and project_page =
  | Static of string * [ `File | `Folder ]
  | Template
  | Page of string
  | Manual of string
  | Api of
      { subproject : string
      ; file : string
      }

let to_string src with_html d =
  let src, ext =
    if src then ("src/", ".wiki") else ("", if with_html then ".html" else "")
  in
  match d with
  | Site s -> s ^ ext
  | Project { page = Template; project; _ } -> project ^ "/template" ^ ext
  | Project { page; version = v; project } ->
    let p =
      match page with
      | Page p -> p ^ ext
      | Manual m -> "manual/" ^ src ^ m ^ ext
      | Api { subproject; file } ->
        "api/" ^ (if subproject = "" then "" else subproject ^ "/") ^ file ^ ext
      | Template -> assert false (* handled above... *)
      | Static (p, `File) | Static (p, `Folder) -> "manual/files/" ^ p
    in
    project ^ "/" ^ (v |> Version.to_string) ^ "/" ^ p
  | Deadlink e -> Printexc.to_string e

let to_uri ?fragment x =
  "/" ^ to_string false false x
  ^
  match fragment with
  | None -> ""
  | Some f -> "#" ^ f
