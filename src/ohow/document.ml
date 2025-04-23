type t =
  | Site of string
  | Project of { page : project_page; version : Version.t; project : string }
  | Deadlink of exn

and project_page =
  | Static of string * [ `File | `Folder ]
  | Template
  | Page of string
  | Manual of string
  | Api of { subproject : string option; file : string }

let to_string d =
  match d with
  | Site s -> s
  | Project { page = Template; project; _ } -> project ^ "/template"
  | Project { page; version = v; project } ->
      let p =
        match page with
        | Page p -> p
        | Manual m -> "manual/" ^ m
        | Api { subproject; file } ->
            "api/"
            ^ (match subproject with
              | None -> ""
              | Some subproject -> subproject ^ "/")
            ^ file
        | Template -> assert false (* handled above... *)
        | Static (p, `File) | Static (p, `Folder) -> "manual/files/" ^ p
      in
      project ^ "/" ^ (v |> Version.to_string) ^ "/" ^ p
  | Deadlink e ->
      Printf.eprintf "Deadlink: %s\n" (Printexc.to_string e);
      "data:text/plain;base64," ^ Base64.encode_string (Printexc.to_string e)

let to_uri ?fragment x =
  "/" ^ to_string x ^ match fragment with None -> "" | Some f -> "#" ^ f
