open Utils
open Tyxml

let a_link_of_uri ?fragment uri contents =
  let uri = uri ^ ".html" ^ (fragment >>= (fun f -> "#" ^ f) |? "") in
  Html.a ~a:[Html.a_href uri] [Html.pcdata (contents |? uri)]

let manual_link file root manual contents = function
  | [project; chapter; fragment; Some version] ->
    let uri = match (project, chapter) with
      | (Some p, Some c) -> concatl [rewind root file; (* inside this version dir *)
                                     ".."; (* inside all versions dir *)
                                     ".."; (* inside all project dir *)
                                     p; version; manual; c]
      | (Some p, None) -> concatl [rewind root file; ".."; ".."; p; version; "index"]
      | (None, Some c) -> concatl [rewind root file; manual; c]
      | (None, None) -> failwith "a_manual: no project nor chapter arg found"
    in
    let link = match fragment with
      | Some fragment -> a_link_of_uri ~fragment
      | None -> a_link_of_uri ?fragment:None
    in
    Lwt.return [link uri contents]
  | _ -> assert false

let api_link prefix file root api contents = function
  | [project; subproject; text; Some version] ->
    let id = Api.parse_contents (contents >>= String.trim) in
    let base = match (project, subproject) with
      | (Some p, Some s) -> concatl [rewind root file; ".."; ".."; p; "latest"; api; s]
      | (Some p, None) -> concatl [rewind root file; ".."; ".."; p; "latest"; api]
      | (None, Some s) -> concatl [rewind root file; api; s]
      | (None, None) -> Filename.concat (rewind root file) api
    in
    let uri = Filename.concat base @@ Api.path_of_id ?prefix id in
    let fragment = Api.fragment_of_id id in
    let body = text |? (Api.string_of_id ~spacer:"." id) in
    Lwt.return [a_link_of_uri ?fragment uri (Some body)]
  | _ -> assert false

let init file root manual api = Extensions.(
    register "a_manual"
      ["project"; "chapter"; "fragment"; "version"]
      ~defaults:[None; None; None; Some "latest"]
      (manual_link file root manual);
    [None; Some "type"; Some "code"] |> List.iter (fun p ->
        let name = "a_api" ^ (p >>= (fun p -> "_" ^ p) |? "") in
        let prefix = p >>= (fun p -> p ^ "_") in
        register name
          ["project"; "subproject"; "text"; "version"]
          ~defaults:[None; None; None; Some "latest"]
          (api_link prefix file root api)))
