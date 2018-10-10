open Utils.Operators
open Tyxml

let cat = Paths.path_of_list

let a_link_of_uri ?fragment ?suffix uri contents =
  let uri = uri ^ (suffix |? "") ^ (fragment >>= (fun f -> "#" ^ f) |? "") in
  Html.a ~a:[Html.a_href uri] [Html.pcdata (contents |? uri)]

let manual_link contents = function
  | [project; chapter; fragment; Some version] ->
    let file = Global.current_file () in
    let {Global.root; manual} = Global.options () in
    let uri = match (project, chapter) with
      | (Some p, Some c) -> cat [Paths.rewind root file; (* inside this version dir *)
                                 ".."; (* inside all versions dir *)
                                 ".."; (* inside all project dir *)
                                 p; version; manual; c]
      | (Some p, None) -> cat [Paths.rewind root file; ".."; ".."; p; version; "index"]
      | (None, Some c) -> cat [Paths.rewind root file; manual; c]
      | (None, None) -> failwith "a_manual: no project nor chapter arg found"
    in
    let link = match fragment with
      | Some fragment -> a_link_of_uri ~fragment
      | None -> a_link_of_uri ?fragment:None
    in
    Lwt.return [link ~suffix:".html" uri contents]
  | _ -> assert false

let api_link prefix contents = function
  | [project; subproject; text; Some version] ->
    let file = Global.current_file () in
    let {Global.root; api} = Global.options () in
    let id = Api.parse_contents (contents >>= String.trim) in
    let base = match (project, subproject) with
      | (Some p, Some s) -> cat [Paths.rewind root file; ".."; ".."; p; "latest"; api; s]
      | (Some p, None) -> cat [Paths.rewind root file; ".."; ".."; p; "latest"; api]
      | (None, Some s) -> cat [Paths.rewind root file; api; s]
      | (None, None) -> Filename.concat (Paths.rewind root file) api
    in
    let uri = Filename.concat base @@ Api.path_of_id ?prefix id in
    let fragment = Api.fragment_of_id id in
    let body = text |? (Api.string_of_id ~spacer:"." id) in
    Lwt.return [a_link_of_uri ?fragment ~suffix:".html" uri (Some body)]
  | _ -> assert false

let img_link contents = function
  | [Some src] ->
    let file = Global.current_file () in
    let {Global.root; images} = Global.options () in
    let uri = cat [Paths.rewind root file; images; src] in
    let alt = Filename.basename src in
    Lwt.return [Html.img ~src:uri ~alt ()]
  | [None] -> failwith "a_img: no src argument error"
  | _ -> assert false

let file_link contents = function
  | [Some src] ->
    let file = Global.current_file () in
    let {Global.root; assets} = Global.options () in
    let uri = cat [Paths.rewind root file; assets; src] in
    Lwt.return [a_link_of_uri uri (Some (contents |? Filename.basename uri))]
  | [None] -> failwith "a_file: no src argument error"
  | _ -> assert false


let init () = Extensions.(
    register "a_manual"
      ["project"; "chapter"; "fragment"; "version"]
      ~defaults:[None; None; None; Some "latest"]
      manual_link;
    [None; Some "type"; Some "code"] |> List.iter (fun p ->
        let name = "a_api" ^ (p >>= (fun p -> "_" ^ p) |? "") in
        let prefix = p >>= (fun p -> p ^ "_") in
        register name
          ["project"; "subproject"; "text"; "version"]
          ~defaults:[None; None; None; Some "latest"]
          (api_link prefix));
    register "a_img" ["src"] img_link;
    register "a_file" ["src"] file_link)
