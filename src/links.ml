open Tyxml
open Ocsimore_lib
open Api

let register name f =
  let wp_rec = Wiki_syntax.phrasing_wikicreole_parser in
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec (fun _ -> Error.wrap_phrasing name f);
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.wikicreole_parser_without_header_footer
    ~wp_rec (fun _ -> Error.wrap_phrasing name f);
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.phrasing_wikicreole_parser
    ~wp_rec (fun _ -> Error.wrap_phrasing name f);
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.menu_parser
    ~wp_rec (fun _ -> Error.wrap_phrasing name f)

let get_project_and_version bi args =
  match bi.Wiki_widgets_interface.bi_page with
  | Document.Site _ ->
    `Maybe_in_args
  | Document.Project {project; version; _} ->
    let project' =
      match get_opt args "project" with
      | None -> project
      | Some project' -> project'
    in
    let default =
      if project = project' then
        version |> Version.to_string
      else
        Projects.latest_of project' |> Version.to_string (* unrelated! *)
    in
    `Project (project', get ~default args "version" |> Version.parse)

let force_project_and_version bi args =
  match get_project_and_version bi args with
  | `Project (p, v) -> p, v
  | `Maybe_in_args ->
    let project = get args "project" in
    let default = Projects.latest_of project |> Version.to_string in
    project, get ~default args "version" |> Version.parse

let make_project bi args page =
  let project, version = force_project_and_version bi args in
  Document.Project {project; version; page}, project

let a_class project =
  Html.a_class ["ocsforge_doclink_" ^ project]

let manual_link bi args contents =
  let chapter = get ~default:"intro" args "chapter" in (* FIXME? *)
  let fragment = get_opt args "fragment" in
  let%lwt contents =
    match contents with
    | Some contents ->
      Wiki_syntax.xml_of_wiki
        (Wiki_syntax.cast_niwp Wiki_syntax.phrasing_wikicreole_parser)
        bi
        contents
    | None -> Lwt.fail (Error.Error "Empty contents")
  in
  let doc, project = make_project bi args (Document.Manual chapter) in
  let href = Html.a_href (Document.to_uri ?fragment doc) in
  bi.Wiki_widgets_interface.bi_add_link doc;
  Lwt.return [Html.a ~a:[a_class project; href] contents]

let local_link bi args =
  let src = get args "src" in
  match bi.Wiki_widgets_interface.bi_page with
  | Document.Project _ ->
    make_project bi args (Document.File src) |> fun (d, p) ->
    d, Some p
  | _ ->
    if List.mem_assoc "project" args then
      make_project bi args (Document.File src) |> fun (d, p) ->
      d, Some p
    else
      Document.Site src, None

let files_link bi args contents =
  let contents =
    match contents with
    | None -> [Html.pcdata (Filename.basename (get args "src"))]
    | Some contents -> contents
  in
  let doc, project = local_link bi args in
  let href = Html.a_href @@ Document.to_uri ~ext:"" doc in
  let a =
    href ::
    match project with
    | None -> []
    | Some p -> [a_class p]
  in
  bi.Wiki_widgets_interface.bi_add_link doc;
  Lwt.return [Html.a ~a contents]

let files_img bi args contents =
  let doc, _ = local_link bi args in
  let alt =
    match contents with
    | None -> Filename.basename (get args "src")
    | Some contents -> contents
  in
  let src = Document.to_uri ~ext:"" doc in
  Lwt.return [ Html.img ~src ~alt () ]

let api prefix bi args contents =
  let id = parse_contents (Eliom_lib.Option.map String.trim contents) in
  let doc, project =
    let project, version = force_project_and_version bi args in
    let subproject =
      match get_opt args "subproject" with
      | Some subproject -> subproject
      | None ->
        match bi.Wiki_widgets_interface.bi_page with
        | Document.Project {page = Document.Api {subproject; _}; _}
          when subproject <> ""
          && Projects.default_subproject_of project <> "" ->
            subproject
        | _ -> Projects.default_subproject_of project
    in
    let file = path_of_id ?prefix id in
    let page = Document.Api {subproject; file} in
    Document.Project {project; version; page}, project
  in
  let default = string_of_id ~spacer:".​" id in
  let body = [Html.pcdata @@ get ~default args "text"] in
  let fragment = fragment_of_id id in
  let href = Html.a_href @@ Document.to_uri ?fragment doc in
  bi.Wiki_widgets_interface.bi_add_link doc;
  Lwt.return [Html.a ~a:[a_class project; href] body]


let init () =
  register "a_api_type" (api (Some "type_"));
  register "a_api_code" (api (Some "code_"));
  register "a_api" (api None);
  register "a_manual" manual_link;
  (* register "a_file" files_link; FIXME WHY *)
  register "a_img" files_img
