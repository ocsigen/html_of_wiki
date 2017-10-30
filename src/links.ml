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

let to_uri bi args =
  let path =
    match project, version with
    | Some p, Some v -> "/" ^ p ^ "/" ^ v ^ "/"
    | Some p, None -> "/" ^ p ^ "/dev/"
    | None, Some v -> "../" ^ v ^ "/"
    | None, None -> ""
  in
  let file =
    match file with
    | Some f -> f
    | None -> ""
  in
  let fragment =
    match fragment with
    | Some f -> "#" ^ f
    | None -> ""
  in
  path ^ file ^ fragment

let a_class bi project =
  let project =
    match project with
    | None -> "xxx" (* FIXME read bi *)
    | Some p -> p
  in
  Html.a_class ["ocsforge_doclink_" ^ project]

let manual_link bi args contents =
  let project = get ~default:bi. args "project" in
  let version = get_opt args "version" in
  let chapter = get ~default:"" args "chapter" in
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
  let file = Some (chapter ^ ".html") in
  let href = Html.a_href @@ to_uri project version file fragment in
  Lwt.return [Html.a ~a:[a_class bi project; href] contents]

let files_link bi args contents =
  let src = get args "src" in
  let project = get_opt args "project" in
  let version = get_opt args "version" in
  let contents =
    match contents with
    | None -> [Html.pcdata (Filename.basename src)]
    | Some contents -> contents
  in
  let href = Html.a_href @@ to_uri project version (Some src) None in
  Lwt.return [Html.a ~a:[a_class bi project; href] contents]

let files_img bi args contents =
  let src = get args "src" in
  let project = get_opt args "project" in
  let version = get_opt args "version" in
  let alt =
    match contents with
    | None -> Filename.basename src
    | Some contents -> contents
  in
  let src = to_uri project version (Some src) None in
  Lwt.return [ Html.img ~src ~alt () ]

let api prefix bi args contents =
  (* Get arguments *)
  let project =
    let p = get_opt args "project" in
    let s = get_opt args "subproject" in
    match p, s with
    | None, None -> Lwt.fail (Error.Error "missing project and/or subproject")
    | _, _ ->
    xxx
  in
  let version = get_opt args "version" in
  let id = parse_contents args contents in
  let default = string_of_id ~spacer:".<U+200B>" id in
  let body = [Html.pcdata @@ get ~default args "text"] in
  let fragment = fragment_of_id id in
  let file = Some (sub ^ "/" ^ path_of_id ?prefix id) in
  let href = Html.a_href @@ to_uri project version file fragment in
  Lwt.return [Html.a ~a:[a_class bi project; href] body]


let init () =
  register "a_api_type" (api (Some "type_"));
  register "a_api_code" (api (Some "code_"));
  register "a_api" (api None);
  register "a_manual" manual_link;
  (*register "a_file" files_link;*)
  register "a_img" files_img
