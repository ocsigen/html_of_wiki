open Utils
open Tyxml

(* let get_project_and_version bi args =
 *   match bi.Wiki_widgets_interface.bi_page with
 *   | Document.Site _ ->
 *     `Maybe_in_args
 *   | Document.Project {project; version; _} ->
 *     let project' =
 *       match get_opt args "project" with
 *       | None -> project
 *       | Some project' -> project'
 *     in
 *     let default =
 *       if project = project' then
 *         version |> Version.to_string
 *       else
 *         (Projects.get project').Projects.latest |>
 *         Version.to_string (\* unrelated! *\)
 *     in
 *     `Project (project', get ~default args "version" |> Version.parse)
 *   | Document.Deadlink _ -> assert false
 *
 * let force_project_and_version bi args =
 *   match get_project_and_version bi args with
 *   | `Project (p, v) -> p, v
 *   | `Maybe_in_args ->
 *     let project = get args "project" in
 *     let default =
 *       (Projects.get project).Projects.latest |>
 *       Version.to_string
 *     in
 *     project, get ~default args "version" |> Version.parse
 *
 * let make_project bi args page =
 *   let project, version = force_project_and_version bi args in
 *   Document.Project {project; version; page}, project
 *
 * let a_class project =
 *   Html.a_class ["ocsforge_doclink_" ^ project]
 *
 * let local_link bi args =
 *   let src = get args "src" in
 *   match bi.Wiki_widgets_interface.bi_page with
 *   | Document.Project _ ->
 *     make_project bi args (Document.Static (src, `File)) |> fun (d, p) ->
 *     d, Some p
 *   | _ ->
 *     if List.mem_assoc "project" args then
 *       make_project bi args (Document.Static (src, `File)) |> fun (d, p) ->
 *       d, Some p
 *     else
 *       Document.Site src, None
 *
 * let files_link bi args contents =
 *   let contents =
 *     match contents with
 *     | None -> [Html.pcdata (Filename.basename (get args "src"))]
 *     | Some contents -> [Html.pcdata contents]
 *   in
 *   let doc, project = local_link bi args in
 *   let href = Html.a_href @@ Document.to_uri doc in
 *   let a =
 *     href ::
 *     match project with
 *     | None -> []
 *     | Some p -> [a_class p]
 *   in
 *   bi.Wiki_widgets_interface.bi_add_link doc;
 *   Lwt.return [Html.a ~a contents]
 *
 * let files_img bi args contents =
 *   let doc, _ = local_link bi args in
 *   let alt =
 *     match contents with
 *     | None -> Filename.basename (get args "src")
 *     | Some contents -> contents
 *   in
 *   let src = Document.to_uri doc in
 *   Lwt.return [ Html.img ~src ~alt () ] *)

let a_link_of_uri ?fragment uri contents =
  let uri = uri ^ ".html" ^ (fragment >>= (fun f -> "#" ^ f) |? "") in
  Html.a ~a:[Html.a_href uri] [Html.pcdata (contents |? uri)]

let manual_link file root manual contents = function
  | [project; chapter; fragment; None] ->
    let uri = match (project, chapter) with
      | (Some p, Some c) -> concatl [rewind root file; (* inside this version dir *)
                                     ".."; (* inside all versions dir *)
                                     ".."; (* inside all project dir *)
                                     p; "latest"; manual; c]
      | (Some p, None) -> concatl [rewind root file; ".."; ".."; p; "latest"; "index"]
      | (None, Some c) -> concatl [rewind root file; manual; c]
      | (None, None) -> failwith "a_manual: must specify at least a project or a chapter"
    in
    let link = match fragment with
      | Some fragment -> a_link_of_uri ~fragment
      | None -> a_link_of_uri ?fragment:None
    in
    Lwt.return [link uri contents]
  | [_; _; _; Some _] -> failwith "version argument not supported yet" (* TODO *)
  | _ -> assert false

let api_link prefix file root api contents = function
  | [project; subproject; text; None] ->
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
  | [_; _; _; Some _] -> failwith "version argument not supported yet" (* TODO *)
  | _ -> assert false

let init file root manual api = Extensions.(
    register "a_manual" ["project"; "chapter"; "fragment"; "version"]
    @@ manual_link file root manual;
    [None; Some "type"; Some "code"] |> List.iter (fun p ->
        let name = "a_api" ^ (p >>= (fun p -> "_" ^ p) |? "") in
        let prefix = p >>= (fun p -> p ^ "_") in
        register name ["project"; "subproject"; "text"; "version"]
          @@ api_link prefix file root api))
