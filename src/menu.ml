let doctree bi args contents =
  let project, version = Projects.get_implicit_project bi in
  let files =
    Document.(Project {page = Manual "menu"; version; project}) ::
    (List.assoc version (Projects.get project).Projects.versions |>
    List.map (fun subproject ->
      let file = "menu" in
      Document.(Project {page = Api {subproject; file}; version; project})
    ))
  in
  let bi' = Wiki_widgets_interface.{
    bi_page = Document.(Project {page = Template; project; version});
    bi_sectioning = true;
    bi_add_link = bi.bi_add_link;
    bi_content = Lwt.return [];
    bi_title = "";
  } in
  let menus =
    files |>
    List.map Document.read |>
    Lwt_list.map_p @@
      Wiki_syntax.xml_of_wiki
        (Wiki_syntax.cast_wp Wiki_syntax.menu_parser)
        bi'
  in
  Lwt.map List.concat
    (menus :> Html_types.flow5 Tyxml_html.elt list list Lwt.t)

  (*
  let versions =
    (Projects.get bi."eliom").Projects.versions |>
    List.map fst |>
    List.map Version.to_string
  *)

let init () =
  Wiki_syntax.register_raw_wiki_extension
    ~name:"doctree"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> Error.wrap_flow5 "doctree" doctree);
