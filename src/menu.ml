open Tyxml
open Tyxml.Html

let doctree bi args contents =
  let attrs = Wiki_syntax.parse_common_attribs args in
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
  }
  in
  let menus =
    files |>
    List.map Document.read |>
    Lwt_list.map_p @@
      Wiki_syntax.xml_of_wiki
        (Wiki_syntax.cast_wp Wiki_syntax.menu_parser)
        bi'
  in
  `Flow5 (
    let%lwt r = Lwt.map
        List.concat
        (menus :> Html_types.flow5 Tyxml_html.elt list list Lwt.t)
    in
    Lwt.return [ nav ~a:( a_class [ "how-doctree" ] :: attrs) r ]
  )

let docversion bi args contents =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let project, cur_version = Projects.get_implicit_project bi in
  let versions_links =
    (Projects.get project).Projects.versions |>
    List.map (fun (version, _) ->
        if version = cur_version
        then
          span ~a:[ a_class [ "how-versions-all-current" ] ]
            [ pcdata (Version.to_string version) ]
        else
          Html.a
            ~a:[ a_href
                   Document.(to_uri
                               (match bi.Wiki_widgets_interface.bi_page with
                                | Project p ->
                                  Project { page = p.page
                                          ; version
                                          ; project = p.project }
                                | _ ->
                                  Project { page = Manual ""
                                          ; version
                                          ; project })) ]
            [ pcdata (Version.to_string version) ])
  in
  `Flow5 (
    Lwt.return @@
    [ div ~a:( a_class [ "how-versions" ] :: attrs)
        [ input ~a:[ a_id "how-versions-toggle"
                   ; a_input_type `Checkbox ] ()
        ; label ~a:[ a_for "how-versions-toggle"
                   ; a_class [ "how-versions-current" ] ]
            [ pcdata "Version "
            ; pcdata (Version.to_string cur_version) ]
        ; div ~a:[ a_class [ "how-versions-all" ] ] versions_links ]
    ]
  )


let init () =
  Wiki_syntax.register_raw_wiki_extension
    ~name:"doctree"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> doctree);
  Wiki_syntax.register_raw_wiki_extension
    ~name:"docversion"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> docversion)
