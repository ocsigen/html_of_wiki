open Tyxml
open Tyxml.Html

let doctree bi args contents =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let project, version = Projects.get_implicit_project bi in
  let manual_file =
    Document.(Project {page = Manual "menu"; version; project})
  in
  let api_files =
    (List.assoc version (Projects.get project).Projects.versions |> fun v ->
     let file = "menu" in
     if v = []
     then (* no sub project *)
       [ Document.(Project { page = Api { subproject = ""; file };
                             version;
                             project }) ]
     else
       List.map (fun subproject ->
           Document.(Project { page = Api { subproject; file };
                               version;
                               project })
         ) v
    )
  in
  let api_bi = Wiki_widgets_interface.{
      bi_page = Document.(Project { page = Api { subproject = ""; file = ""}
                                  ; project; version});
      bi_sectioning = true;
      bi_add_link = bi.bi_add_link;
      bi_content = Lwt.return [];
      bi_title = "";
  }
  in
  let manual_bi = Wiki_widgets_interface.{
      bi_page = Document.(Project {page = Manual ""; project; version});
      bi_sectioning = true;
      bi_add_link = bi.bi_add_link;
      bi_content = Lwt.return [];
      bi_title = "";
  }
  in
  let try_read f = try Document.read f with _ -> "" in
  let menus =
    api_files |>
    List.map try_read |>
    Lwt_list.map_p @@
      Wiki_syntax.xml_of_wiki
        (Wiki_syntax.cast_wp Wiki_syntax.menu_parser)
        api_bi
  in
  let menus =
    let%lwt menus = menus in
    let%lwt m = Wiki_syntax.xml_of_wiki
        (Wiki_syntax.cast_wp Wiki_syntax.menu_parser)
        manual_bi
        (try_read manual_file)
    in
    Lwt.return (m :: menus)
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
          option ~a:[ a_selected () ] (pcdata (Version.to_string version))
        else
          option
            ~a:[ a_value
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
            (pcdata (Version.to_string version)))
  in
  `Flow5 (
    Lwt.return @@
    [ pcdata "Version "
    ; select ~a:( a_class [ "how-versions" ] ::
                  a_onchange "location = this.value;" :: attrs) versions_links ]
  )


let init () =
  Wiki_syntax.register_interactive_simple_flow_extension
    ~name:"doctree"
    ~reduced:false
    doctree;
  Wiki_syntax.register_simple_extension
    Wiki_syntax.wikicreole_parser
    "docversion"
    docversion
