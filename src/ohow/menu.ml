open Import
open Tyxml.Html
open Operators

let doctree _ args _ =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let { Global.root; manual; api; _ } = Global.options () in
  let find_menus p =
    p >>= (fun p -> Some (root +/+ p)) <$> find_files "menu.wiki" |? []
  in
  let pman_menus = find_menus manual in
  let papi_menus = find_menus api in
  let compile bi path =
    path |> read_file
    |> Wiki_syntax.xml_of_wiki (Wiki_syntax.cast_wp Wiki_syntax.menu_parser) bi
  in
  let bi_of_menu_file mf =
    let bi_page =
      Global.(
        match mf with
        | Manual _ ->
          Document.Project { page = Manual ""; project = ""; version = None }
        | Api _ ->
          Document.Project
            { page = Api { subproject = None; file = "" }
            ; project = ""
            ; version = None
            })
    in
    Wiki_widgets_interface.
      { bi_page
      ; bi_sectioning = true
      ; bi_add_link = ignore
      ; bi_content = []
      ; bi_title = ""
      }
  in
  let compile_with_menu_file mf =
    let f =
      Global.(
        match mf with
        | Manual f | Api f -> f)
    in
    let bi = bi_of_menu_file mf in
    Global.(with_menu_file mf (fun () -> compile bi f))
  in
  let compile_manual f = compile_with_menu_file (Global.Manual f) in
  let compile_api f = compile_with_menu_file (Global.Api f) in
  let menus =
    List.map compile_manual pman_menus @ List.map compile_api papi_menus
  in
  `Flow5
    (let r = List.concat menus in
     [ nav ~a:(a_class [ "how-doctree" ] :: attrs) r ])

let docversion _bi args _contents =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let { Global.docversions; _ } = Global.options () in
  let links =
    docversions |> List.map Version.parse
    |> List.map (fun v ->
           let old : Document.t = assert false in
           let new_ =
             match old with
             | Site _ as x -> x
             | Deadlink _ as x -> x
             | Project { page; version = _; project } ->
               Project { page; version = Some v; project }
           in
           let is_curr =
             match old with
             | Site _ -> fun _ -> false
             | Deadlink _ -> fun _ -> false
             | Project { version = None; _ } -> fun _ -> false
             | Project { version = Some v; _ } ->
               fun x -> Version.compare v x = 0
           in
           let dst = Document.to_relative_uri ~from:old new_ in
           let selected = if is_curr v then Some (a_selected ()) else None in
           option
             ~a:(a_value dst :: (selected <$> (fun s -> [ s ]) |? []))
             (txt (Version.to_string v)))
  in
  `Flow5
    [ txt "Version "
    ; select
        ~a:
          (a_class [ "how-versions" ]
          :: a_onchange "location = this.value;"
          :: attrs)
        links
    ]

let init () =
  Wiki_syntax.register_interactive_simple_flow_extension ~name:"doctree"
    ~reduced:false doctree;
  Wiki_syntax.register_simple_extension ~wp:Wiki_syntax.wikicreole_parser
    ~name:"docversion" docversion
