open Tyxml
open Tyxml.Html
open Utils.Operators

let doctree _ args _ =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let {Global.root; manual; api} = Global.options () in
  let pman = root +/+ manual in
  let pman_menu = pman +/+ "menu.wiki" in
  let papi_menus = Utils.find_files "menu.wiki" @@ root +/+ api in

  let menu_exc_msg =
    Printf.sprintf "missing required file %s for doctree" pman_menu
  in
  Utils.check_errors [(menu_exc_msg, lazy (Sys.file_exists pman_menu))];

  let compile bi path =
    path
    |> Utils.readfile
    |> (Wiki_syntax.xml_of_wiki
          (Wiki_syntax.cast_wp Wiki_syntax.menu_parser)
          bi)
    |> Lwt_main.run
  in
  let compile_manual = compile Wiki_widgets_interface.{
      bi_page = Document.(Project {page = Manual "";
                                   project = "";
                                   version = Version.Dev});
      bi_sectioning = true;
      bi_add_link = ignore;
      bi_content = Lwt.return [];
      bi_title = ""}
  in
  let compile_api = compile Wiki_widgets_interface.{
      bi_page = Document.(Project {page = Api { subproject = ""; file = ""};
                                   project = "";
                                   version = Version.Dev});
      bi_sectioning = true;
      bi_add_link = ignore;
      bi_content = Lwt.return [];
      bi_title = ""}
  in
  let menus = Lwt.return (compile_manual pman_menu
                          :: List.map compile_api papi_menus) in
  `Flow5 (let%lwt r = Lwt.map List.concat menus in
          Lwt.return [nav ~a:(a_class ["how-doctree"] :: attrs) r])

let docversion bi args contents =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let file = Global.current_file () in
  let root = Global.root () in
  let versions = Global.project_dir ()
                 |> Utils.a'_sorted_dir_files
                 |> List.filter Paths.is_visible_dir
  in
  let links = versions |> List.map (fun v ->
      let dst = Paths.(rewind root file +/+ up +/+ v +/+ "index.html") in
      option ~a:[a_value dst] (pcdata v))
  in
  `Flow5 (Lwt.return [pcdata "Version ";
                      select ~a:(a_class [ "how-versions" ]
                                 :: a_onchange "location = this.value;"
                                 :: attrs)
                        links])

let init () =
  Wiki_syntax.register_interactive_simple_flow_extension
    ~name:"doctree"
    ~reduced:false
    doctree;
  Wiki_syntax.register_simple_extension
    Wiki_syntax.wikicreole_parser
    "docversion"
    docversion
