open Utils

let get_opts opts args = List.map (Ocsimore_lib.get_opt args) opts

let _reg name f =
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

let extension_with_opts opts ext = fun _ args contents ->
  ext contents @@ get_opts opts args

let register name opts f = _reg name @@ extension_with_opts opts f
