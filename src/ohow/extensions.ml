let get_opts ?defaults opts args : string option list =
  let values = List.map (Ocsimore_lib.get_opt args) opts in
  match defaults with
  | None -> values
  | Some defaults ->
    let pick_first_some first second =
      match first with
      | Some _ -> first
      | None -> second
    in
    List.map2 (fun v def -> pick_first_some v def) values defaults

let _reg name f =
  let wp_rec = Wiki_syntax.phrasing_wikicreole_parser in
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.wikicreole_parser ~wp_rec (fun _ ->
      Error.wrap_phrasing name f);
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.wikicreole_parser_without_header_footer ~wp_rec (fun _ ->
      Error.wrap_phrasing name f);
  Wiki_syntax.register_raw_wiki_extension ~name
    ~wp:Wiki_syntax.phrasing_wikicreole_parser ~wp_rec (fun _ ->
      Error.wrap_phrasing name f);
  Wiki_syntax.register_raw_wiki_extension ~name ~wp:Wiki_syntax.menu_parser
    ~wp_rec (fun _ -> Error.wrap_phrasing name f)

let extension_with_opts ?defaults opts ext _ args contents =
  ext contents @@ get_opts ?defaults opts args

let register ?defaults name opts f =
  _reg name @@ extension_with_opts ?defaults opts f
