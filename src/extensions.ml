open Utils.Operators

let get_opts ?defaults opts args =
  let values = List.map (Ocsimore_lib.get_opt args) opts in
  defaults >>= (fun def ->
      List.map2 (fun sx -> function
          | Some d -> Some (sx |? d)
          | None -> sx) values def) |? values


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

let extension_with_opts ?defaults opts ext = fun _ args contents ->
  ext contents @@ get_opts ?defaults opts args

let register ?defaults name opts f =
  _reg name @@ extension_with_opts ?defaults opts f
