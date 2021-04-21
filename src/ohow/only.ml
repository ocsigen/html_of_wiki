let do_pdfonly _ _ _ = `Phrasing_without_interactive []

let do_webonly _ _ contents =
  match contents with
  | None -> `Phrasing_without_interactive []
  | Some contents -> `Flow5 contents

let init () =
  Wiki_syntax.register_wiki_phrasing_extension ~name:"pdfonly"
    { Wiki_syntax.ppp = do_pdfonly };
  Wiki_syntax.register_wiki_phrasing_extension ~name:"webonly"
    { Wiki_syntax.ppp = do_webonly };
  Wiki_syntax.register_wiki_extension ~name:"webonly"
    ~wp:Wiki_syntax.phrasing_wikicreole_parser
    ~wp_rec:Wiki_syntax.phrasing_wikicreole_parser ~ni_plugin:do_webonly
    do_webonly
