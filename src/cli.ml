let file_cmd =
  let doc = "A wikicreole file to convert to HTML." in
  Cmdliner.Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)

let print_cmd =
  let doc = "Print the HTML to stdout." in
  Cmdliner.Arg.(value & flag & info ["p"; "print"] ~doc)

let outfile_cmd =
  let doc = "Writes the HTML into the given file. Always overwrites.
Overrides $(b,--print)." in
  Cmdliner.Arg.(value & opt (some string) None & info ["o"; "output"]
                  ~docv:"FILE" ~doc)

let cwd = Sys.getcwd ()
let root_cmd =
  let doc = "Use the given root directory." in
  Cmdliner.Arg.(value & opt string cwd & info ["root"]
                  ~docv:"DIR" ~doc)

let manual_cmd =
  let doc = "Use the given manual path." in
  Cmdliner.Arg.(value & opt string cwd & info ["manual"]
                  ~docv:"DIR" ~doc)

let api_cmd =
  let doc = "Use the given api path." in
  Cmdliner.Arg.(value & opt string cwd & info ["api"]
                  ~docv:"DIR" ~doc)

let img_cmd =
  let doc = "Use the given image directory path." in
  Cmdliner.Arg.(value & opt string cwd & info ["images"]
                  ~docv:"DIR" ~doc)

let assets_cmd =
  let doc = "Use the given assets directory path." in
  Cmdliner.Arg.(value & opt string cwd & info ["assets"]
                  ~docv:"DIR" ~doc)

let info_cmd = Cmdliner.(
    let doc = "Converts a wikicreole file into an HTML file." in
    let man = [
      `S Manpage.s_description;
      `P "$(tname) is a command line utility for compiling Wikicreole files
into HTML.";

      `S "EXTENSIONS";
      `P "Extensions are supported. Some are built-in which are described
in the following sections.";

      `S "<<a_manual project chapter version fragment|>>";
      `P "Expands to a link to the `chapter' page inside `project''s
manual directory and for a given `version'. A `fragment' can be added.
Default values:";
      `P "- project: current project";
      `P "- chapter: `project''s index page";
      `P "- version: \"latest\"";
      `P "- fragment: \"\"";
      `P "The expansion fails when neither `project' nor `chapter' is given.";

      `S "<<a_api project sub-project version text|thing>>";
      `P "Expands to a link to the `thing' of the given `project' or/and
`subproject' of the given `version'. The link's text can be chosen
using the `text' argument. Default values:";
      `P "- project: current project";
      `P "- subproject: \"\"";
      `P "- version: \"latest\"";
      `P "- text: `thing'";

      `S "<<a_api_type project subproject version text|thing>>";
      `P "See a_api.";

      `S "<<a_api_code project subproject version text|thing>>";
      `P "See a_api.";

      `S "LINKS";
      `P "$(tname) is supposed to compile each wiki independently but these
might contain links. Since $(tname) is not responsible for checking for dead
links, it guesses the redirection for each link in a consistent way. However
$(tname) sill needs to assume a bunch of things and requires some extra
information.";
      `P "- All the projects which links to each other must be placed inside
the same directory.";
      `P "- Inside each project directory are the root directories of each
version of the documentation.";
      `P "- The architecture inside each root (version of project) directory
is less strict. However, if the a_manual tag is used, then the manual directory
must not contain wikis in sub-directories.";
      `P "- If any of the a_api* tags is used, then each sub-project directory
- if any - must be located inside the api directory and every wiki must be
directly inside the api directory or its sub-project directory (i.e: no
sub-directories are allowed).";
      `P "The options $(b,--root), $(b,--manual) and $(b,--api) can be used
to explicitly provide the path to the, respectively, the root directory, the
manual directory and the api directory."
    ] in
    Term.info "ohow" ~version:"v0.0.0" ~doc ~exits:Term.default_exits ~man)


let register_options k print outfile root manual api images assets files =
  let opts = {Global.print; outfile; root; manual; api; images; assets; files} in
  Global.with_options opts (fun () -> k opts)

let run main =
  let ohow_cmd = Cmdliner.Term.(
      const (register_options main)
      $ print_cmd
      $ outfile_cmd
      $ root_cmd
      $ manual_cmd
      $ api_cmd
      $ img_cmd
      $ assets_cmd
      $ file_cmd)
  in
  Cmdliner.Term.(exit @@ eval (ohow_cmd, info_cmd))
