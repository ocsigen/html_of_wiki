let mtime_of fn =
  let open Unix in
  (stat fn).st_mtime

let to_rebuild source_t html =
  try
    mtime_of html < source_t
  with _ -> (* probably not found *)
    true (* not built yet *)

let rec create_tree dirs =
  (* create parents first *)
  if Filename.basename dirs = dirs then
    ()
  else
    create_tree (Filename.dirname dirs);
  (* then, create the child dir (base case, too) *)
  try
    Unix.mkdir dirs 0755
  with Unix.(Unix_error (EEXIST, _, _)) ->
    ()

(* FIXME avoid leaking handles *)
let explore max_depth output force dry files =
  Document.output := output;
  Projects.init ".";
  let open Compiler in
  let header = read_file "header" in (* FIXME *)
  let footer = read_file "footer" in (* FIXME *)
  let compile add_link page =
    let inp = read_file (Document.to_source page) in
    let content = Lwt_main.run (parse ~page add_link inp) in
    let title = extract_h1 content in
    let out_fn = Document.to_output page in
    create_tree (Filename.dirname out_fn);
    let ch = open_out (if dry then "/dev/null" else out_fn) in
    render ch ~header ~footer ~title content;
    close_out ch
  in
  let dead = ref 0 in
  let processed = ref 0 in
  let docs = List.map Document.parse_filename files in
  let module C = Crawler.Make(Document) in
  let set = C.bfs ?max_depth docs ~f:(fun ~add ?pred cur ->
    try
      let source = Document.to_source cur in
      let source_t =
        try
          mtime_of source
        with Unix.Unix_error (e, _, _) ->
          let from =
            match pred with
            | None -> ""
            | Some d -> Document.to_source d ^ " -> "
          in
          prerr_endline @@ from ^ source ^ ": " ^ Unix.error_message e;
          incr dead;
          raise Exit
      in
      if force || to_rebuild source_t (Document.to_output cur) then (
        try
          (*
          let add x =
            print_endline @@ "add: " ^ (Document.to_source x);
            add x
          in
          *)
          compile add cur;
          incr processed
        with Unix.Unix_error (e, _, _) ->
          prerr_endline @@ source ^ ": " ^ Unix.error_message e;
          raise Exit
      )
    with Exit ->
      ()
  ) in
  let n = C.Set.cardinal set in
  Printf.fprintf stderr "%d targets: %d cached, %d dead, %d processed.\n"
    n
    (n - !dead - !processed)
    !dead
    !processed


open Cmdliner

let files = Arg.(non_empty & pos_all non_dir_file [] & info [] ~docv:"FILE")

let depth =
  let doc = "Follow links down to a given depth, 0 disables recursion." in
  Arg.(value & opt (some int) None & info ["m"; "max-depth"] ~doc)

let output =
  let doc = "Write output to directory." in
  Arg.(value & opt dir !Document.output & info ["o"; "output"] ~doc)

let dry =
  let doc = "Don't output HTML files." in
  Arg.(value & flag & info ["n"; "dry-run"] ~doc)

let force =
  let doc = "Force a complete rebuild." in
  Arg.(value & flag & info ["f"; "force"; "fresh"] ~doc)

let cmd =
  let doc = "compile Ocsigen's Wikicreole dialect to HTML" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) compiles documentation files to HTML. Header and footers are
        read directly from text files, the \\$TITLE variable is injected in the
        header, extracted from the first <h1> tag of the output.";
    `S Manpage.s_examples;
    `P "cd ~/dev/ocsigen.org-data && html_of_wiki index.wiki";
    `S Manpage.s_bugs;
    `P "Currently, it only works from ocsigen.org-data's root. As a workaround,
        just update a fake index file with links to the specific parts you want
        to compile.";
    `P "Report bugs to <guillaume.huysmans@student.umons.ac.be>.";
  ] in
  Term.(const explore $ depth $ output $ force $ dry $ files),
  Term.info "html_of_wiki" ~version:"v1.0.0" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)
