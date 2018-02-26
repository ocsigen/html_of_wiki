let rec create_tree dirs =
  (* create parents first *)
  if Filename.basename dirs = dirs then
    ()
  else
    create_tree (Filename.dirname dirs);
  (* then, create the child dir (base case, too) *)
  try
    Unix.mkdir dirs 0o755
  with Unix.(Unix_error (EEXIST, _, _)) ->
    ()

let copy f =
  let source = Document.to_source f |> How_lib.Option.force in
  let dest = Document.to_output f in
  try
    (match f with
    | Document.Project {page = Document.Static (_, `File); _} ->
      let dest = Filename.dirname dest in
      create_tree dest;
      FileUtil.cp [source] dest
    | Document.Project {page = Document.Static (_, `Folder); _} ->
      create_tree dest;
      FileUtil.cp ~recurse:true [Filename.dirname source] dest
    | _ ->
      assert false);
    print_endline @@ "\t" ^ source
  with FileUtil.CpError e ->
    raise (Sys_error e)

(* FIXME avoid leaking handles *)
let explore max_depth output dry files =
  Document.output := output;
  Projects.init ".";
  let open Compiler in
  let compile add_link page =
    let my_add d =
      match Document.to_source d with
      | None ->
        (* no corresponding source, don't add it *)
        (* TODO collect names! *)
        ()
      | Some _ -> add_link d
    in
    let content, title =
      let content =
        let inp = Document.read page in
        let empty = Lwt.return [] in
        Lwt_main.run (parse ~page my_add empty inp)
      in
      let title =
        extract_h1 content |>
        How_lib.Option.default_to "Ocsigen"
      in
      let template =
        match page with
        | Document.Project {project; version; _} ->
          Document.Project {page = Document.Template; project; version}
        | Document.Site _ -> Document.Site "template"
        | Document.Deadlink _ -> assert false
      in
      let raw = Document.read template in
      Lwt_main.run (parse ~page ~title my_add (Lwt.return content) raw), title
    in
    let out_fn = Document.to_output page in
    create_tree (Filename.dirname out_fn);
    let ch = open_out (if dry then "/dev/null" else out_fn) in
    render ch ~title ~page content;
    close_out ch
  in
  let process add_link = function
    | Document.Project {page = Document.Static _; _} as f -> copy f
    | Document.Deadlink _ as d ->
      raise (Sys_error (Document.to_string true false d))
    | d -> compile add_link d
  in
  let dead = ref 0 in
  let processed = ref 0 in
  let docs = List.map Document.parse_filename files in
  let module C = Crawler.Make(Document) in
  let set = C.bfs ?max_depth docs ~f:(fun ~already ~add ?pred cur ->
    try
      if not already then (
        process add cur;
        incr processed
      )
    with Sys_error e ->
      let from =
        match pred with
        | None -> ""
        | Some d ->
          (Document.to_source d |> How_lib.Option.force) ^ " -> "
      in
      prerr_endline @@ from ^ e;
      incr dead;
    )
  in
  (* VVV Removing this and commiting style.css and client.js separately.
     It was not working anyway. *)
  (* if not dry then ( *)
  (*   write_script (Filename.concat output script_name); *)
  (*   write_style (Filename.concat output style_name) *)
  (* ); *)
  let n = C.Set.cardinal set in
  Printf.printf
    "%d targets\n%d dead links\n%d files processed\n"
    n !dead !processed


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

let cmd =
  let doc = "compile Ocsigen's Wikicreole dialect to HTML" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) compiles documentation files to HTML. The <<title>> command
        in the template injects the first <h1>'s content, and <<content>>
        inserts the page's content.";
    `S Manpage.s_examples;
    `P "cd ~/dev/ocsigen.org-data && html_of_wiki index.wiki";
    `S Manpage.s_bugs;
    `P "Currently, it only works from ocsigen.org-data's root. As a workaround,
        just update a fake index file with links to the specific pages you want
        to generate.";
    `P "Report bugs to <guillaume.huysmans@student.umons.ac.be>.";
  ] in
  Term.(const explore $ depth $ output $ dry $ files),
  Term.info "html_of_wiki" ~version:"v1.0.0" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)
