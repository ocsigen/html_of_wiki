open Utils.Operators

let replace_content_tag tmpl content =
  let regexp = Str.regexp "<<content>>" in
  try
    ignore @@ Str.search_forward regexp tmpl 0;
    Some (Str.replace_first regexp content tmpl)
  with Not_found ->
    None

let main template =
  let tmpl = Utils.readfile template in
  let wiki = Utils.read_in_channel stdin in
  match replace_content_tag tmpl wiki with
  | Some replacement -> print_string replacement
  | None -> Printf.fprintf stderr "no <<content>> tag found in template\n"; exit 1


let tmpl_cmd =
  let doc = "The template to put the wiki into." in
  Cmdliner.Arg.(required & pos ~rev:true 0 (some file) None & info []
                  ~docv:"TMPL" ~doc)

let info_cmd = Cmdliner.(
    let doc = "Inlines a wikicreole file into another one with a <<content>> tag." in
    let man = [
      `S Manpage.s_description;
      `P "$(tname) reads wikicreole content from stdin and inserts it inside the
given template file $(b,TMPL) in place of the first <<content>> tag found and
outputs the result on stdout.";
      `P "The $(b,TMPL) file is never modified.";

      `S Manpage.s_bugs;
      `P "Escaping <<content>> tags has no effect, the tag is still recognized."
    ]
    in
    Term.info "wit" ~version:"v0.0.0" ~doc ~exits:Term.default_exits ~man)

let () =
  let wit_cmd = Cmdliner.Term.(const main $ tmpl_cmd) in
  Cmdliner.Term.(exit @@ eval (wit_cmd, info_cmd))
